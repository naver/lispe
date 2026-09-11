; =============================================================================
; Loading a sharded MLX Safetensors model (Qwen3.5 / Qwen3.8 27B hybrid)
; =============================================================================
; This script loads a Qwen3_5ForConditionalGeneration model in safetensors
; format with multiple files (shards) and combines all weights into a
; unified structure.
;
; HYBRID architecture (unlike GPT-OSS which is MoE):
; - 64 layers, of which 48 are GatedDeltaNet ("linear attention", Mamba-like)
;   and 16 are classic full attention (one every 4: (layer_idx+1) % 4 == 0)
; - Dense MLP with SwiGLU (no experts, no router)
; - Attention with output gate (q_proj emits 2x head_dim: values + gate)
; - Q/K RMSNorm per head, partial RoPE (25% of head_dim)
; - 4-bit quantization, group_size 64
; Compatible with MLX models from lmstudio-community
;
; Reference Python implementation: mlx_lm/models/qwen3_5.py + qwen3_next.py
;
; Known limitation (shared with gpt_oss.lisp, not specific to this model):
; tiktoken_create only receives vocab + added_tokens + pattern, without the BPE
; "merges" table and without HuggingFace's ByteLevel unmapping. Vocabulary keys
; stay in their byte-level form, so non-ASCII characters can come out
; double-encoded. Fixing it belongs in lispetiktoken, not here.
; =============================================================================

(use 'lispe_mlx)
(use 'lispe_tiktoken)  ; For the BPE tokenizer (tiktoken)

; =============================================================================
; Model configuration
; =============================================================================

(setq MODEL_PATH "/Users/clauderoux/.lmstudio/models/lmstudio-community/Qwen3.8-27B-MLX-4bit")

; Reasoning-effort instructions, copied verbatim from chat_template.jinja.
; The template injects one of these as the system message when thinking is on
; ("xhigh" is its default); "medium" adds no instructions at all.
(setq REASONING_XHIGH
    "Reasoning effort is set to xhigh. Please think carefully through the task, validate key assumptions, consider plausible alternatives, and prioritize correctness, consistency, and clarity in the final answer.")
(setq REASONING_LOW
    "Reasoning effort is set to low. Keep your thinking brief and focused, moving directly to the conclusion without unnecessary elaboration.")

; Quantization parameters (from config.json "quantization")
(setq QUANT_GROUP_SIZE 64)
(setq QUANT_BITS 4)

; Use the fused Metal kernel for the GatedDeltaNet recurrence when the MLX
; module provides it. The kernel runs the whole time loop on the GPU with the
; state in registers; the LispE fallback is the same math, one step at a time.
; Set to nil to force the fallback (useful when comparing the two).
(setq USE_GATED_DELTA_KERNEL (in (atoms) 'mlx_gated_delta))

; Fused GatedDeltaNet layer. The LispE version issues ~47 mlx_ calls per layer
; and, over 48 layers, that elementwise glue costs more than the matmuls: each
; op round-trips a tensor through memory. The C++ primitive keeps every
; intermediate inside one MLX graph. Set to nil to force the LispE path.
(setq USE_LINEAR_ATTN_FUSED (in (atoms) 'mlx_linear_attn))

; Same idea for the dense MLP and the full-attention layer: the elementwise
; steps run on large intermediates (17408-wide for the MLP), so keeping them
; inside one MLX graph avoids a round-trip through memory per layer.
(setq USE_MLP_FUSED (in (atoms) 'mlx_swiglu_mlp))
(setq USE_ATTN_FUSED (in (atoms) 'mlx_attn_step))

; Native KV cache for the full-attention layers: preallocated in blocks rather
; than concatenated at every token. Only usable through the fused attention
; step, which knows how to append into it.
(setq USE_NATIVE_KV_CACHE (and USE_ATTN_FUSED (in (atoms) 'mlx_kv_native_cache_create)))
(setq KV_CACHE_MAX_LEN 8192)

; Lists the safetensors files of a model directory.
; The names come straight from model.safetensors.index.json rather than being
; rebuilt from a "model-0000i-of-0000N" pattern, which breaks as soon as a
; repository deviates from it. Falls back to the single-file convention used by
; HuggingFace for unsharded models.
(defun list_safetensors_files(dir_path)
    (setq idx (maybe (json_read (+ dir_path "/model.safetensors.index.json")) nil))
    (setq wmap (if idx (key@ idx "weight_map") nil))
    (ife wmap
        (block
            (setq seen (dictionary))
            (loop k (keys@ wmap)
                (set@ seen (@ wmap k) true))
            (maplist (lambda(f) (+ dir_path "/" f)) (sort '< (keys@ seen))))
        (list (+ dir_path "/model.safetensors"))))

; =============================================================================
; Function to load and merge multiple safetensors files
; =============================================================================

; Loads all safetensors files and merges tensors.
; Vision tower weights are skipped: this script runs the language model only.
(defun load_sharded_safetensors(dir_path)

    (setq files (list_safetensors_files dir_path))
    (setq num_files (size files))

    (check (zerop num_files)
        (println "ERROR: No safetensors file found!")
        (return nil))

    (println "Found " num_files " safetensors file(s)")

    ; List to store all tensors (O(1) index access)
    (setq all_tensors (list))
    ; Dictionary to map name -> index (used only once at load)
    (setq tensor_index (dictionary))
    (setq all_metadata (dictionary))
    (setq total_tensors 0)
    (setq skipped_vision 0)

    ; Load each file
    (setq file_idx 0)
    (loop filepath files
        (+= file_idx 1)
        (println "")
        (println "[" file_idx "/" num_files "] Loading: " filepath)

        ; Load the safetensors file
        (setq result (mlx_load_safetensors filepath))
        (setq tensors (@ result 0))
        (setq metadata (@ result 1))

        ; Count and merge tensors
        (setq tensor_names (keys@ tensors))
        (setq num_tensors (size tensor_names))
        (println "  → " num_tensors " tensors loaded")

        ; Add tensors to list and create index.
        ; The vision tower is not used by the text-only pipeline.
        (loop name tensor_names
            (ncheck (rgx_match (rgx "^vision_tower") name)
                (block
                    (set@ tensor_index name total_tensors)
                    (push all_tensors (@ tensors name))
                    (+= total_tensors 1))
                (+= skipped_vision 1)))

        ; Merge metadata
        (loop key (keys@ metadata)
            (set@ all_metadata key (@ metadata key)))

        (mlx_synchronize)
    )

    (println "")
    (println (fill "=" 60))
    (println "Loading complete!")
    (println "  Total tensors: " total_tensors)
    (println "  Vision tensors skipped: " skipped_vision)
    (println (fill "=" 60))

    (list all_tensors tensor_index all_metadata)
)

; =============================================================================
; Function to load model configuration
; =============================================================================

; Loads the model's config.json file.
; For Qwen3.5 the language-model parameters live under "text_config".
(defun load_model_config(dir_path)
    (setq config_path (+ dir_path "/config.json"))
    (setq raw (maybe
        (json_read config_path)
        (block
            (println "WARNING: config.json not found")
            (dictionary))))
    ; Flatten text_config into the top level so lookups stay uniform
    (setq txt (key@ raw "text_config"))
    (check txt
        (loop k (keys@ txt)
            (set@ raw k (@ txt k))))
    raw)

; =============================================================================
; Function to display memory statistics
; =============================================================================

(defun print_memory_stats()
    (setq stats (maybe (mlx_get_memory_info) nil))
    (check stats
        (println "")
        (println "Memory:")
        (loop k (keys@ stats)
            (println "  " k ": " (@ stats k)))))

; =============================================================================
; Tensor extraction helpers
; =============================================================================

; Helper function to get a tensor by name (uses index)
; Note: key@ is used because @ on a missing key throws an error
; and key@ returns nil if the key doesn't exist, otherwise the value
(defun get_tensor(weights_list tensor_index name)
    (setq idx (key@ tensor_index name))
    (if (nullp idx)
        nil
        (@ weights_list idx)))

; Returns (weight scales biases) for a quantized linear projection.
; scales is nil when the tensor is not quantized.
(defun get_quant_triplet(weights_list tensor_index base)
    (list
        (get_tensor weights_list tensor_index (+ base ".weight"))
        (get_tensor weights_list tensor_index (+ base ".scales"))
        (get_tensor weights_list tensor_index (+ base ".biases"))))

; =============================================================================
; Layer tensor index constants
; =============================================================================
; Two layer kinds share the same slot table. Slots that do not apply to a
; given kind hold nil:
;   - full attention layers use L_Q_* .. L_K_NORM
;   - linear (GatedDeltaNet) layers use L_IN_QKV_* .. L_DT_BIAS
; Both kinds use L_INPUT_NORM, L_POST_ATTN_NORM and the L_GATE/UP/DOWN MLP.

; Common
(setq L_IS_LINEAR 0)          ; true when the layer is a GatedDeltaNet layer
(setq L_INPUT_NORM 1)
(setq L_POST_ATTN_NORM 2)

; --- Full attention slots ---
(setq L_Q_WEIGHT 3)
(setq L_Q_SCALES 4)
(setq L_Q_QBIASES 5)
(setq L_K_WEIGHT 6)
(setq L_K_SCALES 7)
(setq L_K_QBIASES 8)
(setq L_V_WEIGHT 9)
(setq L_V_SCALES 10)
(setq L_V_QBIASES 11)
(setq L_O_WEIGHT 12)
(setq L_O_SCALES 13)
(setq L_O_QBIASES 14)
(setq L_Q_NORM 15)
(setq L_K_NORM 16)

; --- GatedDeltaNet (linear attention) slots ---
(setq L_IN_QKV_WEIGHT 17)
(setq L_IN_QKV_SCALES 18)
(setq L_IN_QKV_QBIASES 19)
(setq L_IN_Z_WEIGHT 20)
(setq L_IN_Z_SCALES 21)
(setq L_IN_Z_QBIASES 22)
(setq L_IN_B_WEIGHT 23)
(setq L_IN_B_SCALES 24)
(setq L_IN_B_QBIASES 25)
(setq L_IN_A_WEIGHT 26)
(setq L_IN_A_SCALES 27)
(setq L_IN_A_QBIASES 28)
(setq L_CONV_WEIGHT 29)
(setq L_LA_NORM 30)           ; RMSNormGated weight, shape [head_v_dim]
(setq L_LA_OUT_WEIGHT 31)
(setq L_LA_OUT_SCALES 32)
(setq L_LA_OUT_QBIASES 33)
(setq L_A_LOG 34)
(setq L_DT_BIAS 35)

; --- Dense MLP slots (shared by both layer kinds) ---
(setq L_GATE_WEIGHT 36)
(setq L_GATE_SCALES 37)
(setq L_GATE_QBIASES 38)
(setq L_UP_WEIGHT 39)
(setq L_UP_SCALES 40)
(setq L_UP_QBIASES 41)
(setq L_DOWN_WEIGHT 42)
(setq L_DOWN_SCALES 43)
(setq L_DOWN_QBIASES 44)

(setq L_SLOT_COUNT 45)

; Global tensor indices
(setq G_EMBED_WEIGHT 0)
(setq G_EMBED_SCALES 1)
(setq G_EMBED_BIASES 2)
(setq G_FINAL_NORM 3)
(setq G_LM_HEAD_WEIGHT 4)
(setq G_LM_HEAD_SCALES 5)
(setq G_LM_HEAD_BIASES 6)

; Transpose patterns
(setq TRANSPOSE_0213 (integers 0 2 1 3))

; KV cache slots (LispE-side cache: keys, values, offset)
(setq KV_KEYS 0)
(setq KV_VALUES 1)
(setq KV_OFFSET 2)

; GatedDeltaNet recurrent cache slots (conv state, recurrent state)
(setq LA_CONV_STATE 0)
(setq LA_REC_STATE 1)

; Layer type labels used in config "layer_types"
(setq LAYER_LINEAR "linear_attention")
(setq LAYER_FULL "full_attention")

; Attention parameter slots (numbers vector)
(setq AP_N_HEADS 0)
(setq AP_N_KV_HEADS 1)
(setq AP_HEAD_DIM 2)
(setq AP_SCALE 3)
(setq AP_EPS 4)
(setq AP_HIDDEN_OUT 5)
(setq AP_ROPE_DIMS 6)
(setq AP_ROPE_THETA 7)
(setq AP_NUM_V_HEADS 8)
(setq AP_NUM_K_HEADS 9)
(setq AP_HEAD_K_DIM 10)
(setq AP_HEAD_V_DIM 11)
(setq AP_CONV_KERNEL 12)
(setq AP_KEY_DIM 13)
(setq AP_VALUE_DIM 14)
(setq AP_CONV_DIM 15)
(setq AP_HIDDEN_SIZE 16)

; =============================================================================
; Per-layer tensor pre-extraction
; =============================================================================

; Pre-extracts the tensors of one layer into a flat, index-addressable list.
; is_linear decides which family of slots gets filled; the rest stay nil.
(defun extract_layer_tensors(weights_list tensor_index layer_idx is_linear)
    (setq prefix (+ "language_model.model.layers." (string layer_idx) "."))
    (setq attn (+ prefix "self_attn."))
    (setq la (+ prefix "linear_attn."))
    (setq mlp (+ prefix "mlp."))

    ; Start with an all-nil slot table, then fill what applies
    (setq lt (list))
    (loopcount L_SLOT_COUNT into i
        (push lt nil))

    (set@ lt L_IS_LINEAR is_linear)
    (set@ lt L_INPUT_NORM (get_tensor weights_list tensor_index (+ prefix "input_layernorm.weight")))
    (set@ lt L_POST_ATTN_NORM (get_tensor weights_list tensor_index (+ prefix "post_attention_layernorm.weight")))

    (ncheck is_linear
        ; ---- Full attention layer ----
        (block
            (setq q (get_quant_triplet weights_list tensor_index (+ attn "q_proj")))
            (setq k (get_quant_triplet weights_list tensor_index (+ attn "k_proj")))
            (setq v (get_quant_triplet weights_list tensor_index (+ attn "v_proj")))
            (setq o (get_quant_triplet weights_list tensor_index (+ attn "o_proj")))
            (set@ lt L_Q_WEIGHT (@ q 0)) (set@ lt L_Q_SCALES (@ q 1)) (set@ lt L_Q_QBIASES (@ q 2))
            (set@ lt L_K_WEIGHT (@ k 0)) (set@ lt L_K_SCALES (@ k 1)) (set@ lt L_K_QBIASES (@ k 2))
            (set@ lt L_V_WEIGHT (@ v 0)) (set@ lt L_V_SCALES (@ v 1)) (set@ lt L_V_QBIASES (@ v 2))
            (set@ lt L_O_WEIGHT (@ o 0)) (set@ lt L_O_SCALES (@ o 1)) (set@ lt L_O_QBIASES (@ o 2))
            (set@ lt L_Q_NORM (get_tensor weights_list tensor_index (+ attn "q_norm.weight")))
            (set@ lt L_K_NORM (get_tensor weights_list tensor_index (+ attn "k_norm.weight"))))

        ; ---- GatedDeltaNet layer ----
        (block
            (setq qkv (get_quant_triplet weights_list tensor_index (+ la "in_proj_qkv")))
            (setq z (get_quant_triplet weights_list tensor_index (+ la "in_proj_z")))
            (setq b (get_quant_triplet weights_list tensor_index (+ la "in_proj_b")))
            (setq a (get_quant_triplet weights_list tensor_index (+ la "in_proj_a")))
            (setq outp (get_quant_triplet weights_list tensor_index (+ la "out_proj")))
            (set@ lt L_IN_QKV_WEIGHT (@ qkv 0)) (set@ lt L_IN_QKV_SCALES (@ qkv 1)) (set@ lt L_IN_QKV_QBIASES (@ qkv 2))
            (set@ lt L_IN_Z_WEIGHT (@ z 0)) (set@ lt L_IN_Z_SCALES (@ z 1)) (set@ lt L_IN_Z_QBIASES (@ z 2))
            (set@ lt L_IN_B_WEIGHT (@ b 0)) (set@ lt L_IN_B_SCALES (@ b 1)) (set@ lt L_IN_B_QBIASES (@ b 2))
            (set@ lt L_IN_A_WEIGHT (@ a 0)) (set@ lt L_IN_A_SCALES (@ a 1)) (set@ lt L_IN_A_QBIASES (@ a 2))
            (set@ lt L_LA_OUT_WEIGHT (@ outp 0)) (set@ lt L_LA_OUT_SCALES (@ outp 1)) (set@ lt L_LA_OUT_QBIASES (@ outp 2))
            (set@ lt L_CONV_WEIGHT (get_tensor weights_list tensor_index (+ la "conv1d.weight")))
            (set@ lt L_LA_NORM (get_tensor weights_list tensor_index (+ la "norm.weight")))
            (set@ lt L_A_LOG (get_tensor weights_list tensor_index (+ la "A_log")))
            (set@ lt L_DT_BIAS (get_tensor weights_list tensor_index (+ la "dt_bias")))))

    ; ---- Dense MLP (both kinds) ----
    (setq g (get_quant_triplet weights_list tensor_index (+ mlp "gate_proj")))
    (setq u (get_quant_triplet weights_list tensor_index (+ mlp "up_proj")))
    (setq dn (get_quant_triplet weights_list tensor_index (+ mlp "down_proj")))
    (set@ lt L_GATE_WEIGHT (@ g 0)) (set@ lt L_GATE_SCALES (@ g 1)) (set@ lt L_GATE_QBIASES (@ g 2))
    (set@ lt L_UP_WEIGHT (@ u 0)) (set@ lt L_UP_SCALES (@ u 1)) (set@ lt L_UP_QBIASES (@ u 2))
    (set@ lt L_DOWN_WEIGHT (@ dn 0)) (set@ lt L_DOWN_SCALES (@ dn 1)) (set@ lt L_DOWN_QBIASES (@ dn 2))

    lt)

; Builds the per-layer tensor tables for the whole model
(defun build_layer_tensors(weights_list tensor_index num_layers layer_types)
    (setq layers (list))
    (loopcount num_layers into i
        (setq is_linear (eq (@ layer_types i) LAYER_LINEAR))
        (push layers (extract_layer_tensors weights_list tensor_index i is_linear)))
    layers)

; Extracts the global (non-layer) tensors
(defun extract_global_tensors(weights_list tensor_index)
    (setq emb (get_quant_triplet weights_list tensor_index "language_model.model.embed_tokens"))
    (setq head (get_quant_triplet weights_list tensor_index "language_model.lm_head"))
    (list
        (@ emb 0) (@ emb 1) (@ emb 2)
        (get_tensor weights_list tensor_index "language_model.model.norm.weight")
        (@ head 0) (@ head 1) (@ head 2)))

; =============================================================================
; MLXModel class - Qwen3.5 hybrid
; =============================================================================
; Represents a loaded Qwen3.5 model with its weights, config and tokenizer.
; Attributes:
;   weights: list of tensors (O(1) index access)
;   tensor_index: dictionary name -> index
;   config: flattened model configuration (text_config merged in)
;   metadata: safetensors metadata
;   tokenizer: tiktoken tokenizer
;   cached_embeddings: pre-dequantized embeddings
;   layer_tensors: pre-extracted tensors by layer
;   global_tensors: global tensors (embed, lm_head, norm)
;   attn_params: precomputed attention / linear-attention parameters
;   kv_caches: per-layer caches. Full-attention layers hold (keys values offset),
;              linear layers hold (conv_state rec_state).
;   layer_types: list of layer types (linear_attention / full_attention)
(class@ MLXModel (weights tensor_index config metadata tokenizer cached_embeddings
                  layer_tensors global_tensors attn_params kv_caches layer_types
                  eos_token_ids)

    ; =========================================================================
    ; Basic accessors
    ; =========================================================================

    (defun get_weight(name)
        (setq idx (key@ tensor_index name))
        (check (nullp idx)
            (println "WARNING: Tensor not found: " name)
            (return nil))
        (@ weights idx))

    (defun has_weight(name)
        (neq (key@ tensor_index name) nil))

    (defun list_weights((pattern ""))
        (setq all_names (keys@ tensor_index))
        (ncheck (eq pattern "")
            (setq rx (rgx pattern))
            (filter (λ(n) (rgx_match rx n)) all_names)
            all_names))

    (defun num_weights()
        (size weights))

    (defun get_config(key (default nil))
        (select (key@ config key) default))

    (defun info()
        (setq n_lin 0)
        (loop t layer_types
            (check (eq t LAYER_LINEAR) (+= n_lin 1)))
        (println "MLX Qwen3.5 hybrid model:")
        (println "  Tensors: " (num_weights))
        (println "  Type: " (get_config "model_type" "unknown"))
        (println "  Hidden size: " (get_config "hidden_size" 0))
        (println "  Layers: " (get_config "num_hidden_layers" 0)
                 " (" n_lin " linear / " (- (size layer_types) n_lin) " full attention)")
        (println "  Heads: " (get_config "num_attention_heads" 0)
                 " (kv: " (get_config "num_key_value_heads" 0) ", head_dim: " (get_config "head_dim" 0) ")")
        (println "  Quantization: " QUANT_BITS " bits, group " QUANT_GROUP_SIZE)
        (check tokenizer
            (println "  Vocabulary: " (tiktoken_vocab_size tokenizer) " tokens")))

    ; =========================================================================
    ; Tokenizer (tiktoken)
    ; =========================================================================

    (defun encode(text)
        (check (not tokenizer)
            (println "ERROR: Tokenizer not loaded")
            (return nil))
        (tiktoken_encode tokenizer text))

    (defun decode(tokens)
        (check (not tokenizer)
            (println "ERROR: Tokenizer not loaded")
            (return nil))
        (tiktoken_decode tokenizer tokens))

    (defun vocab_size()
        (check (not tokenizer)
            (return 0))
        (tiktoken_vocab_size tokenizer))

    ; =========================================================================
    ; Internal utility functions
    ; =========================================================================

    ; Linear projection with quantized or plain weights.
    ; Qwen3.5 uses 4-bit affine quantization with group_size 64 and no
    ; linear bias anywhere (attention_bias is false).
    (defun _linear(x weight scales quant_biases)
        (if scales
            (mlx_quantized_matmul x weight scales quant_biases true QUANT_GROUP_SIZE QUANT_BITS)
            (mlx_matmul x . mlx_transpose weight)))

    ; Standard RMSNorm
    (defun _rms_norm(x weight eps)
        (mlx_rms_norm x weight eps false))

    ; softplus(x) = log(1 + exp(x)), computed stably as max(x,0) + log1p(exp(-|x|))
    ; so that large positive x cannot overflow the exponential.
    (defun _softplus(x)
        (setq zero (mlx_array 0.0))
        (mlx_add
            (mlx_maximum x zero)
            (mlx_log1p (mlx_exp (mlx_negative (mlx_abs x))))))

    ; SwiGLU used by the dense MLP: down(silu(gate(x)) * up(x))
    ; Tensor vector expected by mlx_swiglu_mlp
    (defun _mlp_tensors(lt)
        (list (@ lt L_GATE_WEIGHT) (@ lt L_GATE_SCALES) (@ lt L_GATE_QBIASES)
              (@ lt L_UP_WEIGHT) (@ lt L_UP_SCALES) (@ lt L_UP_QBIASES)
              (@ lt L_DOWN_WEIGHT) (@ lt L_DOWN_SCALES) (@ lt L_DOWN_QBIASES)))

    (defun _mlp(x lt)
        (check USE_MLP_FUSED
            (return (mlx_swiglu_mlp x (_mlp_tensors lt)
                        (integers QUANT_GROUP_SIZE QUANT_BITS))))
        (setq gate (_linear x (@ lt L_GATE_WEIGHT) (@ lt L_GATE_SCALES) (@ lt L_GATE_QBIASES)))
        (setq up (_linear x (@ lt L_UP_WEIGHT) (@ lt L_UP_SCALES) (@ lt L_UP_QBIASES)))
        (setq h (mlx_multiply (mlx_silu gate) up))
        (_linear h (@ lt L_DOWN_WEIGHT) (@ lt L_DOWN_SCALES) (@ lt L_DOWN_QBIASES)))

    ; =========================================================================
    ; KV cache (full attention layers)
    ; =========================================================================

    ; Creates the per-layer cache list. Full-attention layers get a KV cache,
    ; GatedDeltaNet layers get a (conv_state, recurrent_state) pair.
    ; Full-attention layers get a native KV cache when the fused attention
    ; step can use one: it grows by preallocated blocks and writes with
    ; slice_update, instead of concatenating (which recopies the whole history
    ; at every token, a cost that grows with the sequence).
    (defun _make_all_caches()
        (setq num_layers (@ config "num_hidden_layers"))
        (setq n_kv_heads (integer (@ attn_params AP_N_KV_HEADS)))
        (setq head_dim (integer (@ attn_params AP_HEAD_DIM)))
        (setq caches (list))
        (loopcount num_layers into i
            (ife (eq (@ layer_types i) LAYER_LINEAR)
                (push caches (list nil nil))
                (ife USE_NATIVE_KV_CACHE
                    (push caches (list
                        (mlx_kv_native_cache_create 1 n_kv_heads head_dim KV_CACHE_MAX_LEN nil)
                        nil 0))
                    (push caches (list nil nil 0)))))
        caches)

    ; Appends keys/values to a layer's KV cache and returns the full history
    (defun _update_kv_cache(cache keys values)
        (setq cached_keys (@ cache KV_KEYS))
        (ife cached_keys
            (block
                (setq new_keys (mlx_concatenate (list cached_keys keys) 2))
                (setq new_values (mlx_concatenate (list (@ cache KV_VALUES) values) 2)))
            (block
                (setq new_keys keys)
                (setq new_values values)))
        (set@ cache KV_KEYS new_keys)
        (set@ cache KV_VALUES new_values)
        (set@ cache KV_OFFSET (@ (mlx_shape new_keys) 2))
        (list new_keys new_values))

    ; Native caches hold GPU memory, so free them before dropping the list
    (defun reset_cache()
        (check kv_caches
            (loop c kv_caches
                (check (numberp (@ c KV_KEYS))
                    (mlx_kv_cache_free (@ c KV_KEYS)))))
        (setqi kv_caches nil))

    ; =========================================================================
    ; Full attention (1 layer out of 4)
    ; =========================================================================
    ; Differences from a classic transformer block:
    ;  - q_proj emits 2 * head_dim per head: the first half feeds attention,
    ;    the second half is a sigmoid output gate applied after o_proj's input
    ;  - q_norm / k_norm are RMSNorms applied per head, before RoPE
    ;  - RoPE is partial: it rotates only rope_dims (= 0.25 * head_dim) values
    ; Tensor vector expected by mlx_attn_step
    (defun _attn_tensors(lt)
        (list (@ lt L_Q_WEIGHT) (@ lt L_Q_SCALES) (@ lt L_Q_QBIASES)
              (@ lt L_K_WEIGHT) (@ lt L_K_SCALES) (@ lt L_K_QBIASES)
              (@ lt L_V_WEIGHT) (@ lt L_V_SCALES) (@ lt L_V_QBIASES)
              (@ lt L_O_WEIGHT) (@ lt L_O_SCALES) (@ lt L_O_QBIASES)
              (@ lt L_Q_NORM) (@ lt L_K_NORM)))

    (defun _attention(x lt offset kv_cache)
        (setq n_heads (integer (@ attn_params AP_N_HEADS)))
        (setq n_kv_heads (integer (@ attn_params AP_N_KV_HEADS)))
        (setq head_dim (integer (@ attn_params AP_HEAD_DIM)))
        (setq scale (@ attn_params AP_SCALE))
        (setq eps (@ attn_params AP_EPS))
        (setq hidden_out (integer (@ attn_params AP_HIDDEN_OUT)))
        (setq rope_dims (integer (@ attn_params AP_ROPE_DIMS)))
        (setq rope_theta (@ attn_params AP_ROPE_THETA))

        (setq shape (mlx_shape x))
        (setq B (@ shape 0))
        (setq L (@ shape 1))

        ; Fused path: one primitive, KV cache passed in and returned
        (check USE_ATTN_FUSED
            (setq at_params (numbers n_heads n_kv_heads head_dim scale eps
                                     hidden_out rope_dims rope_theta
                                     QUANT_GROUP_SIZE QUANT_BITS))
            (setq kv_slot (if kv_cache (@ kv_cache KV_KEYS) nil))
            ; With a native cache the id is a number and the primitive returns
            ; the output alone, having appended in place. Otherwise it returns
            ; (out keys values) and the arrays are carried by the caller.
            (ife (numberp kv_slot)
                (return (mlx_attn_step x (_attn_tensors lt) at_params offset kv_slot nil))
                (block
                    (setq r (mlx_attn_step x (_attn_tensors lt) at_params offset
                                kv_slot
                                (if kv_cache (@ kv_cache KV_VALUES) nil)))
                    (check kv_cache
                        (set@ kv_cache KV_KEYS (@ r 1))
                        (set@ kv_cache KV_VALUES (@ r 2))
                        (set@ kv_cache KV_OFFSET (@ (mlx_shape (@ r 1)) 2)))
                    (return (@ r 0)))))

        ; q_proj -> [B, L, n_heads * 2 * head_dim], split into values and gate
        (setq q_out (_linear x (@ lt L_Q_WEIGHT) (@ lt L_Q_SCALES) (@ lt L_Q_QBIASES)))
        (setq q_out (mlx_reshape q_out (integers B L n_heads (* 2 head_dim))))
        (setq q_parts (mlx_split q_out 2 -1))
        (setq queries (@ q_parts 0))                       ; [B, L, n_heads, head_dim]
        (setq gate (mlx_reshape (@ q_parts 1) (integers B L hidden_out)))

        (setq keys (_linear x (@ lt L_K_WEIGHT) (@ lt L_K_SCALES) (@ lt L_K_QBIASES)))
        (setq values (_linear x (@ lt L_V_WEIGHT) (@ lt L_V_SCALES) (@ lt L_V_QBIASES)))
        (setq keys (mlx_reshape keys (integers B L n_kv_heads head_dim)))
        (setq values (mlx_reshape values (integers B L n_kv_heads head_dim)))

        ; Per-head RMSNorm on queries and keys, then [B, H, L, D]
        (setq queries (mlx_transpose (_rms_norm queries (@ lt L_Q_NORM) eps) TRANSPOSE_0213))
        (setq keys (mlx_transpose (_rms_norm keys (@ lt L_K_NORM) eps) TRANSPOSE_0213))
        (setq values (mlx_transpose values TRANSPOSE_0213))

        ; Partial RoPE: only the first rope_dims of each head are rotated
        (setq queries (mlx_rope queries rope_dims false rope_theta 1.0 offset nil))
        (setq keys (mlx_rope keys rope_dims false rope_theta 1.0 offset nil))

        ; Causal mask only matters while prefilling more than one position
        (if (== L 1)
            (setq mask_mode nil)
            (setq mask_mode "causal"))

        (ife kv_cache
            (block
                (setq kv_pair (_update_kv_cache kv_cache keys values))
                (setq attention_output (mlx_scaled_dot_product_attention
                    queries (@ kv_pair 0) (@ kv_pair 1) scale mask_mode () nil)))
            (setq attention_output (mlx_scaled_dot_product_attention
                queries keys values scale mask_mode () nil)))

        ; [B, H, L, D] -> [B, L, H*D], then the sigmoid output gate
        (setq attention_output (mlx_reshape
            (mlx_transpose attention_output TRANSPOSE_0213)
            (integers B L hidden_out)))
        (setq attention_output (mlx_multiply attention_output (mlx_sigmoid gate)))

        (_linear attention_output (@ lt L_O_WEIGHT) (@ lt L_O_SCALES) (@ lt L_O_QBIASES)))

    ; =========================================================================
    ; GatedDeltaNet - "linear attention" (3 layers out of 4)
    ; =========================================================================
    ; Linear-attention layer with a gated delta rule. Instead of a growing KV
    ; cache it keeps a fixed-size recurrent state per value head:
    ;
    ;   state <- state * g_t                       (exponential decay)
    ;   delta <- (v_t - state . k_t) * beta_t      (prediction error)
    ;   state <- state + k_t (x) delta             (rank-1 correction)
    ;   y_t   <- state . q_t
    ;
    ; with g_t = exp(-exp(A_log) * softplus(a_t + dt_bias)) and beta_t = sigmoid(b_t).
    ; The recurrence is inherently sequential, so prefill loops over positions;
    ; state size is [B, num_v_heads, head_v_dim, head_k_dim], independent of L.

    ; One recurrence step.
    ;   q, k: [B, Hv, Dk]   v: [B, Hv, Dv]
    ;   g, beta: [B, Hv]    state: [B, Hv, Dv, Dk]
    ; Returns (y state) with y: [B, Hv, Dv]
    (defun _delta_step(q k v g beta state)
        ; Decay the state: g broadcast over (Dv, Dk)
        (setq decay (mlx_expand_dims (mlx_expand_dims g -1) -1))     ; [B, Hv, 1, 1]
        (setq state (mlx_multiply state decay))

        ; kv_mem = state . k  -> [B, Hv, Dv]
        (setq k_row (mlx_expand_dims k -2))                          ; [B, Hv, 1, Dk]
        (setq kv_mem (mlx_sum (mlx_multiply state k_row) -1 false))

        ; delta = (v - kv_mem) * beta -> [B, Hv, Dv]
        (setq delta (mlx_multiply (mlx_subtract v kv_mem) (mlx_expand_dims beta -1)))

        ; state += k (x) delta -> outer product on (Dv, Dk)
        (setq state (mlx_add state (mlx_multiply k_row (mlx_expand_dims delta -1))))

        ; y = state . q -> [B, Hv, Dv]
        (setq q_row (mlx_expand_dims q -2))                          ; [B, Hv, 1, Dk]
        (setq y (mlx_sum (mlx_multiply state q_row) -1 false))

        (list y state))

    ; Causal depthwise convolution over the (q,k,v) stream.
    ; conv_state holds the previous kernel_size-1 positions so that decoding
    ; one token at a time sees the same left context as prefill.
    ; qkv: [B, L, conv_dim]  weight: [conv_dim, K, 1]
    ; Returns (conv_out new_conv_state)
    (defun _causal_conv(qkv conv_weight conv_state conv_dim kernel_size)
        (setq shape (mlx_shape qkv))
        (setq B (@ shape 0))
        (setq L (@ shape 1))
        (setq n_keep (- kernel_size 1))

        ; Left-pad with the cached tail (zeros on the first call)
        (setq prev (if conv_state
            conv_state
            (mlx_zeros (integers B n_keep conv_dim) "float32")))
        (setq conv_input (mlx_concatenate (list (mlx_astype prev (mlx_dtype qkv)) qkv) 1))

        ; Keep the last kernel_size-1 positions for the next call
        (setq total (+ L n_keep))
        (setq new_state (mlx_slice conv_input
            (integers 0 (- total n_keep) 0)
            (integers B total conv_dim)))

        ; Depthwise conv: groups = conv_dim, no padding (already left-padded)
        (setq conv_out (mlx_silu (mlx_conv1d conv_input conv_weight 1 0 1 conv_dim)))
        (list conv_out new_state))

    ; Full GatedDeltaNet forward for a layer.
    ; x: [B, L, hidden_size]; cache is (conv_state rec_state), mutated in place.
    ; Tensor vector expected by mlx_linear_attn, in its documented order.
    (defun _la_tensors(lt)
        (list (@ lt L_IN_QKV_WEIGHT) (@ lt L_IN_QKV_SCALES) (@ lt L_IN_QKV_QBIASES)
              (@ lt L_IN_Z_WEIGHT) (@ lt L_IN_Z_SCALES) (@ lt L_IN_Z_QBIASES)
              (@ lt L_IN_B_WEIGHT) (@ lt L_IN_B_SCALES) (@ lt L_IN_B_QBIASES)
              (@ lt L_IN_A_WEIGHT) (@ lt L_IN_A_SCALES) (@ lt L_IN_A_QBIASES)
              (@ lt L_CONV_WEIGHT) (@ lt L_LA_NORM)
              (@ lt L_LA_OUT_WEIGHT) (@ lt L_LA_OUT_SCALES) (@ lt L_LA_OUT_QBIASES)
              (@ lt L_A_LOG) (@ lt L_DT_BIAS)))

    (defun _linear_attention(x lt cache)
        (setq num_v_heads (integer (@ attn_params AP_NUM_V_HEADS)))
        (setq num_k_heads (integer (@ attn_params AP_NUM_K_HEADS)))
        (setq head_k_dim (integer (@ attn_params AP_HEAD_K_DIM)))
        (setq head_v_dim (integer (@ attn_params AP_HEAD_V_DIM)))
        (setq conv_kernel (integer (@ attn_params AP_CONV_KERNEL)))
        (setq key_dim (integer (@ attn_params AP_KEY_DIM)))
        (setq value_dim (integer (@ attn_params AP_VALUE_DIM)))
        (setq conv_dim (integer (@ attn_params AP_CONV_DIM)))
        (setq eps (@ attn_params AP_EPS))

        (setq shape (mlx_shape x))
        (setq B (@ shape 0))
        (setq L (@ shape 1))

        ; Fused path: one primitive covers projections through output projection
        (check USE_LINEAR_ATTN_FUSED
            (setq la_params (numbers num_v_heads num_k_heads head_k_dim head_v_dim
                                     conv_kernel key_dim value_dim conv_dim eps
                                     QUANT_GROUP_SIZE QUANT_BITS))
            (setq r (mlx_linear_attn x (_la_tensors lt) la_params
                        (@ cache LA_CONV_STATE) (@ cache LA_REC_STATE)))
            (set@ cache LA_CONV_STATE (@ r 1))
            (set@ cache LA_REC_STATE (@ r 2))
            (return (@ r 0)))

        ; Input projections
        (setq qkv (_linear x (@ lt L_IN_QKV_WEIGHT) (@ lt L_IN_QKV_SCALES) (@ lt L_IN_QKV_QBIASES)))
        (setq z (_linear x (@ lt L_IN_Z_WEIGHT) (@ lt L_IN_Z_SCALES) (@ lt L_IN_Z_QBIASES)))
        (setq b (_linear x (@ lt L_IN_B_WEIGHT) (@ lt L_IN_B_SCALES) (@ lt L_IN_B_QBIASES)))
        (setq a (_linear x (@ lt L_IN_A_WEIGHT) (@ lt L_IN_A_SCALES) (@ lt L_IN_A_QBIASES)))
        (setq z (mlx_reshape z (integers B L num_v_heads head_v_dim)))

        ; Causal depthwise convolution + SiLU on the packed (q,k,v) stream
        (setq conv_pair (_causal_conv qkv (@ lt L_CONV_WEIGHT)
            (@ cache LA_CONV_STATE) conv_dim conv_kernel))
        (setq conv_out (@ conv_pair 0))
        (set@ cache LA_CONV_STATE (@ conv_pair 1))

        ; Split the stream back into q, k (key_dim each) and v (value_dim)
        (setq splits (mlx_split conv_out (integers key_dim (* 2 key_dim)) -1))
        (setq q (mlx_reshape (@ splits 0) (integers B L num_k_heads head_k_dim)))
        (setq k (mlx_reshape (@ splits 1) (integers B L num_k_heads head_k_dim)))
        (setq v (mlx_reshape (@ splits 2) (integers B L num_v_heads head_v_dim)))

        ; L2-style normalisation of q and k, folding the attention scale in.
        ; Python: q = inv_scale^2 * rms_norm(q), k = inv_scale * rms_norm(k)
        (setq inv_scale (/ 1.0 (sqrt (number head_k_dim))))
        (setq q (mlx_multiply (mlx_array (* inv_scale inv_scale)) (mlx_rms_norm q nil 1e-6 false)))
        (setq k (mlx_multiply (mlx_array inv_scale) (mlx_rms_norm k nil 1e-6 false)))

        ; Gates: beta = sigmoid(b), g = exp(-exp(A_log) * softplus(a + dt_bias))
        ; beta/g feed the recurrence, which runs in float32: convert once here
        ; rather than letting the kernel cast them on every layer.
        (setq beta (mlx_sigmoid (mlx_astype b "float32")))
        (setq a32 (mlx_astype a "float32"))
        (setq decay_rate (mlx_exp (mlx_astype (@ lt L_A_LOG) "float32")))
        (setq g (mlx_exp (mlx_negative (mlx_multiply decay_rate
            (_softplus (mlx_add a32 (mlx_astype (@ lt L_DT_BIAS) "float32")))))))

        ; Recurrent state, carried across calls (fixed size, independent of L)
        (setq state (@ cache LA_REC_STATE))
        (check (nullp state)
            (setq state (mlx_zeros (integers B num_v_heads head_v_dim head_k_dim) "float32")))

        ; The whole time loop runs inside one Metal kernel, with the recurrent
        ; state kept in registers. It also expands the grouped heads itself,
        ; so q/k are passed with their original num_k_heads.
        (ife USE_GATED_DELTA_KERNEL
            (block
                (setq r (mlx_gated_delta q k v g beta state))
                (setq out (@ r 0))
                (setq state (@ r 1)))

            ; Fallback: sequential scan in LispE (same math, much slower)
            (block
                (setq repeat_factor (/ num_v_heads num_k_heads))
                (check (> repeat_factor 1)
                    (setq q (mlx_repeat q repeat_factor -2))
                    (setq k (mlx_repeat k repeat_factor -2)))
                (setq outputs (list))
                (loopcount L into t
                    (setq t1 (+ t 1))
                    (setq q_t (mlx_squeeze (mlx_slice q (integers 0 t 0 0) (integers B t1 num_v_heads head_k_dim)) 1))
                    (setq k_t (mlx_squeeze (mlx_slice k (integers 0 t 0 0) (integers B t1 num_v_heads head_k_dim)) 1))
                    (setq v_t (mlx_squeeze (mlx_slice v (integers 0 t 0 0) (integers B t1 num_v_heads head_v_dim)) 1))
                    (setq g_t (mlx_squeeze (mlx_slice g (integers 0 t 0) (integers B t1 num_v_heads)) 1))
                    (setq beta_t (mlx_squeeze (mlx_slice beta (integers 0 t 0) (integers B t1 num_v_heads)) 1))

                    ; The recurrence runs in float32 for numerical stability
                    (setq step (_delta_step
                        (mlx_astype q_t "float32") (mlx_astype k_t "float32") (mlx_astype v_t "float32")
                        g_t (mlx_astype beta_t "float32") state))
                    (push outputs (@ step 0))
                    (setq state (@ step 1)))
                (setq out (mlx_stack outputs 1))))

        (set@ cache LA_REC_STATE state)
        (setq out (mlx_astype out (mlx_dtype x)))

        ; Gated RMSNorm: rms_norm(out, weight) * silu(z), then project back
        (setq normed (mlx_rms_norm out (@ lt L_LA_NORM) eps false))
        (setq out (mlx_multiply normed (mlx_silu z)))
        (setq out (mlx_reshape out (integers B L value_dim)))
        (_linear out (@ lt L_LA_OUT_WEIGHT) (@ lt L_LA_OUT_SCALES) (@ lt L_LA_OUT_QBIASES)))

    ; =========================================================================
    ; Decoder layer
    ; =========================================================================
    ; Same pre-norm residual shape for both layer kinds; only the token-mixing
    ; sublayer differs (GatedDeltaNet vs full attention).
    (defun _decoder_layer(x lt offset cache)
        (setq eps (@ attn_params AP_EPS))

        (setq normed_x (_rms_norm x (@ lt L_INPUT_NORM) eps))
        (if (@ lt L_IS_LINEAR)
            (setq mix_output (_linear_attention normed_x lt cache))
            (setq mix_output (_attention normed_x lt offset cache)))

        (setq h (mlx_add x mix_output))

        (setq normed_h (_rms_norm h (@ lt L_POST_ATTN_NORM) eps))
        (mlx_add h (_mlp normed_h lt)))

    ; =========================================================================
    ; Full forward pass
    ; =========================================================================

    ; Forward pass - returns logits [B, L, vocab_size]
    ; last_only slices the hidden state down to the final position before the
    ; norm and lm_head. Generation only ever needs that row, and the lm_head
    ; projects onto ~248k vocabulary entries: on a 1000-token prompt, keeping
    ; every position would allocate a logits buffer of several hundred MB and
    ; spend a full matmul on rows that are then thrown away.
    (defun forward(input_ids offset (last_only true))
        (setq num_layers (@ config "num_hidden_layers"))
        (setq eps (@ attn_params AP_EPS))

        (setq input_shape (mlx_shape input_ids))
        (setq B (@ input_shape 0))
        (setq L (@ input_shape 1))

        ; Embedding lookup on the pre-dequantized table
        (setq flat_ids (mlx_flatten input_ids))
        (setq embeddings_flat (mlx_take cached_embeddings flat_ids 0))
        (setq hidden_size (@ (mlx_shape cached_embeddings) 1))
        (setq hidden_states (mlx_reshape embeddings_flat (integers B L hidden_size)))

        (check (nullp kv_caches)
            (setqi kv_caches (_make_all_caches)))

        (loopcount num_layers into i
            (setq hidden_states (_decoder_layer
                hidden_states
                (@ layer_tensors i)
                offset
                (@ kv_caches i))))

        ; Keep only the last position before the vocabulary projection
        (check (and last_only (> L 1))
            (setq hidden_states (mlx_slice hidden_states
                (integers 0 (- L 1) 0) (integers B L hidden_size))))

        (setq hidden_states (_rms_norm hidden_states (@ global_tensors G_FINAL_NORM) eps))

        (_linear hidden_states
            (@ global_tensors G_LM_HEAD_WEIGHT)
            (@ global_tensors G_LM_HEAD_SCALES)
            (@ global_tensors G_LM_HEAD_BIASES)))

    ; Runs a forward pass and keeps only the last position's logits.
    ; forward already slices before the lm_head, so this is just a call.
    (defun _last_logits_for_step(input_tensor offset)
        (forward input_tensor offset))

    ; =========================================================================
    ; Text generation
    ; =========================================================================

    ; Formats a chat prompt, following the model's own chat_template.jinja.
    ; Two details matter and are easy to get wrong:
    ;  - when thinking is enabled the template ends with "<think>\n", which is
    ;    what actually makes the model reason. Stopping at "assistant\n" leaves
    ;    it to open the block itself, and it then closes it immediately.
    ;  - with no system message the template still emits one, holding the
    ;    reasoning-effort instructions.
    ; enable_thinking nil reproduces the template's "<think>\n\n</think>\n\n"
    ; prefill, which suppresses reasoning.
    (defun format_prompt(user_message (system_prompt nil) (enable_thinking true) (reasoning_effort "xhigh"))
        ; "medium" is valid but adds no instructions; anything else is a typo.
        ; The Jinja template raises here, so do the same rather than silently
        ; producing a prompt with no reasoning instructions at all.
        (check (not (in (strings "xhigh" "medium" "low") reasoning_effort))
            (throw (+ "Unexpected reasoning effort '" reasoning_effort
                      "'. Supported: xhigh (default), medium, low.")))
        (setq instructions "")
        (check enable_thinking
            (cond
                ((eq reasoning_effort "xhigh")
                    (setq instructions REASONING_XHIGH))
                ((eq reasoning_effort "low")
                    (setq instructions REASONING_LOW))))

        ; System block: instructions and/or caller-supplied prompt
        (setq prompt "")
        (setq sys_body "")
        (ife system_prompt
            (ife (eq instructions "")
                (setq sys_body system_prompt)
                (setq sys_body (+ instructions "\n\n" system_prompt)))
            (setq sys_body instructions))
        (check (neq sys_body "")
            (setq prompt (+ "<|im_start|>system\n" sys_body "<|im_end|>\n")))

        (+= prompt "<|im_start|>user\n" user_message "<|im_end|>\n<|im_start|>assistant\n")
        ; The generation prompt itself
        (ife enable_thinking
            (+= prompt "<think>\n")
            (+= prompt "<think>\n\n</think>\n\n"))
        prompt)

    ; Sampling: greedy below temperature 0.01, categorical otherwise
    ; Sampling: greedy below temperature 0.01, otherwise top-k then top-p
    ; (nucleus) filtering before a categorical draw. Qwen's own generation
    ;_config recommends top_k 20 / top_p 0.95; greedy decoding is discouraged
    ; in thinking mode because it tends to fall into repetition loops.
    (defun _sample(logits temperature (top_k 20) (top_p 0.95))
        (ife (< temperature 0.01)
            (mlx_argmax logits -1)
            (block
                (setq logits_flat (mlx_squeeze (mlx_divide logits (mlx_array temperature))))
                (setq shape (mlx_shape logits_flat))
                (check (eq (size shape) 1)
                    (setq logits_flat (mlx_reshape logits_flat (integers 1 (@ shape 0)))))
                (setq vocab (@ (mlx_shape logits_flat) 1))

                ; top-k: mask everything below the k-th largest logit
                (check (and top_k (> top_k 0) (< top_k vocab))
                    (setq kth (mlx_slice (mlx_topk logits_flat top_k -1)
                                  (integers 0 0) (integers 1 1)))
                    (setq logits_flat (mlx_where (mlx_less logits_flat kth)
                                          (mlx_full (mlx_shape logits_flat) -1e9 "float32")
                                          logits_flat)))

                ; top-p: keep the smallest set of tokens whose probabilities
                ; sum past top_p, working on the descending-sorted order
                (check (and top_p (< top_p 1.0))
                    (setq sorted_logits (mlx_sort logits_flat -1))
                    ; mlx_sort is ascending: reverse to get descending order
                    (setq rev_idx (mlx_astype
                                      (mlx_subtract (mlx_array (- vocab 1))
                                          (mlx_arange 0 vocab 1 "int32"))
                                      "int32"))
                    (setq desc (mlx_take sorted_logits rev_idx 1))
                    (setq probs (mlx_softmax desc -1))
                    (setq cum (mlx_cumsum probs 1 false true))
                    ; A token is kept when the cumulative mass BEFORE it is
                    ; still under top_p, so the first token always survives.
                    (setq prev (mlx_subtract cum probs))
                    (setq keep (mlx_less prev (mlx_array top_p)))
                    ; Smallest kept logit becomes the cutoff
                    (setq big (mlx_full (mlx_shape desc) 1e9 "float32"))
                    (setq kept_vals (mlx_where keep desc big))
                    (setq cutoff (mlx_min kept_vals 1 true))
                    (setq logits_flat (mlx_where (mlx_less logits_flat cutoff)
                                          (mlx_full (mlx_shape logits_flat) -1e9 "float32")
                                          logits_flat)))

                (mlx_random_categorical logits_flat -1))))

    ; Prints whatever became decodable since the last call and returns the text
    ; accounted for so far.
    ; A token boundary is not always a character boundary: the byte-level vocab
    ; can split a multi-byte character across two tokens (a space is ids 128+254,
    ; carrying bytes C4 then A0). Decoding a prefix that stops in between leaves
    ; a dangling byte that is not valid UTF-8; LispE surfaces it as a codepoint
    ; far above the Unicode range (0x10FFFF), which is what printed as a black
    ; diamond. Trim those trailing invalid codepoints and wait for the next
    ; token to complete the character.
    (defun _emit_stream(tokens shown_text)
        (setq full_text (decode tokens))
        (setq n (size full_text))
        (while (and (> n 0) (> (@ (ord (extract full_text (- n 1) n)) 0) 1114111))
            (setq full_text (extract full_text 0 (- n 1)))
            (setq n (- n 1)))
        (setq shown (size shown_text))
        (check (> (size full_text) shown)
            (printerr (extract full_text shown (size full_text))))
        full_text)

    ; Generates text from an already formatted prompt
    (defun generate(prompt_text max_new_tokens (temperature 0.7) (stream_output true))
        (println "")
        (println "Generation in progress...")
        (check (not stream_output)
            (println "Benchmark mode: token streaming disabled"))
        (println "Prompt: " prompt_text)
        (println "")

        (reset_cache)

        (setq input_ids (encode prompt_text))
        (check (nullp input_ids)
            (println "ERROR: prompt could not be encoded")
            (return ""))

        ; EOS ids are read from generation_config.json at load time, which
        ; writes eos_token_id either as a single integer or as a list.
        (setq eos_ids eos_token_ids)

        (setq num_tokens (size input_ids))
        (setq input_tensor (mlx_reshape (mlx_array input_ids nil "int32") (integers 1 num_tokens)))

        (setq generated_tokens input_ids)
        (setq completion_tokens (list))
        (setq num_generated 0)
        (setq prefill_time 0)
        (setq decode_time 0)

        (setq gen_time (elapse
            (block
                ; ---- Prefill: process the whole prompt, emit the first token ----
                (setq offset 0)
                (setq prefill_time (elapse
                    (setq last_logits (_last_logits_for_step input_tensor offset))
                    (setq first_token (_sample last_logits temperature))
                    (setq first_token_id (integer (@ (mlx_flatten first_token) 0)))))

                (push generated_tokens first_token_id)
                (push completion_tokens first_token_id)
                (+= num_generated 1)
                ; A token boundary is not always a character boundary, so
                ; _emit_stream holds back an incomplete trailing character until
                ; the next token completes it. See its own comment for details.
                (setq shown_text "")
                (ncheck stream_output
                    (printerr ".")
                    (setq shown_text (_emit_stream completion_tokens shown_text)))

                ; ---- Decode: one token at a time, reusing the caches ----
                ; Pipelined like mlx_lm: the graph for token n+1 is built and
                ; queued (mlx_async_eval) before token n is read back with
                ; mlx_tolist. Reading a token forces a full synchronisation, so
                ; issuing the next step first lets the GPU work while the CPU
                ; waits, instead of the two taking turns.
                (setq current_id first_token_id)
                (setq stop (in eos_ids current_id))
                (setq decode_time (elapse
                    (check (not stop)
                        ; Queue the first decode step
                        (setq offset (- (size generated_tokens) 1))
                        (setq step_input (mlx_reshape
                            (mlx_array (integers current_id) nil "int32") (integers 1 1)))
                        (setq pending (_sample (_last_logits_for_step step_input offset) temperature))
                        (mlx_async_eval pending)

                        (loopcount (- max_new_tokens 1) into i
                            ; Read the token queued on the previous iteration
                            (setq current_id (integer (@ (mlx_tolist pending) 0)))

                            (check (in eos_ids current_id)
                                (break))

                            (push generated_tokens current_id)
                            (push completion_tokens current_id)
                            (+= num_generated 1)

                            ; Queue the next step before touching the output
                            (check (< (+ i 2) max_new_tokens)
                                (setq offset (- (size generated_tokens) 1))
                                (setq step_input (mlx_reshape
                                    (mlx_array (integers current_id) nil "int32") (integers 1 1)))
                                (setq pending (_sample (_last_logits_for_step step_input offset) temperature))
                                (mlx_async_eval pending))

                            (ncheck stream_output
                                (printerr ".")
                                (setq shown_text (_emit_stream completion_tokens shown_text))))))))))

        ; Stats
        (setq decode_tokens (- num_generated 1))
        (setq decode_tps (if (> decode_tokens 0) (/ (* decode_tokens 1000.0) decode_time) 0))
        (setq total_tps (if (> gen_time 0) (/ (* num_generated 1000.0) gen_time) 0))
        (println "")
        (println (fill "=" 60))
        (println "Tokens generated: " num_generated " in " (/ gen_time 1000.0) " sec")
        (println "  Prefill (" num_tokens " tokens): " (/ prefill_time 1000.0) " sec")
        (println "  Decode (" decode_tokens " tokens):  " (/ decode_time 1000.0) " sec (" decode_tps " t/s)")
        (println "Total speed: " total_tps " tokens/sec")
        (println (fill "=" 60))

        (setq output_text (decode completion_tokens))
        (println "")
        (println "Generated text:")
        (println output_text)
        (println (fill "=" 60))

        output_text)

    ; Simple chat interface
    (defun chat(user_message (max_tokens 256) (temperature 0.7) (system_prompt nil) (stream_output true) (enable_thinking true) (reasoning_effort "xhigh"))
        (setq formatted_prompt (format_prompt user_message system_prompt enable_thinking reasoning_effort))
        (generate formatted_prompt max_tokens temperature stream_output))
)

; =============================================================================
; Model loading
; =============================================================================

(defun load_mlx_model(model_path)
    (println "")
    (println "1. Loading configuration...")
    (setq config (load_model_config model_path))
    (println "   ✓ model_type: " (key@ config "model_type"))
    (println "   ✓ hidden_size: " (key@ config "hidden_size")
             ", layers: " (key@ config "num_hidden_layers"))

    (println "")
    (println "2. Loading safetensors weights...")
    (setq result (load_sharded_safetensors model_path))
    (check (nullp result)
        (println "ERROR: weights could not be loaded")
        (return nil))
    (setq weights_list (@ result 0))
    (setq tensor_index (@ result 1))
    (setq metadata (@ result 2))

    ; Load tokenizer from tokenizer.json (HuggingFace BPE, tiktoken-compatible)
    (println "")
    (println "3. Loading tokenizer...")
    (setq tokenizer_path (+ model_path "/tokenizer.json"))
    (setq tokenizer nil)

    (setq tokfile (maybe (json_parse (fread tokenizer_path)) nil))
    (check tokfile
        (setq pattern (@ tokfile "pre_tokenizer" "pretokenizers" 0 "pattern" "Regex"))
        (check (nullp pattern)
            (setq pattern (@ tokfile "pre_tokenizer" "pattern" "Regex")))
        (check (nullp pattern)
            (setq pattern "(?i:'s|'t|'re|'ve|'m|'ll|'d)|[^\\r\\n\\p{L}\\p{N}]?\\p{L}+|\\p{N}| ?[^\\s\\p{L}\\p{N}]+[\\r\\n]*|\\s*[\\r\\n]+|\\s+(?!\\S)|\\s+"))
        (setq tokenizer (maybe
            (tiktoken_create (@ tokfile "model" "vocab") (@ tokfile "added_tokens") pattern)
            nil)))

    (if tokenizer
        (println "   ✓ Tokenizer loaded (" (tiktoken_vocab_size tokenizer) " tokens)")
        (println "   ⚠ Tokenizer not found: " tokenizer_path))

    ; Pre-dequantize embeddings once: the lookup table is used at every step
    (println "")
    (println "4. Pre-dequantizing embeddings...")
    (setq embed_name "language_model.model.embed_tokens")
    (setq embed_scales (get_tensor weights_list tensor_index (+ embed_name ".scales")))
    (setq cached_embeddings nil)

    (ife embed_scales
        (block
            (setq embed_weight (get_tensor weights_list tensor_index (+ embed_name ".weight")))
            (setq embed_biases (get_tensor weights_list tensor_index (+ embed_name ".biases")))
            (setq cached_embeddings (mlx_dequantize embed_weight embed_scales embed_biases
                QUANT_GROUP_SIZE QUANT_BITS))
            (println "   ✓ Embeddings dequantized and cached")
            (println "     Shape: " (mlx_shape cached_embeddings)))
        (block
            (setq cached_embeddings (get_tensor weights_list tensor_index (+ embed_name ".weight")))
            (println "   ✓ Non-quantized embeddings (no cache needed)")))

    ; Layer types drive which sublayer each decoder layer uses
    (println "")
    (println "5. Reading layer types...")
    (setq num_layers (@ config "num_hidden_layers"))
    (setq layer_types (key@ config "layer_types"))
    (check (nullp layer_types)
        ; Fallback: full attention every full_attention_interval layers
        (setq interval (select (key@ config "full_attention_interval") 4))
        (setq layer_types (list))
        (loopcount num_layers into i
            (if (eq (% (+ i 1) interval) 0)
                (push layer_types LAYER_FULL)
                (push layer_types LAYER_LINEAR))))
    (setq n_lin 0)
    (loop t layer_types
        (check (eq t LAYER_LINEAR) (+= n_lin 1)))
    (println "   ✓ " (size layer_types) " layers: " n_lin " linear / " (- (size layer_types) n_lin) " full")

    (println "")
    (println "6. Pre-extracting tensors by layer...")
    (setq layer_tensors (build_layer_tensors weights_list tensor_index num_layers layer_types))
    (setq global_tensors (extract_global_tensors weights_list tensor_index))
    (println "   ✓ " num_layers " layers pre-indexed")
    (println "   ✓ Global tensors extracted")

    ; Precompute the geometry shared by every layer
    (println "")
    (println "7. Precomputing attention parameters...")
    (setq n_heads (@ config "num_attention_heads"))
    (setq n_kv_heads (@ config "num_key_value_heads"))
    (setq head_dim (@ config "head_dim"))
    (setq hidden_size (@ config "hidden_size"))
    (setq eps (@ config "rms_norm_eps"))
    (setq hidden_out (* n_heads head_dim))
    (setq scale_float (/ 1.0 (sqrt (number head_dim))))

    ; RoPE: partial rotation over rope_dims of each head, no YaRN scaling here
    (setq rope_params (key@ config "rope_parameters"))
    (setq partial_factor (select (key@ config "partial_rotary_factor") 0.25))
    (setq rope_theta 10000000.0)
    (check rope_params
        (setq partial_factor (select (key@ rope_params "partial_rotary_factor") partial_factor))
        (setq rope_theta (number (select (key@ rope_params "rope_theta") rope_theta))))
    (setq rope_dims (integer (* (number head_dim) partial_factor)))

    ; GatedDeltaNet geometry
    (setq num_v_heads (@ config "linear_num_value_heads"))
    (setq num_k_heads (@ config "linear_num_key_heads"))
    (setq head_k_dim (@ config "linear_key_head_dim"))
    (setq head_v_dim (@ config "linear_value_head_dim"))
    (setq conv_kernel (@ config "linear_conv_kernel_dim"))
    (setq key_dim (* head_k_dim num_k_heads))
    (setq value_dim (* head_v_dim num_v_heads))
    (setq conv_dim (+ (* 2 key_dim) value_dim))

    ; End-of-sequence ids: generation_config.json writes eos_token_id either as
    ; a single integer or as a list, so normalise to a list here.
    (setq gen_cfg (maybe (json_read (+ model_path "/generation_config.json")) (dictionary)))
    (setq eos_raw (key@ gen_cfg "eos_token_id"))
    (check (nullp eos_raw)
        (setq eos_raw (key@ config "eos_token_id")))
    (ife (nullp eos_raw)
        (setq eos_ids (integers 248046 248044))
        (ife (numberp eos_raw)
            (setq eos_ids (integers eos_raw))
            (setq eos_ids eos_raw)))
    (println "   ✓ EOS ids: " eos_ids)

    (setq attn_params (numbers n_heads n_kv_heads head_dim scale_float eps hidden_out
                               rope_dims rope_theta
                               num_v_heads num_k_heads head_k_dim head_v_dim conv_kernel
                               key_dim value_dim conv_dim hidden_size))
    (println "   ✓ Attention parameters precomputed")
    (println "     n_heads=" n_heads ", n_kv_heads=" n_kv_heads ", head_dim=" head_dim)
    (println "     rope_dims=" rope_dims " (partial " partial_factor "), theta=" rope_theta)
    (println "     linear: " num_v_heads " v_heads / " num_k_heads " k_heads, "
             "k_dim=" head_k_dim ", v_dim=" head_v_dim ", conv_k=" conv_kernel)

    (setq model (MLXModel weights_list tensor_index config metadata tokenizer
                          cached_embeddings layer_tensors global_tensors attn_params
                          nil layer_types eos_ids))

    (print_memory_stats)

    (println "")
    (println "✓ Model loaded successfully!")

    model
)

; =============================================================================
; Main program
; =============================================================================

(println "")
(println "╔════════════════════════════════════════════════════════════╗")
(println "║  MLX model loader for LispE                                ║")
(println "║  Qwen3.8 27B hybrid (GatedDeltaNet + attention) 4-bit      ║")
(println "╚════════════════════════════════════════════════════════════╝")
(println "")

(setq chargement (elapse (setq model (load_mlx_model MODEL_PATH))))
(println "Loading time: " chargement " ms")

(check (nullp model)
    (println "")
    (println "ERROR: The model could not be loaded")
    (exit 1))

(println "")
(model MLXModel (info))

; =============================================================================
; Test: generation
; =============================================================================

; Qwen's recommended sampling for thinking mode: temperature 1.0 with
; top-k 20 / top-p 0.95. Greedy decoding (temperature 0.0) is discouraged here
; because it tends to fall into repetition loops.
(defun test_generation(model (question "What is the capital of France?") (max_tokens 600) (temperature 1.0))
    (model MLXModel (chat question max_tokens temperature)))

(test_generation model "Give me the code to sort lists of strings in Python." 600 1.0)
