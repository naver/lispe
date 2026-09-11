; =============================================================================
; Loading a sharded MLX Safetensors model (GPT-OSS 20B MoE)
; =============================================================================
; This script loads a GPT-OSS model in safetensors format with multiple
; files (shards) and combines all weights into a unified dictionary.
;
; MoE (Mixture of Experts) architecture with:
; - 32 experts, 4 active per token
; - Hybrid attention (sliding window 128 + full attention alternated)
; - Attention bias and sinks
; - 8-bit quantization
; Compatible with MLX models from lmstudio-community
; =============================================================================

(use 'lispe_mlx)
(use 'lispe_tiktoken)  ; Forthe BPE tokenizer (tiktoken)

; =============================================================================
; Model configuration
; =============================================================================

(setq MODEL_PATH "/Users/clauderoux/.lmstudio/models/lmstudio-community/gpt-oss-20b-MLX-8bit")

(setq DEFAULT_SYSTEM_PROMPT
    "You are ChatGPT, a large language model trained by OpenAI. Answer helpfully and naturally.")

(setq BENCHMARK_SYSTEM_PROMPT
    "You are ChatGPT, a large language model trained by OpenAI.\nKnowledge cutoff: 2024-06\nCurrent date: 2026-01-22\n\nReasoning: medium\n\n# Valid channels: analysis, commentary, final. Channel must be included for every message.")

; Experiment switch: compare the native C++ decode path against the generic MLX path.
(setq USE_NATIVE_DECODE true)
(setq USE_FULL_FORWARD_FOR_DECODE nil)
(setq USE_NATIVE_KV_CACHE true)
(setq USE_CPP_GENERATE_LOOP true)
(setq RUN_STARTUP_BENCHMARKS nil)

; =============================================================================
; Function to generate sharded safetensors file paths
; =============================================================================

; Generates safetensors file paths for num_shards files
(defun get_safetensors_files(dir_path num_shards)
    (setq files (list))
    (setq num_str (string num_shards))
    
    (loopcount num_shards into i
        (setq idx (+ i 1))
        (setq idx_str (string idx))
        ; Padding with zeros
        (while (< (size idx_str) 5)
            (setq idx_str (+ "0" idx_str)))
        (setq num_pad num_str)
        (while (< (size num_pad) 5)
            (setq num_pad (+ "0" num_pad)))
        (setq filename (+ dir_path "/model-" idx_str "-of-" num_pad ".safetensors"))
        (push files filename))
    files
)

; =============================================================================
; Function to load and merge multiple safetensors files
; =============================================================================

; Loads all safetensors files and merges tensors
(defun load_sharded_safetensors(dir_path num_shards)
    
    (println "Generating paths for " num_shards " safetensors files...")
    (setq files (get_safetensors_files dir_path num_shards))
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
        
        ; Add tensors to list and create index
        (loop name tensor_names
            (set@ tensor_index name total_tensors)
            (push all_tensors (@ tensors name))
            (+= total_tensors 1))
        
        ; Merge metadata
        (loop key (keys@ metadata)
            (set@ all_metadata key (@ metadata key)))
        
        ; Free memory (optional, garbage collector handles it)
        (mlx_synchronize)
    )
    
    (println "")
    (println (fill "=" 60))
    (println "Loading complete!")
    (println "  Total tensors: " total_tensors)
    (println (fill "=" 60))
    
    ; Return a list (tensor_list, index_dict, metadata)
    (list all_tensors tensor_index all_metadata)
)

; =============================================================================
; Function to load model configuration
; =============================================================================

; Loads the model's config.json file
(defun load_model_config(dir_path)
    (setq config_path (+ dir_path "/config.json"))
    (maybe
        (json_read config_path)
        (block
            (println "WARNING: config.json not found")
            (dictionary))))

; =============================================================================
; Function to display tensor information
; =============================================================================

; Displays information about loaded tensors
; tensor_index: dictionary name -> index
(defun print_tensor_info(tensor_index (max_display 20))
    (setq names (keys@ tensor_index))
    (setq total (size names))
    
    (println "")
    (println "Tensor information:")
    (println (fill "-" 60))
    
    ; Sort names
    (setq sorted_names (sort '< names))
    
    ; Display first tensors
    (setq displayed 0)
    (loop name sorted_names
        (check (< displayed max_display)
            (println "  " name)
            (+= displayed 1)))
    
    (check (> total max_display)
        (println "  ... and " (- total max_display) " other tensors"))
    
    (println (fill "-" 60))
    (println "Total: " total " tensors")
)

; =============================================================================
; Function to get memory statistics
; =============================================================================

; Displays MLX memory usage statistics
(defun print_memory_stats()
    (println "")
    (println "MLX memory statistics:")
    (println (fill "-" 40))
    (println "  Active memory: " (/ (mlx_get_active_memory) 1048576) " MB")
    (println "  Peak memory:    " (/ (mlx_get_peak_memory) 1048576) " MB")
    (println "  Cache memory:  " (/ (mlx_get_cache_memory) 1048576) " MB")
    (println (fill "-" 40))
)

; =============================================================================
; Function to create tensor indices by layer
; =============================================================================

; Helper function to get a tensor by name (uses index)
; Note: key@ is used because @ on a missing key throws an error
; and key@ returns nil if the key doesn't exist, otherwise the value
(defun get_tensor(weights_list tensor_index name)
    (setq idx (key@ tensor_index name))
    (if (nullp idx)
        nil
        (@ weights_list idx)))

; Pre-extracts layer tensors into an ordered list for GPT-OSS MoE
; GPT-OSS structure: attention with bias + sinks, MLP with router + experts
; Returns: (input_norm 
;            q_w q_s q_qb q_bias k_w k_s k_qb k_bias v_w v_s v_qb v_bias o_w o_s o_qb o_bias sinks
;            post_attn_norm 
;            router_w router_s router_qb router_bias
;            experts_gate_w experts_gate_s experts_gate_qb experts_gate_bias
;            experts_up_w experts_up_s experts_up_qb experts_up_bias
;            experts_down_w experts_down_s experts_down_qb experts_down_bias)
(defun extract_layer_tensors(weights_list tensor_index layer_idx)
    (setq prefix (+ "model.layers." (string layer_idx) "."))
    (setq attn (+ prefix "self_attn."))
    (setq mlp (+ prefix "mlp."))
    
    (list
        ; 0: input_layernorm
        (get_tensor weights_list tensor_index (+ prefix "input_layernorm.weight"))
        
        ; 1-4: q_proj (weight, scales, quant_biases, linear_bias)
        (get_tensor weights_list tensor_index (+ attn "q_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "q_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "q_proj.biases"))
        (get_tensor weights_list tensor_index (+ attn "q_proj.bias"))
        
        ; 5-8: k_proj
        (get_tensor weights_list tensor_index (+ attn "k_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "k_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "k_proj.biases"))
        (get_tensor weights_list tensor_index (+ attn "k_proj.bias"))
        
        ; 9-12: v_proj
        (get_tensor weights_list tensor_index (+ attn "v_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "v_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "v_proj.biases"))
        (get_tensor weights_list tensor_index (+ attn "v_proj.bias"))
        
        ; 13-16: o_proj
        (get_tensor weights_list tensor_index (+ attn "o_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "o_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "o_proj.biases"))
        (get_tensor weights_list tensor_index (+ attn "o_proj.bias"))
        
        ; 17: attention sinks
        (get_tensor weights_list tensor_index (+ attn "sinks"))
        
        ; 18: post_attention_layernorm
        (get_tensor weights_list tensor_index (+ prefix "post_attention_layernorm.weight"))
        
        ; 19-22: router (weight, scales, quant_biases, linear_bias)
        (get_tensor weights_list tensor_index (+ mlp "router.weight"))
        (get_tensor weights_list tensor_index (+ mlp "router.scales"))
        (get_tensor weights_list tensor_index (+ mlp "router.biases"))
        (get_tensor weights_list tensor_index (+ mlp "router.bias"))
        
        ; 23-26: experts gate_proj
        (get_tensor weights_list tensor_index (+ mlp "experts.gate_proj.weight"))
        (get_tensor weights_list tensor_index (+ mlp "experts.gate_proj.scales"))
        (get_tensor weights_list tensor_index (+ mlp "experts.gate_proj.biases"))
        (get_tensor weights_list tensor_index (+ mlp "experts.gate_proj.bias"))
        
        ; 27-30: experts up_proj
        (get_tensor weights_list tensor_index (+ mlp "experts.up_proj.weight"))
        (get_tensor weights_list tensor_index (+ mlp "experts.up_proj.scales"))
        (get_tensor weights_list tensor_index (+ mlp "experts.up_proj.biases"))
        (get_tensor weights_list tensor_index (+ mlp "experts.up_proj.bias"))
        
        ; 31-34: experts down_proj
        (get_tensor weights_list tensor_index (+ mlp "experts.down_proj.weight"))
        (get_tensor weights_list tensor_index (+ mlp "experts.down_proj.scales"))
        (get_tensor weights_list tensor_index (+ mlp "experts.down_proj.biases"))
        (get_tensor weights_list tensor_index (+ mlp "experts.down_proj.bias"))
    ))

; Creates the tensor list for all layers
(defun build_layer_tensors(weights_list tensor_index num_layers)
    (setq layers (list))
    (loopcount num_layers into i
        (push layers (extract_layer_tensors weights_list tensor_index i)))
    layers)

; Pre-dequantizes and transposes router weights for all layers
; Each cached router is ready for: x [B, L, hidden] @ router_t [hidden, num_experts]
(defun build_cached_routers(layer_tensors num_layers)
    (setq routers (list))
    (loopcount num_layers into i
        (setq lt (@ layer_tensors i))
        (setq router_w (@ lt L_ROUTER_WEIGHT))
        (setq router_s (@ lt L_ROUTER_SCALES))
        (setq router_qb (@ lt L_ROUTER_QBIASES))
        (if router_s
            (setq router_t (mlx_transpose (mlx_dequantize router_w router_s router_qb 64 8)))
            (setq router_t (mlx_transpose router_w)))
        (push routers router_t))
    routers)

; Extracts global tensors (embed, lm_head, final_norm) for GPT-OSS
(defun extract_global_tensors(weights_list tensor_index)
    (list
        ; 0-2: embed_tokens
        (get_tensor weights_list tensor_index "model.embed_tokens.weight")
        (get_tensor weights_list tensor_index "model.embed_tokens.scales")
        (get_tensor weights_list tensor_index "model.embed_tokens.biases")
        ; 3: final norm
        (get_tensor weights_list tensor_index "model.norm.weight")
        ; 4-6: lm_head
        (get_tensor weights_list tensor_index "lm_head.weight")
        (get_tensor weights_list tensor_index "lm_head.scales")
        (get_tensor weights_list tensor_index "lm_head.biases")
    ))

; =============================================================================
; Model structure
; =============================================================================

; Index constants for GPT-OSS MoE layer tensors
(setq L_INPUT_NORM 0)

; Attention with bias
(setq L_Q_WEIGHT 1)
(setq L_Q_SCALES 2)
(setq L_Q_QBIASES 3)
(setq L_Q_BIAS 4)
(setq L_K_WEIGHT 5)
(setq L_K_SCALES 6)
(setq L_K_QBIASES 7)
(setq L_K_BIAS 8)
(setq L_V_WEIGHT 9)
(setq L_V_SCALES 10)
(setq L_V_QBIASES 11)
(setq L_V_BIAS 12)
(setq L_O_WEIGHT 13)
(setq L_O_SCALES 14)
(setq L_O_QBIASES 15)
(setq L_O_BIAS 16)
(setq L_SINKS 17)

(setq L_POST_ATTN_NORM 18)

; MoE Router
(setq L_ROUTER_WEIGHT 19)
(setq L_ROUTER_SCALES 20)
(setq L_ROUTER_QBIASES 21)
(setq L_ROUTER_BIAS 22)

; MoE Experts - gate_proj
(setq L_GATE_WEIGHT 23)
(setq L_GATE_SCALES 24)
(setq L_GATE_QBIASES 25)
(setq L_GATE_BIAS 26)

; MoE Experts - up_proj
(setq L_UP_WEIGHT 27)
(setq L_UP_SCALES 28)
(setq L_UP_QBIASES 29)
(setq L_UP_BIAS 30)

; MoE Experts - down_proj
(setq L_DOWN_WEIGHT 31)
(setq L_DOWN_SCALES 32)
(setq L_DOWN_QBIASES 33)
(setq L_DOWN_BIAS 34)

; Constants for global tensors
(setq G_EMBED_WEIGHT 0)
(setq G_EMBED_SCALES 1)
(setq G_EMBED_BIASES 2)
(setq G_FINAL_NORM 3)
(setq G_LM_HEAD_WEIGHT 4)
(setq G_LM_HEAD_SCALES 5)
(setq G_LM_HEAD_BIASES 6)

; Transpose indices
(setq TRANSPOSE_0213 (integers 0 2 1 3))

; KV cache constants
(setq KV_KEYS 0)
(setq KV_VALUES 1)
(setq KV_OFFSET 2)

; Layer types
(setq LAYER_SLIDING "sliding_attention")
(setq LAYER_FULL "full_attention")

; Represents a loaded MLX GPT-OSS MoE model with its weights, config, and tokenizer
; Attributes:
;   weights: list of tensors (O(1) index access)
;   tensor_index: dictionary name -> index
;   config: model configuration
;   metadata: safetensors metadata
;   tokenizer: tiktoken tokenizer
;   cached_embeddings: pre-dequantized embeddings
;   rope_freqs: precomputed RoPE frequencies
;   yarn_mscale: YaRN scale factor for RoPE
;   layer_tensors: pre-extracted tensors by layer
;   global_tensors: global tensors (embed, lm_head, norm)
;   attn_params: precomputed attention parameters
;   kv_caches: native KV cache handles for generation (created on demand)
;   layer_types: list of layer types (sliding/full)
;   native_decode_handle: backend handle with pre-resolved decode tensors
(class@ MLXModel (weights tensor_index config metadata tokenizer cached_embeddings rope_freqs yarn_mscale layer_tensors global_tensors attn_params kv_caches layer_types kv_cache_max_len cached_routers native_decode_handle)
    
    ; =========================================================================
    ; Basic accessors
    ; =========================================================================
    
    ; Get a tensor by name
    (defun get_weight(name)
        (setq idx (key@ tensor_index name))
        (check (nullp idx)
            (println "WARNING: Tensor not found: " name)
            (return nil))
        (@ weights idx))
    
    ; Check if a tensor exists
    (defun has_weight(name)
        (neq (key@ tensor_index name) nil))
    
    ; List tensor names (optionally filtered by pattern)
    (defun list_weights((pattern ""))
        (setq all_names (keys@ tensor_index))
        (ncheck (eq pattern "")
            (setq rx (rgx pattern))
            (filter (λ(n) (rgx_match rx n)) all_names)
            all_names))
    
    ; Return the number of tensors
    (defun num_weights()
        (size weights))
    
    ; Get a config value
    (defun get_config(key (default nil))
        (select (key@ config key) default))
    
    ; Display model information
    (defun info()
        (println "MLX GPT-OSS MoE model:")
        (println "  Tensors: " (num_weights))
        (println "  Type: " (get_config "model_type" "unknown"))
        (println "  Architecture: " (get_config "architectures" "unknown"))
        (println "  Experts: " (get_config "num_local_experts" 0) " (active: " (get_config "num_experts_per_tok" 0) ")")
        (println "  Sliding window: " (get_config "sliding_window" 0))
        (check tokenizer
            (println "  Vocabulary: " (tiktoken_vocab_size tokenizer) " tokens")))
    
    ; =========================================================================
    ; Tokenizer (tiktoken)
    ; =========================================================================
    
    ; Encode text into tokens
    (defun encode(text)
        (check (not tokenizer)
            (println "ERROR: Tokenizer not loaded")
            (return nil))
        (tiktoken_encode tokenizer text))
    
    ; Decode tokens into text
    (defun decode(tokens)
        (check (not tokenizer)
            (println "ERROR: Tokenizer not loaded")
            (return nil))
        (tiktoken_decode tokenizer tokens))
    
    ; Return vocabulary size
    (defun vocab_size()
        (check (not tokenizer)
            (return 0))
        (tiktoken_vocab_size tokenizer))
    
    ; =========================================================================
    ; Internal utility functions
    ; =========================================================================
    
    ; Linear projection with quantized or non-quantized weights
    ; Note: For MLX 8-bit quantization, quant_biases is mandatory if scales exist
    ; transpose_weight is always true for our use
    (defun _linear(x weight scales quant_biases)
        (if scales
            ; MLX 8-bit quantization
            (mlx_quantized_matmul x weight scales quant_biases true 64 8)
            (mlx_matmul x . mlx_transpose weight)))
    
    ; Linear projection with additional bias (for attention)
    (defun _linear_bias(x weight scales quant_biases linear_bias)
        (setq out (_linear x weight scales quant_biases))
        (if linear_bias
            (mlx_add out linear_bias)
            out))
    
    ; Standard RMSNorm
    (defun _rms_norm(x weight eps)
        (mlx_rms_norm x weight eps false))
    
    ; Apply YaRN RoPE to queries/keys
    ; Uses mlx_rope with precomputed frequencies
    ; Note: mscale is folded into the attention scale factor (mscale^2)
    (defun _apply_rope(x freqs offset)
        (setq shape (mlx_shape x))
        (setq head_dim (@ shape 3))
        ; Use mlx_rope with custom frequencies
        (mlx_rope x head_dim false nil 1.0 offset freqs))
    
    ; =========================================================================
    ; KV Cache
    ; =========================================================================
    
    ; Create a native KV cache for one layer
    (defun _make_kv_cache(layer_idx batch_size)
        (if USE_NATIVE_KV_CACHE
            (block
                (setq n_kv_heads (@ attn_params 1))
                (setq head_dim (@ attn_params 2))
                (setq sliding_window (@ attn_params 7))
                (setq is_sliding (eq (@ layer_types layer_idx) LAYER_SLIDING))
                (setq cache_size (if is_sliding sliding_window kv_cache_max_len))
                (mlx_kv_native_cache_create batch_size n_kv_heads head_dim cache_size is_sliding))
            (list nil nil 0)))
    
    ; Create all KV caches (one per layer)
    (defun _make_all_kv_caches(batch_size)
        (setq num_layers (@ config "num_hidden_layers"))
        (setq caches (list))
        (loopcount num_layers into i
            (push caches (_make_kv_cache i batch_size)))
        caches)
    
    ; Update a KV cache
    (defun _update_kv_cache(cache keys values)
        (setq cached_keys (@ cache KV_KEYS))
        (setq cached_values (@ cache KV_VALUES))
        (ife cached_keys
            (block
                (setq new_keys (mlx_concatenate (list cached_keys keys) 2))
                (setq new_values (mlx_concatenate (list cached_values values) 2)))
            (setq new_keys keys)
            (setq new_values values))
        (set@ cache KV_KEYS new_keys)
        (set@ cache KV_VALUES new_values)
        (set@ cache KV_OFFSET (@ (mlx_shape new_keys) 2))
        (list new_keys new_values))
    
    ; Update a KV cache with sliding window
    (defun _update_kv_cache_sliding(cache keys values sliding_window)
        (setq cached_keys (@ cache KV_KEYS))
        (setq cached_values (@ cache KV_VALUES))
        (ife cached_keys
            (block
                (setq new_keys (mlx_concatenate (list cached_keys keys) 2))
                (setq new_values (mlx_concatenate (list cached_values values) 2))
                ; Truncate if exceeds sliding_window
                (setq seq_len (@ (mlx_shape new_keys) 2))
                (check (> seq_len sliding_window)
                    (setq start (- seq_len sliding_window))
                    (setq B (@ (mlx_shape new_keys) 0))
                    (setq H (@ (mlx_shape new_keys) 1))
                    (setq D (@ (mlx_shape new_keys) 3))
                    (setq new_keys (mlx_slice new_keys (integers 0 0 start 0) (integers B H seq_len D)))
                    (setq new_values (mlx_slice new_values (integers 0 0 start 0) (integers B H seq_len D)))))
            (setq new_keys keys)
            (setq new_values values))
        (set@ cache KV_KEYS new_keys)
        (set@ cache KV_VALUES new_values)
        (set@ cache KV_OFFSET (@ (mlx_shape new_keys) 2))
        (list new_keys new_values))
    
    ; Reset KV caches
    (defun reset_cache()
        (check kv_caches
            (check USE_NATIVE_KV_CACHE
                (loop cache_id kv_caches
                    (mlx_kv_cache_free cache_id))))
        (setqi kv_caches nil))
    
    ; =========================================================================
    ; Forward pass - Components
    ; =========================================================================
    
    ; Custom SwiGLU for GPT-OSS
    ; x_linear (from up_proj), x_glu (from gate_proj)
    ; Formula: out_glu * (x_linear + 1)
    ; where out_glu = x_glu * sigmoid(alpha * x_glu)
    (defun _swiglu(x_linear x_glu)
        (setq alpha 1.702)
        (setq limit 7.0)
        
        ; Clamp inputs
        (setq x_glu_clamped (mlx_clip x_glu nil limit))
        (setq x_linear_clamped (mlx_clip x_linear (sign limit) limit))
        
        ; glu_scaled = alpha * x_glu
        (setq glu_scaled (mlx_multiply (mlx_array alpha) x_glu_clamped))
        
        ; sig = sigmoid(glu_scaled)
        (setq sig (mlx_sigmoid glu_scaled))
        
        ; out_glu = x_glu * sig
        (setq out_glu (mlx_multiply x_glu_clamped sig))
        
        ; return out_glu * (x_linear + 1)
        (mlx_multiply out_glu . mlx_add x_linear_clamped . mlx_array 1.0))
    
    ; MoE MLP forward
    ; x: [B, L, hidden_size]
    ; router selects top-k experts among num_experts
    ; experts weights: [num_experts, ...]
    (defun _moe_mlp(x lt layer_idx)
        (setq shape (mlx_shape x))
        (setq B (@ shape 0))
        (setq L (@ shape 1))
        (setq hidden_size (@ shape 2))
        
        ; MoE parameters
        (setq num_experts (@ config "num_local_experts"))       ; 32
        (setq experts_per_tok (@ config "num_experts_per_tok")) ; 4
        
        ; Router: [B, L, hidden_size] -> [B, L, num_experts]
        ; The router is quantized with shape [32, 720] -> [32, 2880] dequantized
        ; Must dequantize then do x @ router^T
        (setq router_w (@ lt L_ROUTER_WEIGHT))
        (setq router_s (@ lt L_ROUTER_SCALES))
        (setq router_qb (@ lt L_ROUTER_QBIASES))
        (setq router_b (@ lt L_ROUTER_BIAS))
        
        ; Router is cached as [hidden_size, num_experts] at load time
        ; Matmul: x [B, L, 2880] @ router_t [2880, 32] -> [B, L, 32]
        (setq router_t (@ cached_routers layer_idx))
        (setq router_logits (mlx_matmul x router_t))
        ; Add linear bias if present
        (check router_b
            (setq router_logits (mlx_add router_logits router_b)))
        
        ; Get expert tensors (quantized)
        ; Shape: [num_experts, out_features, in_features_packed] 
        (setq gate_w (@ lt L_GATE_WEIGHT))
        (setq gate_s (@ lt L_GATE_SCALES))
        (setq gate_qb (@ lt L_GATE_QBIASES))
        (setq gate_b (@ lt L_GATE_BIAS))
        
        (setq up_w (@ lt L_UP_WEIGHT))
        (setq up_s (@ lt L_UP_SCALES))
        (setq up_qb (@ lt L_UP_QBIASES))
        (setq up_b (@ lt L_UP_BIAS))
        
        (setq down_w (@ lt L_DOWN_WEIGHT))
        (setq down_s (@ lt L_DOWN_SCALES))
        (setq down_qb (@ lt L_DOWN_QBIASES))
        (setq down_b (@ lt L_DOWN_BIAS))
        
        ; OPTIMIZATION: For L=1 (token-by-token generation), use mlx_fused_moe
        ; which loops over the k active experts with quantized_matmul (no dequantization)
        (if (eq L 1)
            ; === OPTIMIZED PATH: mlx_fused_moe (C++) ===
            (mlx_fused_moe_router x router_t router_b
                gate_w gate_s gate_qb gate_b
                up_w up_s up_qb up_b
                down_w down_s down_qb down_b
                num_experts experts_per_tok 64 8 "swiglu")
            
            ; === PREFILL PATH: LispE loop over all experts ===
            ; Note: Keep the has_any check because avoiding 28 useless experts
            ; is worth more than the 32 mx::eval checks
            (block
                ; Python mlx_lm path: top-k on router logits, then softmax only on selected logits
                (setq partitioned_indices (mlx_argpartition router_logits (sign experts_per_tok) -1))

                ; Take the last k indices (largest logits)
                (setq start_idx (- num_experts experts_per_tok))
                (setq expert_indices (mlx_slice partitioned_indices 
                    (integers 0 0 start_idx) (integers B L num_experts)))  ; [B, L, k]

                ; Softmax only on selected expert logits
                (setq expert_logits (mlx_take_along_axis router_logits expert_indices -1))  ; [B, L, k]
                (setq expert_weights (mlx_softmax expert_logits -1))

                ; To simplify, handle B=1, any L
                ; x_2d: [L, hidden_size]
                (setq x_2d (mlx_squeeze x 0))
                (setq indices_2d (mlx_squeeze expert_indices 0))  ; [L, k]
                (setq weights_2d (mlx_squeeze expert_weights 0))  ; [L, k]
                
                ; Initialize output [L, hidden_size]
                (setq output (mlx_zeros (integers L hidden_size) "float32"))
                
                ; Pre-create zero tensor for masking (reused)
                (setq zeros_weights (mlx_zeros (mlx_shape weights_2d) "float32"))
                
                ; For each possible expert, process all tokens using it
                (loopcount num_experts into expert_id
                    ; Create mask for tokens using this expert
                    ; indices_2d: [L, k], look for where indices_2d == expert_id
                    (setq expert_mask (mlx_equal indices_2d (mlx_array expert_id)))  ; [L, k] bool
                    
                    ; Check if at least one token uses this expert
                    (setq any_per_row (mlx_any expert_mask -1))  ; [L] bool
                    (setq has_tokens (mlx_any any_per_row))
                    
                    ; Convert bool scalar to LispE value (via flatten then @)
                    (setq has_tokens_flat (mlx_flatten has_tokens))
                    (setq has_any (@ has_tokens_flat 0))
                    
                    (check has_any
                        ; Get this expert's weights
                        (setq idx_arr (mlx_array (integers expert_id) nil "int32"))
                        (setq e_gate_w (mlx_squeeze (mlx_take gate_w idx_arr 0) 0))
                        (setq e_gate_s (mlx_squeeze (mlx_take gate_s idx_arr 0) 0))
                        (setq e_gate_qb (mlx_squeeze (mlx_take gate_qb idx_arr 0) 0))
                        
                        (setq e_up_w (mlx_squeeze (mlx_take up_w idx_arr 0) 0))
                        (setq e_up_s (mlx_squeeze (mlx_take up_s idx_arr 0) 0))
                        (setq e_up_qb (mlx_squeeze (mlx_take up_qb idx_arr 0) 0))
                        
                        (setq e_down_w (mlx_squeeze (mlx_take down_w idx_arr 0) 0))
                        (setq e_down_s (mlx_squeeze (mlx_take down_s idx_arr 0) 0))
                        (setq e_down_qb (mlx_squeeze (mlx_take down_qb idx_arr 0) 0))
                        
                        ; Forward for ALL tokens with this expert
                        (setq gate_out (mlx_quantized_matmul x_2d e_gate_w e_gate_s e_gate_qb true 64 8))
                        (check gate_b
                            (setq e_gate_b (mlx_squeeze (mlx_take gate_b idx_arr 0) 0))
                            (setq gate_out (mlx_add gate_out e_gate_b)))
                        
                        (setq up_out (mlx_quantized_matmul x_2d e_up_w e_up_s e_up_qb true 64 8))
                        (check up_b
                            (setq e_up_b (mlx_squeeze (mlx_take up_b idx_arr 0) 0))
                            (setq up_out (mlx_add up_out e_up_b)))
                        
                        ; hidden = SwiGLU(up_out, gate_out)
                        (setq hidden (_swiglu up_out gate_out))
                        
                        ; expert_out = hidden @ down_w
                        (setq expert_out (mlx_quantized_matmul hidden e_down_w e_down_s e_down_qb true 64 8))
                        (check down_b
                            (setq e_down_b (mlx_squeeze (mlx_take down_b idx_arr 0) 0))
                            (setq expert_out (mlx_add expert_out e_down_b)))
                        
                        ; Compute masked weights for this expert
                        (setq expert_weights_masked (mlx_where expert_mask weights_2d zeros_weights))
                        (setq token_weights (mlx_sum expert_weights_masked -1 false))  ; [L]
                        (setq token_weights (mlx_reshape token_weights (integers L 1)))  ; [L, 1]
                        
                        ; Weight and accumulate
                        (setq weighted_out (mlx_multiply expert_out token_weights))
                        (setq output (mlx_add output weighted_out))))
                
                ; Reshape to [B, L, hidden_size]
                (mlx_reshape output (integers B L hidden_size)))))
    
    ; Attention forward for GPT-OSS (with bias, optional sliding window, sinks)
    (defun _attention(x lt offset kv_cache layer_idx)
        (setq n_heads (@ attn_params 0))
        (setq n_kv_heads (@ attn_params 1))
        (setq head_dim (@ attn_params 2))
        (setq scale (@ attn_params 3))
        (setq eps (@ attn_params 4))
        (setq repeats (@ attn_params 5))
        (setq hidden_out (@ attn_params 6))
        (setq sliding_window (@ attn_params 7))
        
        ; Determine if this layer uses sliding window
        (setq is_sliding (eq (@ layer_types layer_idx) LAYER_SLIDING))
        
        (setq shape (mlx_shape x))
        (setq B (@ shape 0))
        (setq L (@ shape 1))
        
        ; QKV projections with bias
        (setq queries (_linear_bias x (@ lt L_Q_WEIGHT) (@ lt L_Q_SCALES) (@ lt L_Q_QBIASES) (@ lt L_Q_BIAS)))
        (setq keys (_linear_bias x (@ lt L_K_WEIGHT) (@ lt L_K_SCALES) (@ lt L_K_QBIASES) (@ lt L_K_BIAS)))
        (setq values (_linear_bias x (@ lt L_V_WEIGHT) (@ lt L_V_SCALES) (@ lt L_V_QBIASES) (@ lt L_V_BIAS)))
        
        ; Reshape [B, L, n_heads * head_dim] -> [B, n_heads, L, head_dim]
        (setq queries (mlx_transpose (mlx_reshape queries (integers B L n_heads head_dim)) TRANSPOSE_0213))
        (setq keys (mlx_transpose (mlx_reshape keys (integers B L n_kv_heads head_dim)) TRANSPOSE_0213))
        (setq values (mlx_transpose (mlx_reshape values (integers B L n_kv_heads head_dim)) TRANSPOSE_0213))
        
        ; Apply RoPE
        (setq queries (_apply_rope queries rope_freqs offset))
        (setq keys (_apply_rope keys rope_freqs offset))
        
        ; Get sinks for this layer
        (setq sinks (@ lt L_SINKS))
        
        ; Prepare mask
        ; For L=1 (generation), no mask needed
        ; For L>1, use "causal" (sliding window handled by KV cache)
        (if (== L 1)
            (setq mask_mode nil)
            (setq mask_mode "causal"))
        
        ; Native KV cache append + direct attention
        ; Function automatically handles GQA (n_kv_heads != n_heads)
        (if kv_cache
            (if USE_NATIVE_KV_CACHE
                (block
                    (mlx_kv_native_cache_append kv_cache keys values)
                    (setq attention_output (mlx_kv_native_cache_attention kv_cache queries scale mask_mode sinks)))
                (block
                    (if is_sliding
                        (setq kv_pair (_update_kv_cache_sliding kv_cache keys values sliding_window))
                        (setq kv_pair (_update_kv_cache kv_cache keys values)))
                    (setq cached_keys (@ kv_pair 0))
                    (setq cached_values (@ kv_pair 1))
                    (setq attention_output (mlx_scaled_dot_product_attention queries cached_keys cached_values scale mask_mode () sinks))))
            (setq attention_output (mlx_scaled_dot_product_attention queries keys values scale mask_mode () sinks)))
        
        ; Reshape [B, n_heads, L, head_dim] -> [B, L, hidden_out]
        (setq attention_output (mlx_reshape (mlx_transpose attention_output TRANSPOSE_0213) (integers B L hidden_out)))
        (_linear_bias attention_output (@ lt L_O_WEIGHT) (@ lt L_O_SCALES) (@ lt L_O_QBIASES) (@ lt L_O_BIAS)))
    
    ; Transformer block for GPT-OSS MoE
    (defun _transformer_block(x lt offset kv_cache layer_idx)
        (setq eps (@ attn_params 4))
        
        ; Pre-norm + attention
        (setq normed_x (_rms_norm x (@ lt L_INPUT_NORM) eps))
        (setq attn_output (_attention normed_x lt offset kv_cache layer_idx))
        
        ; Residual connection
        (setq h (mlx_add x attn_output))
        
        ; Pre-norm + MoE MLP
        (setq normed_h (_rms_norm h (@ lt L_POST_ATTN_NORM) eps))
        (setq mlp_output (_moe_mlp normed_h lt layer_idx))
        
        ; Residual connection
        (setq layer_output (mlx_add h mlp_output))
        
        layer_output)
    
    ; =========================================================================
    ; Full forward pass
    ; =========================================================================
    
    ; Forward pass - returns logits
    (defun forward(input_ids offset)
        (setq num_layers (@ config "num_hidden_layers"))
        (setq eps (@ attn_params 4))
        
        ; Get shapes
        (setq input_shape (mlx_shape input_ids))
        (setq B (@ input_shape 0))
        (setq L (@ input_shape 1))
        
        ; Embedding
        (setq flat_ids (mlx_flatten input_ids))
        (setq embeddings_flat (mlx_take cached_embeddings flat_ids 0))
        (setq hidden_size (@ (mlx_shape cached_embeddings) 1))
        (setq hidden_states (mlx_reshape embeddings_flat (integers B L hidden_size)))
        
        ; Create KV caches if needed
        (check (nullp kv_caches)
            (setqi kv_caches (_make_all_kv_caches B)))
        
        ; Pass through all layers
        (loopcount num_layers into i
            (setq hidden_states (_transformer_block 
                hidden_states 
                (@ layer_tensors i)
                offset 
                (@ kv_caches i)
                i)))
        
        ; Final LayerNorm
        (setq hidden_states (_rms_norm hidden_states (@ global_tensors G_FINAL_NORM) eps))
        
        ; LM Head
        (_linear hidden_states 
            (@ global_tensors G_LM_HEAD_WEIGHT) 
            (@ global_tensors G_LM_HEAD_SCALES) 
            (@ global_tensors G_LM_HEAD_BIASES)))

    ; Native decode step for B=1, L=1.
    ; The current backend implementation supports the gpt_oss_moe layout.
    (defun forward_decode_step(input_ids offset)
        (if (not USE_NATIVE_KV_CACHE)
            (forward input_ids offset)
            (if (and USE_NATIVE_DECODE native_decode_handle)
                (mlx_forward_decode_step_model native_decode_handle input_ids offset kv_caches)
                (mlx_forward_decode_step input_ids offset
                    cached_embeddings rope_freqs yarn_mscale
                    layer_tensors global_tensors attn_params
                    kv_caches layer_types cached_routers
                    (@ config "num_experts_per_tok") "gpt_oss_moe"))))

    ; Returns last-token logits for decode. Native handle path already emits [1, vocab].
    ; Fallback keeps the old backend and extracts the last sequence position.
    (defun forward_decode_last_logits(input_ids offset)
        (if USE_FULL_FORWARD_FOR_DECODE
            (block
                (setq logits (forward input_ids offset))
                (setq last_idx (- (@ (mlx_shape logits) 1) 1))
                (mlx_take logits (mlx_array (integers last_idx) nil "int32") 1))
            (if (and USE_NATIVE_DECODE native_decode_handle)
                (forward_decode_step input_ids offset)
                (block
                    (setq logits (forward_decode_step input_ids offset))
                    (setq last_idx (- (@ (mlx_shape logits) 1) 1))
                    (mlx_take logits (mlx_array (integers last_idx) nil "int32") 1)))))

    (defun effective_decode_backend()
        (if USE_FULL_FORWARD_FOR_DECODE
            "full_forward"
            (if USE_NATIVE_KV_CACHE
                (if (and USE_NATIVE_DECODE native_decode_handle)
                    "native"
                    "generic_step")
                "full_forward_cache_fallback")))
    
    ; =========================================================================
    ; Text generation
    ; =========================================================================
    
    ; Formats a chat prompt for GPT-OSS
    ; Format: <|start|>system<|message|>...<|end|><|start|>user<|message|>...<|end|><|start|>assistant
    (defun format_prompt(user_message (system_prompt DEFAULT_SYSTEM_PROMPT))
        (setq prompt (+ "<|start|>system<|message|>" system_prompt "<|end|>"))
        (+= prompt "<|start|>user<|message|>" user_message "<|end|><|start|>assistant")
        prompt)
    
    ; Sampling
    (defun _sample(logits temperature)
        (ife (< temperature 0.01)
            (mlx_argmax logits -1)

            (setq logits_flat (mlx_squeeze . mlx_divide logits . mlx_array temperature))
            (setq shape (mlx_shape logits_flat))
            (check (eq (size shape) 1)
                (setq logits_flat (mlx_reshape logits_flat (integers 1 (@ shape 0)))))
            (mlx_random_categorical logits_flat -1)))

    (defun _last_logits_for_step(input_tensor offset)
        (if (> (@ (mlx_shape input_tensor) 1) 1)
            (block
                (setq logits (forward input_tensor offset))
                (setq last_idx (- (@ (mlx_shape logits) 1) 1))
                (mlx_take logits (mlx_array (integers last_idx) nil "int32") 1))
            (forward_decode_last_logits input_tensor offset)))
    
    ; Generates text from a prompt
    (defun generate(prompt_text max_new_tokens (temperature 0.7) (stream_output true) (profile_decode false))
        (println "")
        (println "Generation in progress...")
        (check (not stream_output)
            (println "Benchmark mode: token streaming disabled"))
        (println "Decode backend: " (effective_decode_backend))
        (println "KV cache backend: " (if USE_NATIVE_KV_CACHE "native" "lispe"))
        (println "Prompt: " prompt_text)
        (println "")
        
        ; Reset cache
        (reset_cache)

        ; Optional native decode profiling
        (setq decode_profile_report nil)
        (check (and profile_decode native_decode_handle)
            (mlx_decode_model_profile_start native_decode_handle))
        
        ; Encode prompt
        (setq input_ids (encode prompt_text))
        
        ; BOS = 199998 (<|startoftext|>), EOS = 200002 (<|return|>)
        (setq BOS_TOKEN 199998)
        (setq EOS_TOKEN 200002)
        
        ; Add BOS token at start if missing
        (check (not (eq (@ input_ids 0) BOS_TOKEN))
            (pushfirst input_ids BOS_TOKEN))
        
        ; Convert to MLX array
        (setq num_tokens (size input_ids))
        (setqi kv_cache_max_len (+ num_tokens max_new_tokens))
        (setq input_tensor (mlx_array input_ids nil "int32"))
        (setq input_tensor (mlx_reshape input_tensor (integers 1 num_tokens)))
        
        ; List to store generated tokens
        (setq generated_tokens input_ids)
        (setq completion_tokens (list))
        (setq num_generated 0)
        (setq prefill_time 0)
        (setq decode_time 0)
        (setq pending_token nil)
        (setq pending_is_prefill nil)
        (setq pending_step_time 0)
        (setq greedy_mode (< temperature 0.01))
        
        ; Measure time
        (setq gen_time (elapse
            (if (and USE_CPP_GENERATE_LOOP native_decode_handle)
                ; C++ tight loop with async pipelining - no LispE overhead per token
                ; First do prefill in LispE (creates kv_caches), then decode in C++
                (block
                    ; Prefill step (creates KV caches, processes full prompt)
                    (setq offset (- (size generated_tokens) (@ (mlx_shape input_tensor) 1)))
                    (setq prefill_time (elapse
                        (setq last_logits (_last_logits_for_step input_tensor offset))
                        (setq first_token (mlx_argmax last_logits -1))
                        (setq first_token_flat (mlx_flatten first_token))
                        (setq first_token_id (integer (@ first_token_flat 0)))))
                    (push generated_tokens first_token_id)
                    (push completion_tokens first_token_id)
                    (+= num_generated 1)
                    (ncheck stream_output
                        (printerr ".")
                        (printerr (decode (integers first_token_id))))
                    
                    ; Now decode remaining tokens in C++ loop
                    (check (and (neq first_token_id EOS_TOKEN) (> max_new_tokens 1))
                        (setq decode_input (mlx_reshape (mlx_array (integers first_token_id) nil "int32") (integers 1 1)))
                        (setq decode_offset (size generated_tokens))
                        (setq loop_result (mlx_generate_loop_v2 native_decode_handle decode_input (- decode_offset 1) kv_caches (- max_new_tokens 1) temperature EOS_TOKEN))
                        (setq decode_tokens_list (@ loop_result 0))
                        (setq decode_time (@ loop_result 2))
                        (setq num_generated (+ num_generated (size decode_tokens_list)))
                        (nconc generated_tokens decode_tokens_list)
                        (nconc completion_tokens decode_tokens_list)
                        ; Print dots or text for output
                        (ncheck stream_output
                            (printerr (fill "." (size decode_tokens_list)))
                            (printerr (decode decode_tokens_list)))))
            (if greedy_mode
                ; Greedy decode with async pipelining (like Python mlx_lm)
                ; Double-buffer: launch step N+1 on GPU while consuming step N result
                (block
                    ; Prime the pipeline: launch first step
                    (setq offset (- (size generated_tokens) (@ (mlx_shape input_tensor) 1)))
                    (setq pending_is_prefill (> (@ (mlx_shape input_tensor) 1) 1))
                    (setq pending_step_time (elapse
                        (setq last_logits (_last_logits_for_step input_tensor offset))
                        (setq pending_token (mlx_argmax last_logits -1))
                        (mlx_async_eval pending_token)))

                    (loopcount max_new_tokens into i
                        ; Consume the pending result (blocks until GPU done)
                        (setq wait_time (elapse
                            (setq next_token_flat (mlx_flatten pending_token))
                            (setq next_token_id (integer (@ next_token_flat 0)))))
                        (setq step_time (+ pending_step_time wait_time))

                        (if pending_is_prefill
                            (+= prefill_time step_time)
                            (+= decode_time step_time))

                        (ncheck stream_output
                            (printerr ".")
                            (printerr (decode (integers next_token_id))))
                        (+= num_generated 1)

                        (check (eq next_token_id EOS_TOKEN)
                            (break))

                        (push generated_tokens next_token_id)
                        (push completion_tokens next_token_id)
                        (setq input_tensor (mlx_reshape (mlx_array (integers next_token_id) nil "int32") (integers 1 1)))

                        ; Launch next step asynchronously (GPU starts while we loop back)
                        (check (< num_generated max_new_tokens)
                            (setq offset (- (size generated_tokens) (@ (mlx_shape input_tensor) 1)))
                            (setq pending_is_prefill false)
                            (setq pending_step_time (elapse
                                (setq last_logits (_last_logits_for_step input_tensor offset))
                                (setq pending_token (mlx_argmax last_logits -1))
                                (mlx_async_eval pending_token))))))

                (block
                    (loopcount max_new_tokens into i
                        (if pending_token
                            (block
                                ; Consume the previously launched token
                                (setq wait_time (elapse
                                    (setq next_token_flat (mlx_flatten pending_token))
                                    (setq next_token_id (integer (@ next_token_flat 0)))))
                                (setq step_time (+ pending_step_time wait_time))
                                
                                (if pending_is_prefill
                                    (+= prefill_time step_time)
                                    (+= decode_time step_time))
                                
                                ; Stream output
                                (ncheck stream_output
                                    (printerr ".")
                                    (printerr (decode (integers next_token_id))))
                                (+= num_generated 1)
                                
                                ; Check end (EOS = 200002)
                                (check (eq next_token_id EOS_TOKEN)
                                    (setq pending_token nil)
                                    (break))
                                
                                ; Add and prepare next
                                (push generated_tokens next_token_id)
                                (push completion_tokens next_token_id)
                                (setq input_tensor (mlx_reshape (mlx_array (integers next_token_id) nil "int32") (integers 1 1)))
                                
                                ; Launch the next token asynchronously if needed
                                (check (< num_generated max_new_tokens)
                                    (setq offset (- (size generated_tokens) (@ (mlx_shape input_tensor) 1)))
                                    (setq pending_is_prefill (> (@ (mlx_shape input_tensor) 1) 1))
                                    (setq pending_step_time (elapse
                                        (setq last_logits (_last_logits_for_step input_tensor offset))
                                        (setq pending_token (_sample last_logits temperature))
                                        (setq pending_token (mlx_async_eval pending_token))))))
                            
                            ; Prime the pipeline with the first token
                            (block
                                (setq offset (- (size generated_tokens) (@ (mlx_shape input_tensor) 1)))
                                (setq pending_is_prefill (> (@ (mlx_shape input_tensor) 1) 1))
                                (setq pending_step_time (elapse
                                    (setq last_logits (_last_logits_for_step input_tensor offset))
                                    (setq pending_token (_sample last_logits temperature))
                                    (setq pending_token (mlx_async_eval pending_token)))))))

                    ; Flush the final pending token if any
                    (check pending_token
                        (setq wait_time (elapse
                            (setq next_token_flat (mlx_flatten pending_token))
                            (setq next_token_id (integer (@ next_token_flat 0)))))
                        (setq step_time (+ pending_step_time wait_time))
                        (if pending_is_prefill
                            (+= prefill_time step_time)
                            (+= decode_time step_time))
                        (check stream_output
                            (printerr (decode (integers next_token_id))))
                        (+= num_generated 1)
                        (check (neq next_token_id EOS_TOKEN)
                            (push generated_tokens next_token_id)
                            (push completion_tokens next_token_id))))))))
        
        ; Stats
        (setq decode_tokens (- num_generated 1))  ; Premier token est du prefill
        (setq decode_tps (if (> decode_tokens 0) (/ (* decode_tokens 1000.0) decode_time) 0))
        (setq total_tps (/ (* num_generated 1000.0) gen_time))
        (println "")
        (println (fill "=" 60))
        (println "Tokens generated: " num_generated " in " (/ gen_time 1000.0) " sec")
        (println "  Prefill (" num_tokens " tokens): " (/ prefill_time 1000.0) " sec (" (/ (* num_tokens 1000.0) prefill_time) " t/s)")
        (println "  Decode (" decode_tokens " tokens):  " (/ decode_time 1000.0) " sec (" decode_tps " t/s)")
        (println "Total speed: " total_tps " tokens/sec")
        (println (fill "=" 60))

        (check (and profile_decode native_decode_handle)
            (setq decode_profile_report (mlx_decode_model_profile_report native_decode_handle)))
        
        ; Decode
        (setq output_text (decode completion_tokens))
        (println "")
        (println "Generated text:")
        (println output_text)
        (check decode_profile_report
            (println "")
            (println "Decode profile report:")
            (loop entry decode_profile_report
                (println "  " entry)))
        (println (fill "=" 60))
        
        output_text)
    
    ; Simple chat interface
    (defun chat(user_message (max_tokens 256) (temperature 0.7) (system_prompt nil) (stream_output true) (profile_decode false))
        (if system_prompt
            (setq formatted_prompt (format_prompt user_message system_prompt))
            (setq formatted_prompt (format_prompt user_message)))
        (generate formatted_prompt max_tokens temperature stream_output profile_decode))
)

; =============================================================================
; Main loading function
; =============================================================================

; Loads a full MLX model (sharded safetensors + config)
(defun load_mlx_model(model_path)
    
    (println (fill "=" 60))
    (println "Loading MLX GPT-OSS MoE model")
    (println "Path: " model_path)
    (println (fill "=" 60))
    
    ; Load config
    (println "")
    (println "1. Loading configuration...")
    (setq config (load_model_config model_path))
    (println "   ✓ Configuration loaded")
    
    ; Display some config info
    (check (key@ config "model_type")
        (println "   Type: " (@ config "model_type")))
    (check (key@ config "architectures")
        (println "   Architecture: " (@ config "architectures")))
    (check (key@ config "num_local_experts")
        (println "   Experts: " (@ config "num_local_experts") " (active: " (@ config "num_experts_per_tok") ")"))
    
    ; Load weights (5 shards for GPT-OSS 20B 8-bit)
    (println "")
    (println "2. Loading weights...")
    (setq result (load_sharded_safetensors model_path 5))
    
    (check (nullp result)
        (println "ERROR: Failed to load weights")
        (return nil))
    
    (setq weights_list (@ result 0))
    (setq tensor_index (@ result 1))
    (setq metadata (@ result 2))
    
    ; Load tokenizer from tokenizer.json
    (println "")
    (println "3. Loading tokenizer...")
    (setq tokenizer_path (+ model_path "/tokenizer.json"))
    (setq tokenizer nil)
    
    (setq tokfile (maybe (json_parse (fread tokenizer_path)) nil))
    (check tokfile
        (setq pattern (@ tokfile "pre_tokenizer" "pretokenizers" 0 "pattern" "Regex"))
        ; If no pattern in pretokenizers, try other structure
        (check (nullp pattern)
            (setq pattern (@ tokfile "pre_tokenizer" "pattern" "Regex")))
        (check (nullp pattern)
            (setq pattern "'s|'t|'re|'ve|'m|'ll|'d| ?\\p{L}+| ?\\p{N}+| ?[^\\s\\p{L}\\p{N}]+|\\s+(?!\\S)|\\s+"))
        (setq tokenizer (maybe
            (tiktoken_create (@ tokfile "model" "vocab") (@ tokfile "added_tokens") pattern)
            nil)))
    
    (if tokenizer
        (println "   ✓ Tokenizer loaded (" (tiktoken_vocab_size tokenizer) " tokens)")
        (println "   ⚠ Tokenizer not found: " tokenizer_path))
    
    ; Pre-dequantize embeddings (major optimization)
    (println "")
    (println "4. Pre-dequantizing embeddings...")
    (setq embed_name "model.embed_tokens")
    (setq scales_name (+ embed_name ".scales"))
    (setq cached_embeddings nil)
    
    (ife (get_tensor weights_list tensor_index scales_name)
        (block
            (setq embed_weight (get_tensor weights_list tensor_index (+ embed_name ".weight")))
            (setq embed_scales (get_tensor weights_list tensor_index scales_name))
            (setq embed_biases (maybe (get_tensor weights_list tensor_index (+ embed_name ".biases")) nil))
            (setq cached_embeddings (if embed_biases
                (mlx_dequantize embed_weight embed_scales embed_biases 64 8)
                (mlx_dequantize embed_weight embed_scales nil 64 8)))
            (println "   ✓ Embeddings dequantized and cached")
            (println "     Shape: " (mlx_shape cached_embeddings)))
        (setq cached_embeddings (get_tensor weights_list tensor_index (+ embed_name ".weight")))
        (println "   ✓ Non-quantized embeddings (no cache needed)"))
    
    ; Precompute RoPE frequencies with YaRN scaling
    (println "")
    (println "5. Precomputing YaRN RoPE frequencies...")
    (setq head_dim (@ config "head_dim"))
    (setq rope_theta (@ config "rope_theta"))
    (setq rope_scaling (@ config "rope_scaling"))
    
    ; YaRN parameters from config (with default values)
    (setq scaling_factor (select (key@ rope_scaling "factor") 1.0))
    (setq original_max_pos (select (key@ rope_scaling "original_max_position_embeddings") 4096))
    (setq beta_fast (select (key@ rope_scaling "beta_fast") 32.0))
    (setq beta_slow (select (key@ rope_scaling "beta_slow") 1.0))
    (setq mscale_param (select (key@ rope_scaling "mscale") 1.0))
    (setq mscale_all_dim (select (key@ rope_scaling "mscale_all_dim") 0.0))
    
    ; yarn_get_mscale(scale, ms) = 0.1 * ms * log(scale) + 1.0 if scale > 1, else 1.0
    (setq yarn_mscale_num (if (<= scaling_factor 1.0)
                               1.0
                               (+ (* 0.1 mscale_param (log scaling_factor)) 1.0)))
    (setq yarn_mscale_den (if (<= scaling_factor 1.0)
                               1.0
                               (+ (* 0.1 mscale_all_dim (log scaling_factor)) 1.0)))
    (setq yarn_mscale (/ yarn_mscale_num yarn_mscale_den))
    
    ; yarn_find_correction_dim:
    ; dim * log(original_max_pos / (num_rotations * 2 * pi)) / (2 * log(base))
    (setq log_theta (log rope_theta))
    
    ; Convert to floats to avoid integer truncation
    (setq beta_fast_f (number beta_fast))
    (setq beta_slow_f (number beta_slow))
    (setq head_dim_f (number head_dim))
    (setq original_max_pos_f (number original_max_pos))
    
    ; For beta_fast - detailed calculation
    (setq step1_fast (* beta_fast_f 2.0 _pi))
    (setq step2_fast (/ original_max_pos_f step1_fast))
    (setq step3_fast (log step2_fast))
    (setq step4_fast (* head_dim_f step3_fast))
    (setq step5_fast (* 2.0 log_theta))
    (setq low_dim_calc (/ step4_fast step5_fast))
    (setq low_dim (max 0 (floor low_dim_calc)))
    
    ; For beta_slow - detailed calculation
    (setq step1_slow (* beta_slow_f 2.0 _pi))
    (setq step2_slow (/ original_max_pos_f step1_slow))
    (setq step3_slow (log step2_slow))
    (setq step4_slow (* head_dim_f step3_slow))
    (setq high_dim_calc (/ step4_slow step5_fast))
    ; ceil(x) = floor(x) + (1 if x > floor(x), else 0)
    (setq high_dim_floor (floor high_dim_calc))
    (setq high_dim_ceil (if (> high_dim_calc high_dim_floor) (+ high_dim_floor 1) high_dim_floor))
    (setq high_dim (min (- head_dim 1) high_dim_ceil))
    
    ; Create linear mask: (arange(dim) - min) / (max - min), clipped to [0, 1]
    (setq half_dim (/ head_dim 2))
    (setq ramp_indices (mlx_arange 0 half_dim 1 "float32"))
    (setq min_val low_dim)
    (setq max_val (if (eq low_dim high_dim) (+ high_dim 0.001) high_dim))
    (setq linear_func (mlx_divide (mlx_subtract ramp_indices (mlx_array min_val))
                                   (mlx_array (- max_val min_val))))
    (setq freq_mask (mlx_subtract (mlx_array 1.0) (mlx_clip linear_func 0.0 1.0)))
    
    ; Compute frequencies
    ; freq_extra = base ** (arange(0, dims, 2) / dims)
    (setq indices (mlx_arange 0 head_dim 2 "float32"))
    (setq scaled_indices (mlx_divide indices (mlx_array (number head_dim))))
    (setq freq_extra (mlx_power (mlx_array rope_theta) scaled_indices))
    
    ; freq_inter = scaling_factor * base ** (arange(0, dims, 2) / dims)
    (setq freq_inter (mlx_multiply (mlx_array (number scaling_factor)) freq_extra))
    
    ; YaRN formula: (freq_inter * freq_extra) / (freq_inter * freq_mask + freq_extra * (1 - freq_mask))
    (setq one_minus_mask (mlx_subtract (mlx_array 1.0) freq_mask))
    (setq numerator (mlx_multiply freq_inter freq_extra))
    (setq denominator (mlx_add (mlx_multiply freq_inter freq_mask)
                               (mlx_multiply freq_extra one_minus_mask)))
    (setq rope_freqs (mlx_divide numerator denominator))

    (println "   ✓ YaRN RoPE frequencies precomputed")
    (println "     theta=" rope_theta ", factor=" scaling_factor ", mscale=" yarn_mscale)
    (println "     correction range: [" low_dim ", " high_dim "]")
    (println "     freqs shape: " (mlx_shape rope_freqs))
    
    ; Pre-extract tensors by layer
    (println "")
    (println "6. Pre-extracting tensors by layer...")
    (setq num_layers (@ config "num_hidden_layers"))
    (setq layer_tensors (build_layer_tensors weights_list tensor_index num_layers))
    (setq global_tensors (extract_global_tensors weights_list tensor_index))
    (setq cached_routers (build_cached_routers layer_tensors num_layers))
    (println "   ✓ " num_layers " layers pre-indexed")
    (println "   ✓ Global tensors extracted")
    (println "   ✓ Router matrices cached")
    
    ; Extract layer types (sliding/full)
    (setq layer_types (@ config "layer_types"))
    (println "   ✓ Layer types: " (size layer_types) " entries")
    
    ; Precompute attention parameters
    (println "")
    (println "7. Precomputing attention parameters...")
    (setq n_heads (@ config "num_attention_heads"))
    (setq n_kv_heads (@ config "num_key_value_heads"))
    (setq hidden_size (@ config "hidden_size"))
    (setq scale_float (* (/ 1.0 (sqrt head_dim)) yarn_mscale yarn_mscale))
    (setq eps (@ config "rms_norm_eps"))
    (setq repeats (/ n_heads n_kv_heads))
    (setq hidden_out (* n_heads head_dim))
    (setq sliding_window (@ config "sliding_window"))
    
    ; attn_params = (n_heads, n_kv_heads, head_dim, scale_float, eps, repeats, hidden_out, sliding_window)
    ; Use numbers because mix of int/float - more efficient than list for numeric values
    (setq attn_params (numbers n_heads n_kv_heads head_dim scale_float eps repeats hidden_out sliding_window))
    (println "   ✓ Attention parameters precomputed")
    (println "     n_heads=" n_heads ", n_kv_heads=" n_kv_heads ", head_dim=" head_dim)
    (println "     scale=" scale_float ", eps=" eps ", repeats=" repeats)
    (println "     sliding_window=" sliding_window)
    
    ; Create model object
    (setq native_decode_handle
        (mlx_decode_model_create cached_embeddings rope_freqs yarn_mscale
            layer_tensors global_tensors attn_params cached_routers
            (@ config "num_experts_per_tok") "gpt_oss_moe"))

    (setq model (MLXModel weights_list tensor_index config metadata tokenizer cached_embeddings rope_freqs yarn_mscale layer_tensors global_tensors attn_params nil layer_types 0 cached_routers native_decode_handle))
    
    ; Display stats
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
(println "║  MLX model loader for LispE                               ║")
(println "║  GPT-OSS 20B MoE 8-bit                                   ║")
(println "╚════════════════════════════════════════════════════════════╝")
(println "")

; Load the model
(setq chargement (elapse (setq model (load_mlx_model MODEL_PATH))))
(println "Loading time:" chargement)

(check (nullp model)
    (println "")
    (println "ERROR: The model could not be loaded")
    (exit 1))

; Display model information
(println "")
(model MLXModel (info))

; Display some tensors
(print_tensor_info (@ model 'tensor_index) 40)

; Example of weight access
(println "")
(println "Example of weight access:")
(println (fill "-" 40))

; Check if some tensors exist
(setq test_names (strings
    "model.embed_tokens.weight"
    "model.layers.0.self_attn.q_proj.weight"
    "model.layers.0.self_attn.sinks"
    "model.layers.0.mlp.router.weight"
    "model.layers.0.mlp.experts.gate_proj.weight"
    "model.norm.weight"
    "lm_head.weight"
))

(loop name test_names
    (if (model MLXModel (has_weight name))
        (println "  ✓ " name)
        (println "  ✗ " name " (not found)")))

; Tokenizer test
(println "")
(println "Tokenizer test:")
(println (fill "-" 40))
(check (model MLXModel (vocab_size))
    (setq test_text "Bonjour, comment ça va?")
    (setq tokens (model MLXModel (encode test_text)))
    (println "  Text: " test_text)
    (println "  Tokens: " tokens)
    (println "  Decoded: " (model MLXModel (decode tokens))))

; =============================================================================
; Simple forward pass test
; =============================================================================

; Checks for required weights
(defun test_weights(model)
    (println "")
    (println "Checking model weights...")
    (println (fill "-" 50))
    
    (setq key_weights (strings
        "model.embed_tokens.weight"
        "model.layers.0.self_attn.q_proj.weight"
        "model.layers.0.self_attn.q_proj.scales"
        "model.layers.0.self_attn.q_proj.bias"
        "model.layers.0.self_attn.sinks"
        "model.layers.0.mlp.router.weight"
        "model.layers.0.mlp.experts.gate_proj.weight"
        "model.layers.0.input_layernorm.weight"
        "model.norm.weight"
        "lm_head.weight"
    ))
    
    (loop name key_weights
        (if (model MLXModel (has_weight name))
            (block
                (setq w (model MLXModel (get_weight name)))
                (println "  ✓ " name " - shape: " (mlx_shape w)))
            (println "  ✗ " name " - NOT FOUND")))
    
    (println (fill "-" 50))
)

; =============================================================================
; Real generation test
; =============================================================================

; Generation test with a simple question
(defun test_generation(model (question "What is the capital of France?") (max_tokens 50) (temperature 0.7) (profile_decode false) (system_prompt DEFAULT_SYSTEM_PROMPT))
    (println "")
    (println (fill "=" 60))
    (println "GENERATION TEST")
    (println (fill "=" 60))
    (println "Question: " question)
    (println "Max tokens: " max_tokens)
    (println "Temperature: " temperature)
    (println "")
    
    ; Check that the model is valid
    (check (nullp model)
        (println "ERROR: Model not loaded")
        (return nil))
    
    ; Check tokenizer
    (check (not (model MLXModel (vocab_size)))
        (println "ERROR: Tokenizer not available")
        (return nil))
    
    ; Format the prompt for chat
    (setq prompt (model MLXModel (format_prompt question system_prompt)))
    (println "Formatted prompt:")
    (println prompt)
    (println (fill "-" 60))
    
    ; Generate the answer
    (println "")
    (println "Model answer:")
    (println "")
    
    ; Measure time
    (setq elapsed (elapse (setq response (model MLXModel (chat question max_tokens temperature system_prompt false profile_decode)))))
    
    (println "")
    (println (fill "-" 60))
    (println "Generation time: " elapsed " ms")
    (print_memory_stats))

; Same generation test, but with aggregated native decode profiling enabled.
(defun test_generation_profile(model (question "What is the capital of France?") (max_tokens 50) (temperature 0.7) (system_prompt DEFAULT_SYSTEM_PROMPT))
    (test_generation model question max_tokens temperature true system_prompt))

(defun compare_decode_backends(model (question "Write a short story about a robot who learns to paint.") (max_tokens 1000) (temperature 0.0))
    (println "")
    (println (fill "=" 60))
    (println "COMPARE DECODE PATHS")
    (println (fill "=" 60))
    (println "Question: " question)
    (println "Max tokens: " max_tokens)

    (setq old_full_forward USE_FULL_FORWARD_FOR_DECODE)
    (setq old_native_decode USE_NATIVE_DECODE)
    (setq old_native_kv USE_NATIVE_KV_CACHE)

    (setg USE_FULL_FORWARD_FOR_DECODE nil)
    (setg USE_NATIVE_DECODE true)
    (setg USE_NATIVE_KV_CACHE true)
    (println "")
    (println "[1/3] Native decode backend")
    (test_generation model question max_tokens temperature false BENCHMARK_SYSTEM_PROMPT)

    (setg USE_FULL_FORWARD_FOR_DECODE nil)
    (setg USE_NATIVE_DECODE nil)
    (setg USE_NATIVE_KV_CACHE true)
    (println "")
    (println "[2/3] Generic MLX decode step backend")
    (test_generation model question max_tokens temperature false BENCHMARK_SYSTEM_PROMPT)

    (setg USE_FULL_FORWARD_FOR_DECODE true)
    (setg USE_NATIVE_DECODE nil)
    (setg USE_NATIVE_KV_CACHE true)
    (println "")
    (println "[3/3] Full forward decode backend")
    (test_generation model question max_tokens temperature false BENCHMARK_SYSTEM_PROMPT)

    (setg USE_FULL_FORWARD_FOR_DECODE old_full_forward)
    (setg USE_NATIVE_DECODE old_native_decode)
    (setg USE_NATIVE_KV_CACHE old_native_kv)
    (println "")
    (println "Decode backend flags restored"))

(defun compare_kv_cache_backends(model (question "Write a short story about a robot who learns to paint.") (max_tokens 1000) (temperature 0.0))
    (println "")
    (println (fill "=" 60))
    (println "COMPARE KV CACHE BACKENDS")
    (println (fill "=" 60))
    (println "Question: " question)
    (println "Max tokens: " max_tokens)

    (setq old_full_forward USE_FULL_FORWARD_FOR_DECODE)
    (setq old_native_decode USE_NATIVE_DECODE)
    (setq old_native_kv USE_NATIVE_KV_CACHE)

    (setg USE_FULL_FORWARD_FOR_DECODE true)
    (setg USE_NATIVE_DECODE nil)
    (setg USE_NATIVE_KV_CACHE true)
    (println "")
    (println "[1/2] Native KV cache backend (full_forward)")
    (test_generation model question max_tokens temperature false BENCHMARK_SYSTEM_PROMPT)

    (setg USE_FULL_FORWARD_FOR_DECODE true)
    (setg USE_NATIVE_DECODE nil)
    (setg USE_NATIVE_KV_CACHE nil)
    (println "")
    (println "[2/2] LispE KV cache backend (full_forward)")
    (test_generation model question max_tokens temperature false BENCHMARK_SYSTEM_PROMPT)

    (setg USE_FULL_FORWARD_FOR_DECODE old_full_forward)
    (setg USE_NATIVE_DECODE old_native_decode)
    (setg USE_NATIVE_KV_CACHE old_native_kv)
    (println "")
    (println "KV cache backend flags restored"))

; =============================================================================
; Main program - Test
; =============================================================================

(println "")
(println "The model is ready to use!")
(println "Variable 'model' contains the MLXModel object")
(println "")
(println "Available methods:")
(println "  - (model (encode \"text\"))     ; Tokenizer")
(println "  - (model (decode tokens))      ; Detokenizer")
(println "  - (model (get_weight \"name\"))  ; Weight access")
(println "  - (model (info))               ; Model information")
(println "  - (model (reset_cache))        ; Reset KV cache")
(println "")
(println "Text generation:")
(println "  - (model (chat \"message\"))                    ; Simple chat")
(println "  - (model (chat \"message\" 256 0.7 \"system\"))  ; Chat with options")
(println "  - (model (generate \"prompt\" max_tokens temp)) ; Raw generation")
(println "")
(println "Test functions:")
(println "  - (test_weights model)         ; Check weights")
(println "  - (test_generation model)      ; Real generation test")
(println "  - (test_generation_profile model) ; Same test with decode profiling")
(println "  - (compare_decode_backends model) ; Native vs generic decode benchmark")
(println "  - (compare_kv_cache_backends model) ; Native vs LispE KV cache benchmark")
(println "")
(println "Example:")
(println "  (model (chat \"Hello, who are you?\"))")
(println "")

; =============================================================================
; Generation test
; =============================================================================

; Run default test only when executed directly (not loaded from another script)
(check (not (boundp bench_max_tokens))
    (println "")
    (println "Starting test...")
    (test_generation model "Write a short story about a robot who learns to paint." 
        (? max_tokens 1000) (? temperature 0.7)))
;(compare_decode_backends model "Write a short story about a robot who learns to paint." 1000)
;(compare_kv_cache_backends model "Write a short story about a robot who learns to paint." 1000)

; (defun test_generation(model (question "?") (max_tokens 50) (temperature 0.7) (profile_decode false))