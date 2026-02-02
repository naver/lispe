; =============================================================================
; Loading a sharded MLX Safetensors model (Gemma 3 27B)
; =============================================================================
; This script loads a safetensors model with multiple files
; (shards) and combines all weights into a unified dictionary.
;
; Compatible with MLX models from mlx-community on HuggingFace
; =============================================================================

(use 'lispe_mlx)
(use 'lispe_torch)  ; For SentencePiece tokenizer

; =============================================================================
; Model configuration
; =============================================================================

(setq MODEL_PATH "/Users/user/.lmstudio/models/mlx-community/gemma-3-27b-it-qat-4bit")

; =============================================================================
; Function to generate sharded safetensors file paths
; =============================================================================

; Generates safetensors file paths for num_shards files
(defun get_safetensors_files(dir_path num_shards)
    (setq files (list))
    (setq num_str (string num_shards))
    (setq pad_len (size num_str))

    (loopcount num_shards i
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

    (println "Generation of p aths for " num_shards " safetensors files...")
    (setq files (get_safetensors_files dir_path num_shards))
    (setq num_files (size files))

    (check (zerop num_files)
        (println "Error: No safetensors files found!")
        (return nil))

    (println "Found " num_files " safetensors files")

    ; List to store all tensors (index access = O(1))
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
        (println "  → " num_tensors " loaded tensors")

        ; Add tensors to the list and create the index
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
    (println "Loading finished!")
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
            (println "Warning: config.json not found")
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
    (println "Information about tensors:")
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
        (println "  ... et " (- total max_display) " other tensors"))

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
(defun get_tensor(weights_list tensor_index name)
    (setq idx (@ tensor_index name))
    (if idx (@ weights_list idx) nil))

; Pre-extracts a layer's tensors into an ordered list
; Returns: (input_norm q_w q_s q_b k_w k_s k_b v_w v_s v_b o_w o_s o_b q_norm k_norm
;           post_attn_norm pre_ff_norm gate_w gate_s gate_b up_w up_s up_b down_w down_s down_b post_ff_norm)
(defun extract_layer_tensors(weights_list tensor_index layer_idx)
    (setq prefix (+ "language_model.model.layers." (string layer_idx) "."))
    (setq attn (+ prefix "self_attn."))
    (setq mlp (+ prefix "mlp."))

    (list
        ; 0: input_layernorm
        (get_tensor weights_list tensor_index (+ prefix "input_layernorm.weight"))
        ; 1-3: q_proj (weight, scales, biases)
        (get_tensor weights_list tensor_index (+ attn "q_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "q_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "q_proj.biases"))
        ; 4-6: k_proj
        (get_tensor weights_list tensor_index (+ attn "k_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "k_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "k_proj.biases"))
        ; 7-9: v_proj
        (get_tensor weights_list tensor_index (+ attn "v_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "v_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "v_proj.biases"))
        ; 10-12: o_proj
        (get_tensor weights_list tensor_index (+ attn "o_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "o_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "o_proj.biases"))
        ; 13-14: q_norm, k_norm
        (get_tensor weights_list tensor_index (+ attn "q_norm.weight"))
        (get_tensor weights_list tensor_index (+ attn "k_norm.weight"))
        ; 15-17: post_attention_layernorm, pre_feedforward_layernorm, post_feedforward_layernorm
        (get_tensor weights_list tensor_index (+ prefix "post_attention_layernorm.weight"))
        (get_tensor weights_list tensor_index (+ prefix "pre_feedforward_layernorm.weight"))
        (get_tensor weights_list tensor_index (+ prefix "post_feedforward_layernorm.weight"))
        ; 18-20: gate_proj
        (get_tensor weights_list tensor_index (+ mlp "gate_proj.weight"))
        (get_tensor weights_list tensor_index (+ mlp "gate_proj.scales"))
        (get_tensor weights_list tensor_index (+ mlp "gate_proj.biases"))
        ; 21-23: up_proj
        (get_tensor weights_list tensor_index (+ mlp "up_proj.weight"))
        (get_tensor weights_list tensor_index (+ mlp "up_proj.scales"))
        (get_tensor weights_list tensor_index (+ mlp "up_proj.biases"))
        ; 24-26: down_proj
        (get_tensor weights_list tensor_index (+ mlp "down_proj.weight"))
        (get_tensor weights_list tensor_index (+ mlp "down_proj.scales"))
        (get_tensor weights_list tensor_index (+ mlp "down_proj.biases"))
    ))

; Creates the tensor list for all layers
(defun build_layer_tensors(weights_list tensor_index num_layers)
    (setq layers (list))
    (loopcount num_layers i
        (push layers (extract_layer_tensors weights_list tensor_index i)))
    layers)

; Extracts global tensors (embed, lm_head, final_norm)
(defun extract_global_tensors(weights_list tensor_index)
    (list
        ; 0-2: embed_tokens
        (get_tensor weights_list tensor_index "language_model.model.embed_tokens.weight")
        (get_tensor weights_list tensor_index "language_model.model.embed_tokens.scales")
        (get_tensor weights_list tensor_index "language_model.model.embed_tokens.biases")
        ; 3: final norm
        (get_tensor weights_list tensor_index "language_model.model.norm.weight")
        ; 4-6: lm_head
        (get_tensor weights_list tensor_index "language_model.lm_head.weight")
        (get_tensor weights_list tensor_index "language_model.lm_head.scales")
        (get_tensor weights_list tensor_index "language_model.lm_head.biases")
    ))

; =============================================================================
; Model structure
; =============================================================================

; Index constants for layer tensors (defined before the class)
(setq L_INPUT_NORM 0)      ; input_layernorm.weight
(setq L_Q_WEIGHT 1)        ; q_proj.weight
(setq L_Q_SCALES 2)        ; q_proj.scales
(setq L_Q_BIASES 3)        ; q_proj.biases
(setq L_K_WEIGHT 4)        ; k_proj.weight
(setq L_K_SCALES 5)        ; k_proj.scales
(setq L_K_BIASES 6)        ; k_proj.biases
(setq L_V_WEIGHT 7)        ; v_proj.weight
(setq L_V_SCALES 8)        ; v_proj.scales
(setq L_V_BIASES 9)        ; v_proj.biases
(setq L_O_WEIGHT 10)       ; o_proj.weight
(setq L_O_SCALES 11)       ; o_proj.scales
(setq L_O_BIASES 12)       ; o_proj.biases
(setq L_Q_NORM 13)         ; q_norm.weight
(setq L_K_NORM 14)         ; k_norm.weight
(setq L_POST_ATTN_NORM 15) ; post_attention_layernorm.weight
(setq L_PRE_FF_NORM 16)    ; pre_feedforward_layernorm.weight
(setq L_POST_FF_NORM 17)   ; post_feedforward_layernorm.weight
(setq L_GATE_WEIGHT 18)    ; gate_proj.weight
(setq L_GATE_SCALES 19)    ; gate_proj.scales
(setq L_GATE_BIASES 20)    ; gate_proj.biases
(setq L_UP_WEIGHT 21)      ; up_proj.weight
(setq L_UP_SCALES 22)      ; up_proj.scales
(setq L_UP_BIASES 23)      ; up_proj.biases
(setq L_DOWN_WEIGHT 24)    ; down_proj.weight
(setq L_DOWN_SCALES 25)    ; down_proj.scales
(setq L_DOWN_BIASES 26)    ; down_proj.biases

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
(setq TRANSPOSE_0132 (integers 0 1 3 2))

; KV cache constants
(setq KV_TYPE 0)
(setq KV_KEYS 1)
(setq KV_VALUES 2)
(setq KV_MAX_SIZE 3)
(setq KV_OFFSET 4)
(setq CACHE_STANDARD 0)
(setq CACHE_ROTATING 1)

; Represents a loaded MLX model with its weights, config, and tokenizer
; Attributes:
;   weights: list of tensors (O(1) index access)
;   tensor_index: dictionary name -> index
;   config: model configuration
;   metadata: safetensors metadata
;   tokenizer: SentencePiece tokenizer
;   cached_embeddings: pre-dequantized embeddings
;   rope_freqs_global/local: precomputed RoPE frequencies
;   layer_tensors: pre-extracted tensors by layer
;   global_tensors: global tensors (embed, lm_head, norm)
;   attn_params: precomputed attention parameters
;   kv_caches: KV caches for generation (created on demand)
(class@ MLXModel (weights tensor_index config metadata tokenizer cached_embeddings rope_freqs_global rope_freqs_local layer_tensors global_tensors attn_params kv_caches)

    ; =========================================================================
    ; Basic accessors
    ; =========================================================================

    ; Retrieve a tensor by name
    (defun get_weight(name)
        (setq idx (@ tensor_index name))
        (check (nullp idx)
            (println "Warning: tensor not found: " name)
            (return nil))
        (@ weights idx))

    ; Check if a tensor exists
    (defun has_weight(name)
        (neq (@ tensor_index name) nil))

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

    ; Retrieve a config value
    (defun get_config(key (default nil))
        (select (@ config key) default))

    ; Retrieve text_config
    (defun text_config()
        (@ config "text_config"))

    ; Display model information
    (defun info()
        (println "Model MLX:")
        (println "  tensors: " (num_weights))
        (println "  Type: " (get_config "model_type" "inconnu"))
        (println "  Architecture: " (get_config "architectures" "inconnue"))
        (check tokenizer
            (println "  Vocabulary: " (torch_vocab_size tokenizer) " tokens")))

    ; =========================================================================
    ; Tokenizer
    ; =========================================================================

    ; Encode text into tokens
    (defun encode(text)
        (check (not tokenizer)
            (println "ERREUR: Tokenizer non chargé")
            (return nil))
        (torch_encode tokenizer text))

    ; Decode tokens into text
    (defun decode(tokens)
        (check (not tokenizer)
            (println "ERREUR: Tokenizer non chargé")
            (return nil))
        (setq ttype (type tokens))
        (if (or (== ttype "list_") (== ttype "integers_"))
            (torch_decode tokenizer (torch_tensor tokens "long"))
            (torch_decode tokenizer tokens)))

    ; Return vocabulary size
    (defun vocab_size()
        (check (not tokenizer)
            (return 0))
        (torch_vocab_size tokenizer))

    ; =========================================================================
    ; Internal utility functions
    ; =========================================================================

    ; Linear projection with quantized or non-quantized weights
    (defun _linear(x weight scales biases (transpose_weight true))
        (ife scales
            (if biases
                (mlx_quantized_matmul x weight scales biases transpose_weight 64 4)
                (mlx_quantized_matmul x weight scales nil transpose_weight 64 4))
            (if transpose_weight
                (mlx_matmul x . mlx_transpose weight)
                (mlx_matmul x weight))))

    ; RMSNorm with Gemma style (1 + weight)
    (defun _rms_norm(x weight eps)
        (mlx_rms_norm x weight eps true))

    ; Apply RoPE to queries/keys
    (defun _apply_rope(x freqs offset)
        (setq shape (mlx_shape x))
        (setq B (@ shape 0))
        (setq n_heads (@ shape 1))
        (setq L (@ shape 2))
        (setq head_dim (@ shape 3))
        (setq half_dim (/ head_dim 2))

        (setq x1 (mlx_slice x (integers 0 0 0 0) (integers B n_heads L half_dim)))
        (setq x2 (mlx_slice x (integers 0 0 0 half_dim) (integers B n_heads L head_dim)))

        (setq positions (mlx_arange offset (+ offset L) 1 "float32"))
        (setq pos_reshaped (mlx_reshape positions (integers 1 1 L 1)))
        (setq freqs_reshaped (mlx_reshape freqs (integers 1 1 1 half_dim)))
        (setq angles (mlx_multiply pos_reshaped freqs_reshaped))

        (setq cos_angles (mlx_cos angles))
        (setq sin_angles (mlx_sin angles))

        (setq out1 (mlx_subtract (mlx_multiply x1 cos_angles) . mlx_multiply x2 sin_angles))
        (setq out2 (mlx_add (mlx_multiply x1 sin_angles) . mlx_multiply x2 cos_angles))

        (mlx_concatenate (list out1 out2) -1))

    ; =========================================================================
    ; KV Cache
    ; =========================================================================

    ; Create a standard KV cache
    (defun _make_kv_cache_standard()
        (dictionaryi KV_TYPE CACHE_STANDARD KV_KEYS nil KV_VALUES nil KV_OFFSET 0))

    ; Create a rotary KV cache
    (defun _make_kv_cache_rotating(max_size)
        (dictionaryi KV_TYPE CACHE_ROTATING KV_KEYS nil KV_VALUES nil KV_MAX_SIZE max_size KV_OFFSET 0))

    ; Create all KV caches
    (defun _make_all_kv_caches()
        (setq tc (text_config))
        (setq num_layers (@ tc "num_hidden_layers"))
        (setq pattern (@ tc "sliding_window_pattern"))
        (setq window_size (@ tc "sliding_window"))
        (setq caches (list))
        (loopcount num_layers i
            (if (zerop (% (+ i 1) pattern))
                (push caches (_make_kv_cache_standard))
                (push caches (_make_kv_cache_rotating window_size))))
        caches)

    ; Update a standard cache
    (defun _update_kv_cache_standard(cache keys values)
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

    ; Update a rotary cache
    (defun _update_kv_cache_rotating(cache keys values)
        (setq cached_keys (@ cache KV_KEYS))
        (setq cached_values (@ cache KV_VALUES))
        (setq max_size (@ cache KV_MAX_SIZE))
        (setq current_offset (@ cache KV_OFFSET))
        (setq num_new (@ (mlx_shape keys) 2))
        (ife cached_keys
            (block
                (setq new_keys (mlx_concatenate (list cached_keys keys) 2))
                (setq new_values (mlx_concatenate (list cached_values values) 2))
                (setq total_len (@ (mlx_shape new_keys) 2))
                (check (> total_len max_size)
                    (setq start_idx (- total_len max_size))
                    (setq B (@ (mlx_shape new_keys) 0))
                    (setq n_kv_heads (@ (mlx_shape new_keys) 1))
                    (setq head_dim (@ (mlx_shape new_keys) 3))
                    (setq new_keys (mlx_slice new_keys (integers 0 0 start_idx 0) (list B n_kv_heads total_len head_dim)))
                    (setq new_values (mlx_slice new_values (integers 0 0 start_idx 0) (list B n_kv_heads total_len head_dim)))))
            (setq new_keys keys)
            (setq new_values values))
        (set@ cache KV_KEYS new_keys)
        (set@ cache KV_VALUES new_values)
        (set@ cache KV_OFFSET (+ current_offset num_new))
        (list new_keys new_values))

    ; Update a cache (dispatch by type)
    (defun _update_kv_cache(cache keys values)
        (if (eq (@ cache KV_TYPE) CACHE_ROTATING)
            (_update_kv_cache_rotating cache keys values)
            (_update_kv_cache_standard cache keys values)))

    ; Reset KV caches
    (defun reset_cache()
        (setqi kv_caches nil))

    ; =========================================================================
    ; Forward pass - Components
    ; =========================================================================

    ; MLP forward
    (defun _mlp(x lt)
        (setq gate (_linear x (@ lt L_GATE_WEIGHT) (@ lt L_GATE_SCALES) (@ lt L_GATE_BIASES)))
        (setq up (_linear x (@ lt L_UP_WEIGHT) (@ lt L_UP_SCALES) (@ lt L_UP_BIASES)))
        (setq hidden (mlx_multiply (mlx_gelu_tanh gate) up))
        (_linear hidden (@ lt L_DOWN_WEIGHT) (@ lt L_DOWN_SCALES) (@ lt L_DOWN_BIASES)))

    ; Attention forward
    (defun _attention(x lt offset kv_cache layer_idx)
        (setq n_heads (@ attn_params 0))
        (setq n_kv_heads (@ attn_params 1))
        (setq head_dim (@ attn_params 2))
        (setq scale (@ attn_params 3))
        (setq eps (@ attn_params 4))
        (setq repeats (@ attn_params 5))
        (setq hidden_out (@ attn_params 6))

        (setq tc (text_config))
        (setq shape (mlx_shape x))
        (setq B (@ shape 0))
        (setq L (@ shape 1))

        ; QKV projections
        (setq queries (_linear x (@ lt L_Q_WEIGHT) (@ lt L_Q_SCALES) (@ lt L_Q_BIASES)))
        (setq keys (_linear x (@ lt L_K_WEIGHT) (@ lt L_K_SCALES) (@ lt L_K_BIASES)))
        (setq values (_linear x (@ lt L_V_WEIGHT) (@ lt L_V_SCALES) (@ lt L_V_BIASES)))

        ; Reshape
        (setq queries (mlx_transpose (mlx_reshape queries (integers B L n_heads head_dim)) TRANSPOSE_0213))
        (setq keys (mlx_transpose (mlx_reshape keys (integers B L n_kv_heads head_dim)) TRANSPOSE_0213))
        (setq values (mlx_transpose (mlx_reshape values (integers B L n_kv_heads head_dim)) TRANSPOSE_0213))

        ; QK Norm
        (setq queries (_rms_norm queries (@ lt L_Q_NORM) eps))
        (setq keys (_rms_norm keys (@ lt L_K_NORM) eps))

        ; RoPE
        (setq is_sliding (not (zerop (% (+ layer_idx 1) (@ tc "sliding_window_pattern")))))
        (setq rope_freqs (if is_sliding rope_freqs_local rope_freqs_global))
        (setq queries (_apply_rope queries rope_freqs offset))
        (setq keys (_apply_rope keys rope_freqs offset))

        ; KV Cache
        (check kv_cache
            (setq kv_result (_update_kv_cache kv_cache keys values))
            (setq keys (@ kv_result 0))
            (setq values (@ kv_result 1)))

        ; GQA expansion
        (setq keys_expanded keys)
        (setq values_expanded values)
        (check (> repeats 1)
            (setq keys_expanded (mlx_repeat keys repeats 1))
            (setq values_expanded (mlx_repeat values repeats 1)))

        ; Attention scores
        (setq scale_arr (mlx_array scale))
        (setq attention_scores (mlx_multiply (mlx_matmul queries . mlx_transpose keys_expanded TRANSPOSE_0132) scale_arr))

        ; Mask
        (setq L_kv (@ (mlx_shape keys) 2))
        (setq window_size (@ tc "sliding_window"))

        (ife (> L 1)
            (block
                (setq linds (mlx_reshape (mlx_arange 0 L 1 "int32") (list L 1)))
                (setq rinds (mlx_reshape (mlx_arange 0 L_kv 1 "int32") (list 1 L_kv)))
                (setq causal_mask (mlx_greater_equal linds rinds))
                (check is_sliding
                    (setq window_mask (mlx_less linds . mlx_add rinds . mlx_array window_size))
                    (setq causal_mask (mlx_logical_and causal_mask window_mask)))
                (setq mask (mlx_where causal_mask (mlx_zeros (list L L_kv) "float32") . mlx_array -10000.0))
                (setq attention_scores (mlx_add attention_scores . mlx_reshape mask . integers 1 1 L L_kv)))
            (check (and is_sliding (> L_kv window_size))
                (setq visible_start (- L_kv window_size))
                (setq positions (mlx_arange 0 L_kv 1 "int32"))
                (setq window_mask (mlx_greater_equal positions . mlx_array visible_start))
                (setq mask (mlx_where window_mask (mlx_zeros (list L_kv) "float32") . mlx_array -10000.0))
                (setq attention_scores (mlx_add attention_scores . mlx_reshape mask . integers 1 1 1 L_kv))))

        ; Output
        (setq attention_output (mlx_matmul (mlx_softmax attention_scores -1) values_expanded))
        (setq attention_output (mlx_reshape (mlx_transpose attention_output TRANSPOSE_0213) (integers B L hidden_out)))
        (_linear attention_output (@ lt L_O_WEIGHT) (@ lt L_O_SCALES) (@ lt L_O_BIASES)))

    ; Transformer block
    (defun _transformer_block(x lt offset kv_cache layer_idx)
        (setq eps (@ attn_params 4))

        (setq normed_x (_rms_norm x (@ lt L_INPUT_NORM) eps))
        (setq attn_output (_attention normed_x lt offset kv_cache layer_idx))

        (setq attn_normed (_rms_norm attn_output (@ lt L_POST_ATTN_NORM) eps))
        (setq h (mlx_add x attn_normed))

        (setq normed_h (_rms_norm h (@ lt L_PRE_FF_NORM) eps))
        (setq mlp_output (_mlp normed_h lt))

        (setq mlp_normed (_rms_norm mlp_output (@ lt L_POST_FF_NORM) eps))
        (mlx_add h mlp_normed))

    ; =========================================================================
    ; Full forward pass
    ; =========================================================================

    ; Forward pass - returns (logits, kv_caches)
    (defun forward(input_ids offset)
        (setq tc (text_config))
        (setq num_layers (@ tc "num_hidden_layers"))
        (setq eps (@ attn_params 4))
        (setq hidden_size (@ tc "hidden_size"))

        ; Embedding
        (setq embeddings (mlx_take cached_embeddings input_ids 0))
        (setq hidden_states (mlx_multiply embeddings (mlx_array (sqrt hidden_size))))

        ; Create KV caches if needed
        (check (nullp kv_caches)
            (setqi kv_caches (_make_all_kv_caches)))

        ; Pass through all layers
        (loopcount num_layers i
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

    ; =========================================================================
    ; Text generation
    ; =========================================================================

    ; Format a chat prompt
    (defun format_prompt(user_message (system_prompt ""))
        (setq prompt "")
        (check (not (eq system_prompt ""))
            (setq prompt (+ prompt "<start_of_turn>system\n" system_prompt "<end_of_turn>\n")))
        (+= prompt "<start_of_turn>user\n")
        (+= prompt user_message)
        (+= prompt "<end_of_turn>\n<start_of_turn>model\n")
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

    ; Generate text from a prompt
    (defun generate(prompt_text max_new_tokens (temperature 0.7))
        (println "")
        (println "Generation on...")
        (println "Prompt: \"" prompt_text "\"")
        (println "")

        ; Reset cache
        (reset_cache)

        ; Encode prompt
        (setq input_ids_tensor (encode prompt_text))
        (setq input_ids (tensor_to_list input_ids_tensor))

        ; Add BOS if missing
        (setq BOS_TOKEN 2)
        (check (not (eq (@ input_ids 0) BOS_TOKEN))
            (setq input_ids (cons BOS_TOKEN input_ids)))

        ; Convert to MLX array
        (setq num_tokens (size input_ids))
        (setq input_tensor (mlx_array input_ids nil "int32"))
        (setq input_tensor (mlx_reshape input_tensor (integers 1 num_tokens)))

        ; List to store generated tokens
        (setq generated_tokens input_ids)
        (setq num_generated 0)

        ; Measure time
        (setq gen_time (elapse
            (loopcount max_new_tokens i
                (setq offset (- (size generated_tokens) (@ (mlx_shape input_tensor) 1)))

                ; Forward
                (setq logits (forward input_tensor offset))

                ; Last token
                (setq last_idx (- (@ (mlx_shape logits) 1) 1))
                (setq last_logits (mlx_take logits (mlx_array (integers last_idx) nil "int32") 1))

                ; Sample
                (setq next_token (_sample last_logits temperature))
                (setq next_token (mlx_eval next_token))

                ; Convert to int
                (setq next_token_flat (mlx_flatten next_token))
                (setq next_token_id (integer (@ next_token_flat 0)))

                ; Stream output
                (printerr (decode (integers next_token_id)))
                (+= num_generated 1)

                ; Check end
                (check (or (eq next_token_id 1) (eq next_token_id 107))
                    (break))

                ; Add and prepare next
                (push generated_tokens next_token_id)
                (setq input_tensor (mlx_reshape (mlx_array (integers next_token_id) nil "int32") (integers 1 1)))
            )))

        ; Stats
        (setq tokens_per_sec (/ (* num_generated 1000.0) gen_time))
        (println "")
        (println (fill "=" 60))
        (println "Tokens: " num_generated " en " (/ gen_time 1000.0) " sec")
        (println "Speed: " tokens_per_sec " tokens/sec")
        (println (fill "=" 60))

        ; Decode
        (setq output_text (decode generated_tokens))
        (println "")
        (println "Generated Text:")
        (println output_text)
        (println (fill "=" 60))

        output_text)

    ; Simple chat interface
    (defun chat(user_message (max_tokens 256) (temperature 0.7) (system_prompt ""))
        (setq formatted_prompt (format_prompt user_message system_prompt))
        (generate formatted_prompt max_tokens temperature))
)

; =============================================================================
; Main loading function
; =============================================================================

; Loads a full MLX model (sharded safetensors + config)
(defun load_mlx_model(model_path)

    (println (fill "=" 60))
    (println "Loading MLX model")
    (println "Path: " model_path)
    (println (fill "=" 60))

    ; Load configuration
    (println "")
    (println "1. Loading Configuration...")
    (setq config (load_model_config model_path))
    (println "   ✓ Configuration loaded")

    ; Display some config info
    (check (in config "model_type")
        (println "   Type: " (@ config "model_type")))
    (check (in config "architectures")
        (println "   Architecture: " (@ config "architectures")))

    ; Load weights (4 shards for Gemma 3 27B)
    (println "")
    (println "2. Loading Weights...")
    (setq result (load_sharded_safetensors model_path 4))

    (check (nullp result)
        (println "Error: Failed to load weights")
        (return nil))

    (setq weights_list (@ result 0))
    (setq tensor_index (@ result 1))
    (setq metadata (@ result 2))

    ; Load SentencePiece tokenizer
    (println "")
    (println "3. Loading tokenizer...")
    (setq tokenizer_path (+ model_path "/tokenizer.model"))
    (setq tokenizer (maybe
        (torch_sentencepiece_tokenizer tokenizer_path)
        nil))
    (if tokenizer
        (println "   ✓ Tokenizer loaded (" (torch_vocab_size tokenizer) " tokens)")
        (println "   ⚠ Tokenizer not found: " tokenizer_path))

    ; Pre-dequantize embeddings (major optimization)
    (println "")
    (println "4. Pre-dequantification of embeddings...")
    (setq embed_name "language_model.model.embed_tokens")
    (setq scales_name (+ embed_name ".scales"))
    (setq cached_embeddings nil)

    (ife (get_tensor weights_list tensor_index scales_name)
        (block
            (setq embed_weight (get_tensor weights_list tensor_index (+ embed_name ".weight")))
            (setq embed_scales (get_tensor weights_list tensor_index scales_name))
            (setq embed_biases (maybe (get_tensor weights_list tensor_index (+ embed_name ".biases")) nil))
            (setq cached_embeddings (if embed_biases
                (mlx_dequantize embed_weight embed_scales embed_biases 64 4)
                (mlx_dequantize embed_weight embed_scales nil 64 4)))
            (println "   ✓ Embeddings dequantified and in cache")
            (println "     Shape: " (mlx_shape cached_embeddings)))
        (setq cached_embeddings (get_tensor weights_list tensor_index (+ embed_name ".weight")))
        (println "   ✓ Embeddings not quantified (no cache needed)"))

    ; Precompute RoPE frequencies (optimization)
    (println "")
    (println "5. Pre-computing of RoPE frequencies...")
    (setq text_config (@ config "text_config"))
    (setq head_dim (@ text_config "head_dim"))
    (setq rope_theta (@ text_config "rope_theta"))
    (setq rope_local_base (@ text_config "rope_local_base_freq"))

    ; Compute inv_freq for both bases
    ; inv_freq = 1.0 / (base ** (arange(0, head_dim, 2) / head_dim))
    ; Note: rope_scaling with factor=8.0 is already integrated in rope_theta=1000000
    ; (larger base = lower frequencies = positions that vary more slowly)
    (setq indices (mlx_arange 0 head_dim 2 "float32"))
    (setq scaled (mlx_divide indices (mlx_array head_dim)))

    (setq rope_freqs_global (mlx_reciprocal (mlx_power (mlx_array rope_theta) scaled)))
    (setq rope_freqs_local (mlx_reciprocal (mlx_power (mlx_array rope_local_base) scaled)))

    (println "   ✓ RoPE Frequencies pre-computed")
    (println "     Global (theta=" rope_theta "): " (mlx_shape rope_freqs_global))
    (println "     Local (base=" rope_local_base "): " (mlx_shape rope_freqs_local))

    ; Pre-extract tensors by layer (O(1) access optimization)
    (println "")
    (println "6. Pre-extraction of tensors by layer...")
    (setq num_layers (@ text_config "num_hidden_layers"))
    (setq layer_tensors (build_layer_tensors weights_list tensor_index num_layers))
    (setq global_tensors (extract_global_tensors weights_list tensor_index))
    (println "   ✓ " num_layers " pre-indexed layers")
    (println "   ✓ Global tensors extracted")

    ; Precompute attention parameters (major optimization - avoids 62 config accesses per token)
    (println "")
    (println "7. Precompute attention parameters...")
    (setq n_heads (@ text_config "num_attention_heads"))
    (setq n_kv_heads (@ text_config "num_key_value_heads"))
    (setq hidden_size (@ text_config "hidden_size"))
    (setq query_pre_attn_scalar (@ text_config "query_pre_attn_scalar"))
    (setq scale_float (/ 1.0 (sqrt query_pre_attn_scalar)))  ; float pour sdpa
    (setq eps (@ text_config "rms_norm_eps"))
    (setq repeats (/ n_heads n_kv_heads))
    (setq hidden_out (* n_heads head_dim))

    ; attn_params = (n_heads, n_kv_heads, head_dim, scale_float, eps, repeats, hidden_out)
    ; Shapes (B, L, ...) are computed dynamically as L changes between prompt and generation
    (setq attn_params (list n_heads n_kv_heads head_dim scale_float eps repeats hidden_out))
    (println "   ✓ Precomputed attention parameters")
    (println "     n_heads=" n_heads ", n_kv_heads=" n_kv_heads ", head_dim=" head_dim)
    (println "     scale=" scale_float ", eps=" eps ", repeats=" repeats)

    ; Create the model object (kv_caches initialized to nil, created on demand)
    (setq model (MLXModel weights_list tensor_index config metadata tokenizer cached_embeddings rope_freqs_global rope_freqs_local layer_tensors global_tensors attn_params nil))

    ; Display stats
    (print_memory_stats)

    (println "")
    (println "✓ Model sucessfully loaded!")

    model
)

; =============================================================================
; Main program
; =============================================================================

(println "")
(println "╔════════════════════════════════════════════════════════════╗")
(println "║  Loader of MLX model for LispE                         ║")
(println "║  Gemma 3 27B IT QAT 4-bit                                  ║")
(println "╚════════════════════════════════════════════════════════════╝")
(println "")

; Load the model
(setq loading (elapse (setq model (load_mlx_model MODEL_PATH))))
(println "Loading time:" loading)

(check (nullp model)
    (println "")
    (println "Erreur: Model couldn't be loaded")
    (exit 1))

; Display model information
(println "")
(model MLXModel (info))

; Display some tensors
(print_tensor_info (@ model 'tensor_index) 30)

; Example of accessing weights
(println "")
(println "Example of weight access:")
(println (fill "-" 40))

; Check if certain tensors exist
(setq test_names (strings
    "language_model.model.embed_tokens.weight"
    "language_model.model.layers.0.self_attn.q_proj.weight"
    "language_model.model.norm.weight"
))

(loop name test_names
    (if (model MLXModel (has_weight name))
        (println "  ✓ " name)
        (println "  ✗ " name " (not found)")))

; Tokenizer test
(println "")
(println "Test du tokenizer:")
(println (fill "-" 40))
(check (model MLXModel (vocab_size))
    (setq test_text "Bonjour, comment ça va?")
    (setq tokens (model MLXModel (encode test_text)))
    (println "  Texte: \"" test_text "\"")
    (println "  Tokens: " tokens)
    (println "  Décodé: \"" (model MLXModel (decode tokens)) "\""))



; =============================================================================
; Simple forward pass test
; =============================================================================

; Checks for required weights
(defun test_weights(model)
    (println "")
    (println "Verification of model weights...")
    (println (fill "-" 50))

    ; Use the class's get_weight method
    ; Check key weights
    (setq key_weights (strings
        "language_model.model.embed_tokens.weight"
        "language_model.model.layers.0.self_attn.q_proj.weight"
        "language_model.model.layers.0.self_attn.q_proj.scales"
        "language_model.model.layers.0.self_attn.q_norm.weight"
        "language_model.model.layers.0.mlp.gate_proj.weight"
        "language_model.model.layers.0.input_layernorm.weight"
        "language_model.model.norm.weight"
        "language_model.lm_head.weight"
    ))

    (loop name key_weights
        (if (model MLXModel (has_weight name))
            (block
                (setq w (model MLXModel (get_weight name)))
                (println "  ✓ " name " - shape: " (mlx_shape w)))
            (println "  ✗ " name " - not found")))

    (println (fill "-" 50))
)

; =============================================================================
; Real generation test
; =============================================================================

; Generation test with a simple question
(defun test_generation(model (question "What is the capital of France?") (max_tokens 50))
    (println "")
    (println (fill "=" 60))
    (println "TEST DE GÉNÉRATION")
    (println (fill "=" 60))
    (println "Question: " question)
    (println "Max tokens: " max_tokens)
    (println "")

    ; Check that the model is valid
    (check (nullp model)
        (println "ERREUR: Modèle non chargé")
        (return nil))

    ; Check the tokenizer
    (check (not (model MLXModel (vocab_size)))
        (println "ERREUR: Tokenizer non disponible")
        (return nil))

    ; Format the prompt for chat
    (setq prompt (model  MLXModel (format_prompt question)))
    (println "Prompt formaté:")
    (println prompt)
    (println (fill "-" 60))

    ; Generate the response with the new object-oriented API
    (println "")
    (println "Réponse du modèle:")
    (println "")

    ; Measure time with elapse
    (setq elapsed (elapse (setq response (model  MLXModel (chat question max_tokens 0.7)))))

    (println "")
    (println (fill "-" 60))
    (println "Temps de génération: " elapsed " ms")
    (print_memory_stats))

; =============================================================================
; Main program - Test
; =============================================================================

(println "")
(println "Model ready for use!")
(println "Variable 'model' contains MLXModel object")
(println "")
(println "Available Methods:")
(println "  - (model (encode \"texte\"))     ; Tokenizer")
(println "  - (model (decode tokens))      ; Detokenizer")
(println "  - (model (get_weight \"nom\"))  ; Access to weights")
(println "  - (model (info))               ; Information about model")
(println "  - (model (reset_cache))        ; Reinitialise KV cache")
(println "")
(println "Generation of text:")
(println "  - (model (chat \"message\"))                    ; Simple Chat")
(println "  - (model (chat \"message\" 256 0.7 \"system\"))  ; Chat with options")
(println "  - (model (generate \"prompt\" max_tokens temp)) ; Raw generation")
(println "")
(println "Test Functions:")
(println "  - (test_weights model)         ; check weights")
(println "  - (test_generation model)      ; True generation test")
(println "")
(println "Exemple:")
(println "  (model (chat \"Bonjour, qui es-tu?\"))")
(println "")

; =============================================================================
; Generation test
; =============================================================================

(println "")
(println "Launching test...")
(test_generation model "Write a short story about a robot who learns to paint." 100)

