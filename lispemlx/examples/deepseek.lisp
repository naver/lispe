; =============================================================================
; Loading a sharded MLX Safetensors model (DeepSeek-R1-Qwen3 8B)
; =============================================================================
; This script loads a DeepSeek-R1-Qwen3 model in safetensors format with multiple
; files (shards) and combines all weights into a unified dictionary.
;
; Qwen3 architecture with Q/K norm, 8-bit quantization
; Compatible with MLX models from lmstudio-community
; =============================================================================

(use 'lispe_mlx)
(use 'lispe_tiktoken)  ; For the BPE tokenizer (tiktoken)

; =============================================================================
; Model configuration
; =============================================================================

(setq MODEL_PATH "/Users/user/.lmstudio/models/lmstudio-community/DeepSeek-R1-0528-Qwen3-8B-MLX-8bit")

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
        ; Zero padding
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

; Loads all safetensors files and merges the tensors
(defun load_sharded_safetensors(dir_path num_shards)
    
    (println "Generating paths for " num_shards " safetensors files...")
    (setq files (get_safetensors_files dir_path num_shards))
    (setq num_files (size files))
    
    (check (zerop num_files)
        (println "ERROR: No safetensors files found!")
        (return nil))

    (println "Found " num_files " safetensors file(s)")

    ; List to store all tensors (index access = O(1))
    (setq all_tensors (list))
    ; Dictionary to map name -> index (used only once at loading time)
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
    (println "Loading complete!")
    (println "  Total tensors: " total_tensors)
    (println (fill "=" 60))
    
    ; Return a list (tensors_list, index_dict, metadata)
    (list all_tensors tensor_index all_metadata)
)

; =============================================================================
; Function to load the model configuration
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
    
    ; Display the first tensors
    (setq displayed 0)
    (loop name sorted_names
        (check (< displayed max_display)
            (println "  " name)
            (+= displayed 1)))
    
    (check (> total max_display)
        (println "  ... and " (- total max_display) " more tensors"))
    
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
    (println "  Peak memory:   " (/ (mlx_get_peak_memory) 1048576) " MB")
    (println "  Cache memory:  " (/ (mlx_get_cache_memory) 1048576) " MB")
    (println (fill "-" 40))
)

; =============================================================================
; Function to create tensor indices per layer
; =============================================================================

; Helper function to get a tensor by name (uses the index)
; Note: We use key@ because @ on a non-existent key raises an error
; while key@ returns nil if the key doesn't exist, otherwise the value
(defun get_tensor(weights_list tensor_index name)
    (setq idx (key@ tensor_index name))
    (ife (nullp idx)
        nil
        (@ weights_list idx)))

; Pre-extracts a layer's tensors into an ordered list for DeepSeek-R1-Qwen3
; Qwen3 structure: with Q/K norm after projection
; Returns: (input_norm q_w q_s q_b q_norm k_w k_s k_b k_norm v_w v_s v_b o_w o_s o_b
;           post_attn_norm gate_w gate_s gate_b up_w up_s up_b down_w down_s down_b)
(defun extract_layer_tensors(weights_list tensor_index layer_idx)
    (setq prefix (+ "model.layers." (string layer_idx) "."))
    (setq attn (+ prefix "self_attn."))
    (setq mlp (+ prefix "mlp."))
    
    (list
        ; 0: input_layernorm
        (get_tensor weights_list tensor_index (+ prefix "input_layernorm.weight"))
        ; 1-3: q_proj (weight, scales, biases)
        (get_tensor weights_list tensor_index (+ attn "q_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "q_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "q_proj.biases"))
        ; 4: q_norm (Qwen3 has Q/K norm)
        (get_tensor weights_list tensor_index (+ attn "q_norm.weight"))
        ; 5-7: k_proj
        (get_tensor weights_list tensor_index (+ attn "k_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "k_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "k_proj.biases"))
        ; 8: k_norm
        (get_tensor weights_list tensor_index (+ attn "k_norm.weight"))
        ; 9-11: v_proj
        (get_tensor weights_list tensor_index (+ attn "v_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "v_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "v_proj.biases"))
        ; 12-14: o_proj
        (get_tensor weights_list tensor_index (+ attn "o_proj.weight"))
        (get_tensor weights_list tensor_index (+ attn "o_proj.scales"))
        (get_tensor weights_list tensor_index (+ attn "o_proj.biases"))
        ; 15: post_attention_layernorm
        (get_tensor weights_list tensor_index (+ prefix "post_attention_layernorm.weight"))
        ; 16-18: gate_proj
        (get_tensor weights_list tensor_index (+ mlp "gate_proj.weight"))
        (get_tensor weights_list tensor_index (+ mlp "gate_proj.scales"))
        (get_tensor weights_list tensor_index (+ mlp "gate_proj.biases"))
        ; 19-21: up_proj
        (get_tensor weights_list tensor_index (+ mlp "up_proj.weight"))
        (get_tensor weights_list tensor_index (+ mlp "up_proj.scales"))
        (get_tensor weights_list tensor_index (+ mlp "up_proj.biases"))
        ; 22-24: down_proj
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

; Extracts global tensors (embed, lm_head, final_norm) for DeepSeek-R1-Qwen3
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

; Index constants for DeepSeek-R1-Qwen3 layer tensors (with Q/K norm)
(setq L_INPUT_NORM 0)      ; input_layernorm.weight
(setq L_Q_WEIGHT 1)        ; q_proj.weight
(setq L_Q_SCALES 2)        ; q_proj.scales
(setq L_Q_BIASES 3)        ; q_proj.biases
(setq L_Q_NORM 4)          ; q_norm.weight (Qwen3 Q/K norm)
(setq L_K_WEIGHT 5)        ; k_proj.weight
(setq L_K_SCALES 6)        ; k_proj.scales
(setq L_K_BIASES 7)        ; k_proj.biases
(setq L_K_NORM 8)          ; k_norm.weight (Qwen3 Q/K norm)
(setq L_V_WEIGHT 9)        ; v_proj.weight
(setq L_V_SCALES 10)       ; v_proj.scales
(setq L_V_BIASES 11)       ; v_proj.biases
(setq L_O_WEIGHT 12)       ; o_proj.weight
(setq L_O_SCALES 13)       ; o_proj.scales
(setq L_O_BIASES 14)       ; o_proj.biases
(setq L_POST_ATTN_NORM 15) ; post_attention_layernorm.weight
(setq L_GATE_WEIGHT 16)    ; gate_proj.weight
(setq L_GATE_SCALES 17)    ; gate_proj.scales
(setq L_GATE_BIASES 18)    ; gate_proj.biases
(setq L_UP_WEIGHT 19)      ; up_proj.weight
(setq L_UP_SCALES 20)      ; up_proj.scales
(setq L_UP_BIASES 21)      ; up_proj.biases
(setq L_DOWN_WEIGHT 22)    ; down_proj.weight
(setq L_DOWN_SCALES 23)    ; down_proj.scales
(setq L_DOWN_BIASES 24)    ; down_proj.biases

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
(setq KV_KEYS 0)
(setq KV_VALUES 1)
(setq KV_OFFSET 2)

; Represents a loaded MLX model with its weights, configuration and tokenizer
; Attributes:
;   weights: tensor list (O(1) index access)
;   tensor_index: dictionary name -> index
;   config: model configuration
;   metadata: safetensors metadata
;   tokenizer: tiktoken tokenizer
;   cached_embeddings: pre-dequantized embeddings
;   rope_freqs: pre-computed RoPE frequencies (single, no sliding window)
;   layer_tensors: pre-extracted tensors per layer
;   global_tensors: global tensors (embed, lm_head, norm)
;   attn_params: pre-computed attention parameters
;   kv_caches: KV caches for generation (created on demand)
(class@ MLXModel (weights tensor_index config metadata tokenizer cached_embeddings rope_freqs layer_tensors global_tensors attn_params kv_caches)
    
    ; =========================================================================
    ; Basic accessors
    ; =========================================================================

    ; Retrieves a tensor by name
    (defun get_weight(name)
        (setq idx (@ tensor_index name))
        (check (nullp idx)
            (println "WARNING: Tensor not found: " name)
            (return nil))
        (@ weights idx))

    ; Checks if a tensor exists
    (defun has_weight(name)
        (neq (@ tensor_index name) nil))
    
    ; Lists tensor names (optionally filtered by pattern)
    (defun list_weights((pattern ""))
        (setq all_names (keys@ tensor_index))
        (ncheck (eq pattern "")
            (setq rx (rgx pattern))
            (filter (λ(n) (rgx_match rx n)) all_names)
            all_names))
    
    ; Returns the number of tensors
    (defun num_weights()
        (size weights))
    
    ; Retrieves a configuration value
    (defun get_config(key (default nil))
        (select (@ config key) default))
    
    ; Displays model information
    (defun info()
        (println "MLX Model:")
        (println "  Tensors: " (num_weights))
        (println "  Type: " (get_config "model_type" "unknown"))
        (println "  Architecture: " (get_config "architectures" "unknown"))
        (check tokenizer
            (println "  Vocabulary: " (tiktoken_vocab_size tokenizer) " tokens")))
    
    ; =========================================================================
    ; Tokenizer (tiktoken)
    ; =========================================================================
    
    ; Encodes text into tokens
    (defun encode(text)
        (check (not tokenizer)
            (println "ERROR: Tokenizer not loaded")
            (return nil))
        (tiktoken_encode tokenizer text))
    
    ; Decodes tokens into text
    (defun decode(tokens)
        (check (not tokenizer)
            (println "ERROR: Tokenizer not loaded")
            (return nil))
        (tiktoken_decode tokenizer tokens))
    
    ; Returns the vocabulary size
    (defun vocab_size()
        (check (not tokenizer)
            (return 0))
        (tiktoken_vocab_size tokenizer))
    
    ; =========================================================================
    ; Internal utility functions
    ; =========================================================================

    ; Linear projection with quantized or non-quantized weights
    ; Note: For MLX 8-bit quantization, biases is mandatory if scales exists
    ; transpose_weight is always true for our use cases
    (defun _linear(x weight scales biases)
        (ife scales
            ; MLX 8-bit quantization (biases mandatory)
            (mlx_quantized_matmul x weight scales biases true 64 8)
            (mlx_matmul x . mlx_transpose weight)))
    
    ; Standard RMSNorm (not Gemma style for Qwen3)
    (defun _rms_norm(x weight eps)
        (mlx_rms_norm x weight eps false))
    
    ; Applies RoPE to queries/keys
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

    ; Creates a standard KV cache
    (defun _make_kv_cache()
        (dictionaryi KV_KEYS nil KV_VALUES nil KV_OFFSET 0))
    
    ; Creates all KV caches (one per layer)
    (defun _make_all_kv_caches()
        (setq num_layers (@ config "num_hidden_layers"))
        (setq caches (list))
        (loopcount num_layers i
            (push caches (_make_kv_cache)))
        caches)
    
    ; Updates a KV cache
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
    
    ; Resets the KV caches
    (defun reset_cache()
        (setqi kv_caches nil))
    
    ; =========================================================================
    ; Forward pass - Components
    ; =========================================================================

    ; MLP forward with SiLU (not GELU for Qwen3)
    (defun _mlp(x lt)
        (setq gate (_linear x (@ lt L_GATE_WEIGHT) (@ lt L_GATE_SCALES) (@ lt L_GATE_BIASES)))
        (setq up (_linear x (@ lt L_UP_WEIGHT) (@ lt L_UP_SCALES) (@ lt L_UP_BIASES)))
        ; Qwen3 uses SiLU (swish) instead of GELU
        (setq hidden (mlx_multiply (mlx_silu gate) up))
        (_linear hidden (@ lt L_DOWN_WEIGHT) (@ lt L_DOWN_SCALES) (@ lt L_DOWN_BIASES)))
    
    ; Attention forward for Qwen3 (with Q/K norm, no sliding window)
    (defun _attention(x lt offset kv_cache)
        (setq n_heads (@ attn_params 0))
        (setq n_kv_heads (@ attn_params 1))
        (setq head_dim (@ attn_params 2))
        (setq scale (@ attn_params 3))
        (setq eps (@ attn_params 4))
        (setq repeats (@ attn_params 5))
        (setq hidden_out (@ attn_params 6))
        
        (setq shape (mlx_shape x))
        (setq B (@ shape 0))
        (setq L (@ shape 1))
        
        ; Extract tensors explicitly (indices updated for Q/K norm)
        (setq q_w (@ lt L_Q_WEIGHT))
        (setq q_s (@ lt L_Q_SCALES))
        (setq q_b (@ lt L_Q_BIASES))
        (setq q_norm (@ lt L_Q_NORM))
        (setq k_w (@ lt L_K_WEIGHT))
        (setq k_s (@ lt L_K_SCALES))
        (setq k_b (@ lt L_K_BIASES))
        (setq k_norm (@ lt L_K_NORM))
        (setq v_w (@ lt L_V_WEIGHT))
        (setq v_s (@ lt L_V_SCALES))
        (setq v_b (@ lt L_V_BIASES))
        
        ; QKV projections
        (setq queries (_linear x q_w q_s q_b))
        (setq keys (_linear x k_w k_s k_b))
        (setq values (_linear x v_w v_s v_b))
        
        ; Reshape [B, L, n_heads * head_dim] -> [B, n_heads, L, head_dim]
        (setq queries (mlx_transpose (mlx_reshape queries (integers B L n_heads head_dim)) TRANSPOSE_0213))
        (setq keys (mlx_transpose (mlx_reshape keys (integers B L n_kv_heads head_dim)) TRANSPOSE_0213))
        (setq values (mlx_transpose (mlx_reshape values (integers B L n_kv_heads head_dim)) TRANSPOSE_0213))
        
        ; Qwen3 Q/K norm - apply RMS normalization before RoPE
        (setq queries (_rms_norm queries q_norm eps))
        (setq keys (_rms_norm keys k_norm eps))
        
        ; Apply RoPE
        (setq queries (_apply_rope queries rope_freqs offset))
        (setq keys (_apply_rope keys rope_freqs offset))
        
        ; KV Cache
        (check kv_cache
            (setq kv_result (_update_kv_cache kv_cache keys values))
            (setq keys (@ kv_result 0))
            (setq values (@ kv_result 1)))
        
        ; GQA expansion (num_heads / num_kv_heads repetitions)
        (setq keys_expanded keys)
        (setq values_expanded values)
        (check (> repeats 1)
            (setq keys_expanded (mlx_repeat keys repeats 1))
            (setq values_expanded (mlx_repeat values repeats 1)))
        
        ; Attention scores: Q @ K^T * scale
        (setq scale_arr (mlx_array scale))
        (setq attention_scores (mlx_multiply (mlx_matmul queries . mlx_transpose keys_expanded TRANSPOSE_0132) scale_arr))
        
        ; Causal mask (only for prefill, L > 1)
        (setq L_kv (@ (mlx_shape keys) 2))
        
        (check (> L 1)
            ; Create causal mask [L, L_kv]
            (setq linds (mlx_reshape (mlx_arange 0 L 1 "int32") (integers L 1)))
            (setq rinds (mlx_reshape (mlx_arange 0 L_kv 1 "int32") (integers 1 L_kv)))
            (setq causal_mask (mlx_greater_equal linds rinds))
            (setq mask (mlx_where causal_mask (mlx_zeros (integers L L_kv) "float32") . mlx_array -10000.0))
            (setq attention_scores (mlx_add attention_scores . mlx_reshape mask . integers 1 1 L L_kv)))
        
        ; Softmax + output
        (setq attention_output (mlx_matmul (mlx_softmax attention_scores -1) values_expanded))
        (setq attention_output (mlx_reshape (mlx_transpose attention_output TRANSPOSE_0213) (integers B L hidden_out)))
        (_linear attention_output (@ lt L_O_WEIGHT) (@ lt L_O_SCALES) (@ lt L_O_BIASES)))
    
    ; Transformer block for Qwen3 (standard Pre-LN architecture)
    (defun _transformer_block(x lt offset kv_cache)
        (setq eps (@ attn_params 4))
        
        ; Pre-norm + attention
        (setq normed_x (_rms_norm x (@ lt L_INPUT_NORM) eps))
        (setq attn_output (_attention normed_x lt offset kv_cache))
        
        ; Residual connection
        (setq h (mlx_add x attn_output))
        
        ; Pre-norm + MLP
        (setq normed_h (_rms_norm h (@ lt L_POST_ATTN_NORM) eps))
        (setq mlp_output (_mlp normed_h lt))
        
        ; Residual connection
        (mlx_add h mlp_output))
    
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
        
        ; Embedding: input_ids is [B, L], we take embeddings for each token
        ; mlx_take on axis 0 of embeddings (vocab_size, hidden_size)
        ; Flatten input_ids to 1D, take embeddings, then reshape to [B, L, hidden_size]
        (setq flat_ids (mlx_flatten input_ids))
        (setq embeddings_flat (mlx_take cached_embeddings flat_ids 0))
        (setq hidden_size (@ (mlx_shape cached_embeddings) 1))
        (setq hidden_states (mlx_reshape embeddings_flat (integers B L hidden_size)))
        
        ; Create KV caches if needed
        (check (nullp kv_caches)
            (setqi kv_caches (_make_all_kv_caches)))
        
        ; Pass through all layers
        (loopcount num_layers i
            (setq hidden_states (_transformer_block 
                hidden_states 
                (@ layer_tensors i)
                offset 
                (@ kv_caches i))))
        
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

    ; Formats a chat prompt for Qwen3/DeepSeek (ChatML format)
    ; <|im_start|>system\n...\n<|im_end|>\n<|im_start|>user\n...\n<|im_end|>\n<|im_start|>assistant\n
    (defun format_prompt(user_message (system_prompt ""))
        (setq prompt "")
        (ncheck (eq system_prompt "")
            (+= prompt "<|im_start|>system\n" system_prompt "<|im_end|>\n"))
        (+= prompt "<|im_start|>user\n" user_message "<|im_end|>\n<|im_start|>assistant\n")
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
    
    ; Generates text from a prompt
    (defun generate(prompt_text max_new_tokens (temperature 0.7))
        (println "")
        (println "Generating...")
        (println "Prompt: \"" prompt_text "\"")
        (println "")
        
        ; Reset the cache
        (reset_cache)
        
        ; Encode the prompt (tiktoken returns a list of integers directly)
        (setq input_ids (encode prompt_text))
        
        ; BOS token for Qwen3 = 151643, EOS = 151645 (<|im_end|>)
        (setq BOS_TOKEN 151643)
        (setq EOS_TOKEN 151645)
        
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
                
                ; Convert to integer
                (setq next_token_flat (mlx_flatten next_token))
                (setq next_token_id (integer (@ next_token_flat 0)))
                
                ; Display in streaming
                (printerr (decode (integers next_token_id)))
                (+= num_generated 1)
                
                ; Check for end (EOS = 151645 for Qwen3)
                (check (eq next_token_id EOS_TOKEN)
                    (break))
                
                ; Add and prepare next
                (push generated_tokens next_token_id)
                (setq input_tensor (mlx_reshape (mlx_array (integers next_token_id) nil "int32") (integers 1 1)))
            )))
        
        ; Stats
        (setq tokens_per_sec (/ (* num_generated 1000.0) gen_time))
        (println "")
        (println (fill "=" 60))
        (println "Tokens generated: " num_generated " in " (/ gen_time 1000.0) " sec")
        (println "Speed: " tokens_per_sec " tokens/sec")
        (println (fill "=" 60))
        
        ; Decode
        (setq output_text (decode generated_tokens))
        (println "")
        (println "Generated text:")
        (println output_text)
        (println (fill "=" 60))
        
        output_text)
    
    ; Simplified chat interface
    (defun chat(user_message (max_tokens 256) (temperature 0.7) (system_prompt ""))
        (setq formatted_prompt (format_prompt user_message system_prompt))
        (generate formatted_prompt max_tokens temperature))
)

; =============================================================================
; Main loading function
; =============================================================================

; Loads a complete MLX model (sharded safetensors + config)
(defun load_mlx_model(model_path)
    
    (println (fill "=" 60))
    (println "Loading MLX model")
    (println "Path: " model_path)
    (println (fill "=" 60))
    
    ; Load configuration
    (println "")
    (println "1. Loading configuration...")
    (setq config (load_model_config model_path))
    (println "   ✓ Configuration loaded")

    ; Display some config info
    (check (in config "model_type")
        (println "   Type: " (@ config "model_type")))
    (check (in config "architectures")
        (println "   Architecture: " (@ config "architectures")))
    
    ; Load weights (2 shards for DeepSeek-R1-Qwen3 8-bit)
    (println "")
    (println "2. Loading weights...")
    (setq result (load_sharded_safetensors model_path 2))
    
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
        ; If no pattern in pretokenizers, try another structure
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
    
    ; Pre-compute RoPE frequencies (optimization)
    (println "")
    (println "5. Pre-computing RoPE frequencies...")
    (setq head_dim (@ config "head_dim"))
    (setq rope_theta (@ config "rope_theta"))

    ; Compute inv_freq for Qwen3 (single base)
    ; inv_freq = 1.0 / (base ** (arange(0, head_dim, 2) / head_dim))
    (setq indices (mlx_arange 0 head_dim 2 "float32"))
    (setq scaled (mlx_divide indices (mlx_array head_dim)))

    (setq rope_freqs (mlx_reciprocal (mlx_power (mlx_array rope_theta) scaled)))

    (println "   ✓ RoPE frequencies pre-computed")
    (println "     theta=" rope_theta ": " (mlx_shape rope_freqs))
    
    ; Pre-extract tensors per layer (O(1) access optimization)
    (println "")
    (println "6. Pre-extracting tensors per layer...")
    (setq num_layers (@ config "num_hidden_layers"))
    (setq layer_tensors (build_layer_tensors weights_list tensor_index num_layers))
    (setq global_tensors (extract_global_tensors weights_list tensor_index))
    (println "   ✓ " num_layers " layers pre-indexed")
    (println "   ✓ Global tensors extracted")
    
    ; Pre-compute attention parameters (major optimization)
    (println "")
    (println "7. Pre-computing attention parameters...")
    (setq n_heads (@ config "num_attention_heads"))
    (setq n_kv_heads (@ config "num_key_value_heads"))
    (setq hidden_size (@ config "hidden_size"))
    (setq scale_float (/ 1.0 (sqrt head_dim)))  ; scale = 1/sqrt(head_dim) for Qwen3
    (setq eps (@ config "rms_norm_eps"))
    (setq repeats (/ n_heads n_kv_heads))
    (setq hidden_out (* n_heads head_dim))
    
    ; attn_params = (n_heads, n_kv_heads, head_dim, scale_float, eps, repeats, hidden_out)
    (setq attn_params (list n_heads n_kv_heads head_dim scale_float eps repeats hidden_out))
    (println "   ✓ Attention parameters pre-computed")
    (println "     n_heads=" n_heads ", n_kv_heads=" n_kv_heads ", head_dim=" head_dim)
    (println "     scale=" scale_float ", eps=" eps ", repeats=" repeats)
    
    ; Create the model object (kv_caches initialized to nil, created on demand)
    (setq model (MLXModel weights_list tensor_index config metadata tokenizer cached_embeddings rope_freqs layer_tensors global_tensors attn_params nil))
    
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
(println "║  MLX Model Loader for LispE                                ║")
(println "║  DeepSeek-R1-0528-Qwen3-8B-MLX 8-bit                      ║")
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
(print_tensor_info (@ model 'tensor_index) 30)

; Example of weight access
(println "")
(println "Example of weight access:")
(println (fill "-" 40))

; Check if certain tensors exist
(setq test_names (strings
    "model.embed_tokens.weight"
    "model.layers.0.self_attn.q_proj.weight"
    "model.norm.weight"
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
    (println "  Text: \"" test_text "\"")
    (println "  Tokens: " tokens)
    (println "  Decoded: \"" (model MLXModel (decode tokens)) "\""))



; =============================================================================
; Simple forward pass test
; =============================================================================

; Checks for the presence of required weights
(defun test_weights(model)
    (println "")
    (println "Checking model weights...")
    (println (fill "-" 50))
    
    ; Use the class get_weight method
    ; Check key weights for DeepSeek-R1-Qwen3
    (setq key_weights (strings
        "model.embed_tokens.weight"
        "model.layers.0.self_attn.q_proj.weight"
        "model.layers.0.self_attn.q_proj.scales"
        "model.layers.0.mlp.gate_proj.weight"
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
(defun test_generation(model (question "What is the capital of France?") (max_tokens 50))
    (println "")
    (println (fill "=" 60))
    (println "GENERATION TEST")
    (println (fill "=" 60))
    (println "Question: " question)
    (println "Max tokens: " max_tokens)
    (println "")
    
    ; Check that the model is valid
    (check (nullp model)
        (println "ERROR: Model not loaded")
        (return nil))
    
    ; Check the tokenizer
    (check (not (model MLXModel (vocab_size)))
        (println "ERROR: Tokenizer not available")
        (return nil))
    
    ; Format the prompt for chat
    (setq prompt (model  MLXModel (format_prompt question)))
    (println "Formatted prompt:")
    (println prompt)
    (println (fill "-" 60))
    
    ; Generate the response with the new object-oriented API
    (println "")
    (println "Model response:")
    (println "")
    
    ; Measure time with elapse
    (setq elapsed (elapse (setq response (model  MLXModel (chat question max_tokens 0.7)))))
    
    (println "")
    (println (fill "-" 60))
    (println "Generation time: " elapsed " ms")
    (print_memory_stats))

; =============================================================================
; Main program - Test
; =============================================================================

(println "")
(println "The model is ready to use!")
(println "Variable 'model' contains the MLXModel object")
(println "")
(println "Available methods:")
(println "  - (model (encode \"text\"))      ; Tokenizer")
(println "  - (model (decode tokens))      ; Detokenizer")
(println "  - (model (get_weight \"name\")) ; Weight access")
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
(println "")
(println "Example:")
(println "  (model (chat \"Hello, who are you?\"))")
(println "")

; =============================================================================
; Generation test
; =============================================================================

(println "")
(println "Starting test...")
(test_generation model "Write a short story about a robot who learns to paint." 100)
