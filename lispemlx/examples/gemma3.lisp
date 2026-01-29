; =============================================================================
; Chargement d'un modèle MLX Safetensors shardé (Gemma 3 27B)
; =============================================================================
; Ce script charge un modèle au format safetensors avec plusieurs fichiers
; (shards) et combine tous les poids dans un dictionnaire unifié.
;
; Compatible avec les modèles MLX de mlx-community sur HuggingFace
; =============================================================================

(use 'lispe_mlx)
(use 'lispe_torch)  ; Pour le tokenizer SentencePiece

; =============================================================================
; Configuration du modèle
; =============================================================================

(setq MODEL_PATH "/Users/user/.lmstudio/models/mlx-community/gemma-3-27b-it-qat-4bit")

; =============================================================================
; Fonction pour générer les chemins des fichiers safetensors shardés
; =============================================================================

; Génère les chemins des fichiers safetensors pour num_shards fichiers
(defun get_safetensors_files(dir_path num_shards)
    (setq files (list))
    (setq num_str (string num_shards))
    (setq pad_len (size num_str))
    
    (loopcount num_shards i
        (setq idx (+ i 1))
        (setq idx_str (string idx))
        ; Padding avec des zéros
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
; Fonction pour charger et fusionner plusieurs fichiers safetensors
; =============================================================================

; Charge tous les fichiers safetensors et fusionne les tenseurs
(defun load_sharded_safetensors(dir_path num_shards)
    
    (println "Génération des chemins pour " num_shards " fichiers safetensors...")
    (setq files (get_safetensors_files dir_path num_shards))
    (setq num_files (size files))
    
    (check (zerop num_files)
        (println "ERREUR: Aucun fichier safetensors trouvé!")
        (return nil))
    
    (println "Trouvé " num_files " fichier(s) safetensors")
    
    ; Liste pour stocker tous les tenseurs (accès par index = O(1))
    (setq all_tensors (list))
    ; Dictionnaire pour mapper nom -> index (utilisé une seule fois au chargement)
    (setq tensor_index (dictionary))
    (setq all_metadata (dictionary))
    (setq total_tensors 0)
    
    ; Charger chaque fichier
    (setq file_idx 0)
    (loop filepath files
        (+= file_idx 1)
        (println "")
        (println "[" file_idx "/" num_files "] Chargement: " filepath)
        
        ; Charger le fichier safetensors
        (setq result (mlx_load_safetensors filepath))
        (setq tensors (@ result 0))
        (setq metadata (@ result 1))
        
        ; Compter et fusionner les tenseurs
        (setq tensor_names (keys@ tensors))
        (setq num_tensors (size tensor_names))
        (println "  → " num_tensors " tenseurs chargés")
        
        ; Ajouter les tenseurs à la liste et créer l'index
        (loop name tensor_names
            (set@ tensor_index name total_tensors)
            (push all_tensors (@ tensors name))
            (+= total_tensors 1))
        
        ; Fusionner les métadonnées
        (loop key (keys@ metadata)
            (set@ all_metadata key (@ metadata key)))
        
        ; Libérer la mémoire (optionnel, garbage collector s'en charge)
        (mlx_synchronize)
    )
    
    (println "")
    (println (fill "=" 60))
    (println "Chargement terminé!")
    (println "  Total tenseurs: " total_tensors)
    (println (fill "=" 60))
    
    ; Retourner une liste (tenseurs_liste, index_dict, métadonnées)
    (list all_tensors tensor_index all_metadata)
)

; =============================================================================
; Fonction pour charger la configuration du modèle
; =============================================================================

; Charge le fichier config.json du modèle
(defun load_model_config(dir_path)
    (setq config_path (+ dir_path "/config.json"))
    (maybe
        (json_read config_path)
        (block
            (println "AVERTISSEMENT: config.json non trouvé")
            (dictionary))))

; =============================================================================
; Fonction pour afficher les informations sur les tenseurs
; =============================================================================

; Affiche des informations sur les tenseurs chargés
; tensor_index: dictionnaire nom -> index
(defun print_tensor_info(tensor_index (max_display 20))
    (setq names (keys@ tensor_index))
    (setq total (size names))
    
    (println "")
    (println "Informations sur les tenseurs:")
    (println (fill "-" 60))
    
    ; Trier les noms
    (setq sorted_names (sort '< names))
    
    ; Afficher les premiers tenseurs
    (setq displayed 0)
    (loop name sorted_names
        (check (< displayed max_display)
            (println "  " name)
            (+= displayed 1)))
    
    (check (> total max_display)
        (println "  ... et " (- total max_display) " autres tenseurs"))
    
    (println (fill "-" 60))
    (println "Total: " total " tenseurs")
)

; =============================================================================
; Fonction pour obtenir les statistiques mémoire
; =============================================================================

; Affiche les statistiques d'utilisation mémoire MLX
(defun print_memory_stats()
    (println "")
    (println "Statistiques mémoire MLX:")
    (println (fill "-" 40))
    (println "  Mémoire active: " (/ (mlx_get_active_memory) 1048576) " MB")
    (println "  Mémoire pic:    " (/ (mlx_get_peak_memory) 1048576) " MB")
    (println "  Mémoire cache:  " (/ (mlx_get_cache_memory) 1048576) " MB")
    (println (fill "-" 40))
)

; =============================================================================
; Fonction pour créer les indices des tenseurs par couche
; =============================================================================

; Fonction helper pour obtenir un tenseur par son nom (utilise l'index)
(defun get_tensor(weights_list tensor_index name)
    (setq idx (@ tensor_index name))
    (if idx (@ weights_list idx) nil))

; Pré-extrait les tenseurs d'une couche dans une liste ordonnée
; Retourne: (input_norm q_w q_s q_b k_w k_s k_b v_w v_s v_b o_w o_s o_b q_norm k_norm 
;            post_attn_norm pre_ff_norm gate_w gate_s gate_b up_w up_s up_b down_w down_s down_b post_ff_norm)
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

; Crée la liste des tenseurs pour toutes les couches
(defun build_layer_tensors(weights_list tensor_index num_layers)
    (setq layers (list))
    (loopcount num_layers i
        (push layers (extract_layer_tensors weights_list tensor_index i)))
    layers)

; Extrait les tenseurs globaux (embed, lm_head, final_norm)
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
; Structure du modèle
; =============================================================================

; Constantes d'indices pour les tenseurs de couche (définies avant la classe)
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

; Constantes pour les tenseurs globaux
(setq G_EMBED_WEIGHT 0)
(setq G_EMBED_SCALES 1)
(setq G_EMBED_BIASES 2)
(setq G_FINAL_NORM 3)
(setq G_LM_HEAD_WEIGHT 4)
(setq G_LM_HEAD_SCALES 5)
(setq G_LM_HEAD_BIASES 6)

; Indices de transposition
(setq TRANSPOSE_0213 (integers 0 2 1 3))
(setq TRANSPOSE_0132 (integers 0 1 3 2))

; Constantes KV cache
(setq KV_TYPE 0)
(setq KV_KEYS 1)
(setq KV_VALUES 2)
(setq KV_MAX_SIZE 3)
(setq KV_OFFSET 4)
(setq CACHE_STANDARD 0)
(setq CACHE_ROTATING 1)

; Représente un modèle MLX chargé avec ses poids, sa configuration et son tokenizer
; Attributs:
;   weights: liste des tenseurs (accès par index O(1))
;   tensor_index: dictionnaire nom -> index
;   config: configuration du modèle
;   metadata: métadonnées safetensors
;   tokenizer: tokenizer SentencePiece
;   cached_embeddings: embeddings pré-déquantifiés
;   rope_freqs_global/local: fréquences RoPE pré-calculées
;   layer_tensors: tenseurs pré-extraits par couche
;   global_tensors: tenseurs globaux (embed, lm_head, norm)
;   attn_params: paramètres d'attention pré-calculés
;   kv_caches: caches KV pour la génération (créé à la demande)
(class@ MLXModel (weights tensor_index config metadata tokenizer cached_embeddings rope_freqs_global rope_freqs_local layer_tensors global_tensors attn_params kv_caches)
    
    ; =========================================================================
    ; Accesseurs de base
    ; =========================================================================
    
    ; Récupère un tenseur par son nom
    (defun get_weight(name)
        (setq idx (@ tensor_index name))
        (check (nullp idx)
            (println "AVERTISSEMENT: Tenseur non trouvé: " name)
            (return nil))
        (@ weights idx))
    
    ; Vérifie si un tenseur existe
    (defun has_weight(name)
        (neq (@ tensor_index name) nil))
    
    ; Liste les noms des tenseurs (optionnellement filtrés par pattern)
    (defun list_weights((pattern ""))
        (setq all_names (keys@ tensor_index))
        (ncheck (eq pattern "")
            (setq rx (rgx pattern))
            (filter (λ(n) (rgx_match rx n)) all_names)
            all_names))
    
    ; Retourne le nombre de tenseurs
    (defun num_weights()
        (size weights))
    
    ; Récupère une valeur de configuration
    (defun get_config(key (default nil))
        (select (@ config key) default))
    
    ; Récupère text_config
    (defun text_config()
        (@ config "text_config"))
    
    ; Affiche les informations du modèle
    (defun info()
        (println "Modèle MLX:")
        (println "  Tenseurs: " (num_weights))
        (println "  Type: " (get_config "model_type" "inconnu"))
        (println "  Architecture: " (get_config "architectures" "inconnue"))
        (check tokenizer
            (println "  Vocabulaire: " (torch_vocab_size tokenizer) " tokens")))
    
    ; =========================================================================
    ; Tokenizer
    ; =========================================================================
    
    ; Encode du texte en tokens
    (defun encode(text)
        (check (not tokenizer)
            (println "ERREUR: Tokenizer non chargé")
            (return nil))
        (torch_encode tokenizer text))
    
    ; Décode des tokens en texte
    (defun decode(tokens)
        (check (not tokenizer)
            (println "ERREUR: Tokenizer non chargé")
            (return nil))
        (setq ttype (type tokens))
        (if (or (== ttype "list_") (== ttype "integers_"))
            (torch_decode tokenizer (torch_tensor tokens "long"))
            (torch_decode tokenizer tokens)))
    
    ; Retourne la taille du vocabulaire
    (defun vocab_size()
        (check (not tokenizer)
            (return 0))
        (torch_vocab_size tokenizer))
    
    ; =========================================================================
    ; Fonctions utilitaires internes
    ; =========================================================================
    
    ; Projection linéaire avec poids quantifiés ou non
    (defun _linear(x weight scales biases (transpose_weight true))
        (ife scales
            (if biases
                (mlx_quantized_matmul x weight scales biases transpose_weight 64 4)
                (mlx_quantized_matmul x weight scales nil transpose_weight 64 4))
            (if transpose_weight
                (mlx_matmul x . mlx_transpose weight)
                (mlx_matmul x weight))))
    
    ; RMSNorm avec style Gemma (1 + weight)
    (defun _rms_norm(x weight eps)
        (mlx_rms_norm x weight eps true))
    
    ; Applique RoPE aux queries/keys
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
    
    ; Crée un cache KV standard
    (defun _make_kv_cache_standard()
        (dictionaryi KV_TYPE CACHE_STANDARD KV_KEYS nil KV_VALUES nil KV_OFFSET 0))
    
    ; Crée un cache KV rotatif
    (defun _make_kv_cache_rotating(max_size)
        (dictionaryi KV_TYPE CACHE_ROTATING KV_KEYS nil KV_VALUES nil KV_MAX_SIZE max_size KV_OFFSET 0))
    
    ; Crée tous les caches KV
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
    
    ; Met à jour un cache standard
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
    
    ; Met à jour un cache rotatif
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
    
    ; Met à jour un cache (dispatch selon type)
    (defun _update_kv_cache(cache keys values)
        (if (eq (@ cache KV_TYPE) CACHE_ROTATING)
            (_update_kv_cache_rotating cache keys values)
            (_update_kv_cache_standard cache keys values)))
    
    ; Réinitialise les caches KV
    (defun reset_cache()
        (setqi kv_caches nil))
    
    ; =========================================================================
    ; Forward pass - Composants
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
        
        ; Masque
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
    ; Forward pass complet
    ; =========================================================================
    
    ; Forward pass - retourne (logits, kv_caches)
    (defun forward(input_ids offset)
        (setq tc (text_config))
        (setq num_layers (@ tc "num_hidden_layers"))
        (setq eps (@ attn_params 4))
        (setq hidden_size (@ tc "hidden_size"))
        
        ; Embedding
        (setq embeddings (mlx_take cached_embeddings input_ids 0))
        (setq hidden_states (mlx_multiply embeddings (mlx_array (sqrt hidden_size))))
        
        ; Créer les caches KV si nécessaire
        (check (nullp kv_caches)
            (setqi kv_caches (_make_all_kv_caches)))
        
        ; Passer à travers toutes les couches
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
    ; Génération de texte
    ; =========================================================================
    
    ; Formate un prompt de chat
    (defun format_prompt(user_message (system_prompt ""))
        (setq prompt "")
        (check (not (eq system_prompt ""))
            (setq prompt (+ prompt "<start_of_turn>system\n" system_prompt "<end_of_turn>\n")))
        (+= prompt "<start_of_turn>user\n")
        (+= prompt user_message)
        (+= prompt "<end_of_turn>\n<start_of_turn>model\n")
        prompt)
    
    ; Échantillonnage
    (defun _sample(logits temperature)
        (ife (< temperature 0.01)
            (mlx_argmax logits -1)

            (setq logits_flat (mlx_squeeze . mlx_divide logits . mlx_array temperature))
            (setq shape (mlx_shape logits_flat))
            (check (eq (size shape) 1)
                (setq logits_flat (mlx_reshape logits_flat (integers 1 (@ shape 0)))))
            (mlx_random_categorical logits_flat -1)))
    
    ; Génère du texte à partir d'un prompt
    (defun generate(prompt_text max_new_tokens (temperature 0.7))
        (println "")
        (println "Génération en cours...")
        (println "Prompt: \"" prompt_text "\"")
        (println "")
        
        ; Réinitialiser le cache
        (reset_cache)
        
        ; Encoder le prompt
        (setq input_ids_tensor (encode prompt_text))
        (setq input_ids (tensor_to_list input_ids_tensor))
        
        ; Ajouter BOS si absent
        (setq BOS_TOKEN 2)
        (check (not (eq (@ input_ids 0) BOS_TOKEN))
            (setq input_ids (cons BOS_TOKEN input_ids)))
        
        ; Convertir en array MLX
        (setq num_tokens (size input_ids))
        (setq input_tensor (mlx_array input_ids nil "int32"))
        (setq input_tensor (mlx_reshape input_tensor (integers 1 num_tokens)))
        
        ; Liste pour stocker les tokens générés
        (setq generated_tokens input_ids)
        (setq num_generated 0)
        
        ; Mesurer le temps
        (setq gen_time (elapse
            (loopcount max_new_tokens i
                (setq offset (- (size generated_tokens) (@ (mlx_shape input_tensor) 1)))
                
                ; Forward
                (setq logits (forward input_tensor offset))
                
                ; Dernier token
                (setq last_idx (- (@ (mlx_shape logits) 1) 1))
                (setq last_logits (mlx_take logits (mlx_array (integers last_idx) nil "int32") 1))
                
                ; Échantillonner
                (setq next_token (_sample last_logits temperature))
                (setq next_token (mlx_eval next_token))
                
                ; Convertir en entier
                (setq next_token_flat (mlx_flatten next_token))
                (setq next_token_id (integer (@ next_token_flat 0)))
                
                ; Afficher en streaming
                (printerr (decode (integers next_token_id)))
                (+= num_generated 1)
                
                ; Vérifier fin
                (check (or (eq next_token_id 1) (eq next_token_id 107))
                    (break))
                
                ; Ajouter et préparer prochain
                (push generated_tokens next_token_id)
                (setq input_tensor (mlx_reshape (mlx_array (integers next_token_id) nil "int32") (integers 1 1)))
            )))
        
        ; Stats
        (setq tokens_per_sec (/ (* num_generated 1000.0) gen_time))
        (println "")
        (println (fill "=" 60))
        (println "Tokens générés: " num_generated " en " (/ gen_time 1000.0) " sec")
        (println "Vitesse: " tokens_per_sec " tokens/sec")
        (println (fill "=" 60))
        
        ; Décoder
        (setq output_text (decode generated_tokens))
        (println "")
        (println "Texte généré:")
        (println output_text)
        (println (fill "=" 60))
        
        output_text)
    
    ; Interface chat simplifiée
    (defun chat(user_message (max_tokens 256) (temperature 0.7) (system_prompt ""))
        (setq formatted_prompt (format_prompt user_message system_prompt))
        (generate formatted_prompt max_tokens temperature))
)

; =============================================================================
; Fonction principale de chargement
; =============================================================================

; Charge un modèle MLX complet (safetensors shardé + config)
(defun load_mlx_model(model_path)
    
    (println (fill "=" 60))
    (println "Chargement du modèle MLX")
    (println "Chemin: " model_path)
    (println (fill "=" 60))
    
    ; Charger la configuration
    (println "")
    (println "1. Chargement de la configuration...")
    (setq config (load_model_config model_path))
    (println "   ✓ Configuration chargée")
    
    ; Afficher quelques infos de config
    (check (in config "model_type")
        (println "   Type: " (@ config "model_type")))
    (check (in config "architectures")
        (println "   Architecture: " (@ config "architectures")))
    
    ; Charger les poids (4 shards pour Gemma 3 27B)
    (println "")
    (println "2. Chargement des poids...")
    (setq result (load_sharded_safetensors model_path 4))
    
    (check (nullp result)
        (println "ERREUR: Échec du chargement des poids")
        (return nil))
    
    (setq weights_list (@ result 0))
    (setq tensor_index (@ result 1))
    (setq metadata (@ result 2))
    
    ; Charger le tokenizer SentencePiece
    (println "")
    (println "3. Chargement du tokenizer...")
    (setq tokenizer_path (+ model_path "/tokenizer.model"))
    (setq tokenizer (maybe
        (torch_sentencepiece_tokenizer tokenizer_path)
        nil))
    (if tokenizer
        (println "   ✓ Tokenizer chargé (" (torch_vocab_size tokenizer) " tokens)")
        (println "   ⚠ Tokenizer non trouvé: " tokenizer_path))
    
    ; Pré-déquantifier les embeddings (optimisation majeure)
    (println "")
    (println "4. Pré-déquantification des embeddings...")
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
            (println "   ✓ Embeddings déquantifiés et mis en cache")
            (println "     Shape: " (mlx_shape cached_embeddings)))
        (setq cached_embeddings (get_tensor weights_list tensor_index (+ embed_name ".weight")))
        (println "   ✓ Embeddings non quantifiés (pas de cache nécessaire)"))
    
    ; Pré-calculer les fréquences RoPE (optimisation)
    (println "")
    (println "5. Pré-calcul des fréquences RoPE...")
    (setq text_config (@ config "text_config"))
    (setq head_dim (@ text_config "head_dim"))
    (setq rope_theta (@ text_config "rope_theta"))
    (setq rope_local_base (@ text_config "rope_local_base_freq"))

    ; Calculer inv_freq pour les deux bases
    ; inv_freq = 1.0 / (base ** (arange(0, head_dim, 2) / head_dim))
    ; Note: rope_scaling avec factor=8.0 est déjà intégré dans rope_theta=1000000
    ; (base plus grand = fréquences plus basses = positions qui varient plus lentement)
    (setq indices (mlx_arange 0 head_dim 2 "float32"))
    (setq scaled (mlx_divide indices (mlx_array head_dim)))

    (setq rope_freqs_global (mlx_reciprocal (mlx_power (mlx_array rope_theta) scaled)))
    (setq rope_freqs_local (mlx_reciprocal (mlx_power (mlx_array rope_local_base) scaled)))

    (println "   ✓ Fréquences RoPE pré-calculées")
    (println "     Global (theta=" rope_theta "): " (mlx_shape rope_freqs_global))
    (println "     Local (base=" rope_local_base "): " (mlx_shape rope_freqs_local))
    
    ; Pré-extraire les tenseurs par couche (optimisation accès O(1))
    (println "")
    (println "6. Pré-extraction des tenseurs par couche...")
    (setq num_layers (@ text_config "num_hidden_layers"))
    (setq layer_tensors (build_layer_tensors weights_list tensor_index num_layers))
    (setq global_tensors (extract_global_tensors weights_list tensor_index))
    (println "   ✓ " num_layers " couches pré-indexées")
    (println "   ✓ Tenseurs globaux extraits")
    
    ; Pré-calculer les paramètres d'attention (optimisation majeure - évite 62 accès config par token)
    (println "")
    (println "7. Pré-calcul des paramètres d'attention...")
    (setq n_heads (@ text_config "num_attention_heads"))
    (setq n_kv_heads (@ text_config "num_key_value_heads"))
    (setq hidden_size (@ text_config "hidden_size"))
    (setq query_pre_attn_scalar (@ text_config "query_pre_attn_scalar"))
    (setq scale_float (/ 1.0 (sqrt query_pre_attn_scalar)))  ; float pour sdpa
    (setq eps (@ text_config "rms_norm_eps"))
    (setq repeats (/ n_heads n_kv_heads))
    (setq hidden_out (* n_heads head_dim))
    
    ; attn_params = (n_heads, n_kv_heads, head_dim, scale_float, eps, repeats, hidden_out)
    ; Les shapes (B, L, ...) sont calculées dynamiquement car L change entre prompt et génération
    (setq attn_params (list n_heads n_kv_heads head_dim scale_float eps repeats hidden_out))
    (println "   ✓ Paramètres d'attention pré-calculés")
    (println "     n_heads=" n_heads ", n_kv_heads=" n_kv_heads ", head_dim=" head_dim)
    (println "     scale=" scale_float ", eps=" eps ", repeats=" repeats)
    
    ; Créer l'objet modèle (kv_caches initialisé à nil, créé à la demande)
    (setq model (MLXModel weights_list tensor_index config metadata tokenizer cached_embeddings rope_freqs_global rope_freqs_local layer_tensors global_tensors attn_params nil))
    
    ; Afficher les stats
    (print_memory_stats)
    
    (println "")
    (println "✓ Modèle chargé avec succès!")
    
    model
)

; =============================================================================
; Programme principal
; =============================================================================

(println "")
(println "╔════════════════════════════════════════════════════════════╗")
(println "║  Chargeur de modèle MLX pour LispE                         ║")
(println "║  Gemma 3 27B IT QAT 4-bit                                  ║")
(println "╚════════════════════════════════════════════════════════════╝")
(println "")

; Charger le modèle
(setq chargement (elapse (setq model (load_mlx_model MODEL_PATH))))
(println "Temps de chargement:" chargement)

(check (nullp model)
    (println "")
    (println "ERREUR: Le modèle n'a pas pu être chargé")
    (exit 1))

; Afficher les informations du modèle
(println "")
(model MLXModel (info))

; Afficher quelques tenseurs
(print_tensor_info (@ model 'tensor_index) 30)

; Exemple d'accès aux poids
(println "")
(println "Exemple d'accès aux poids:")
(println (fill "-" 40))

; Vérifier si certains tenseurs existent
(setq test_names (strings
    "language_model.model.embed_tokens.weight"
    "language_model.model.layers.0.self_attn.q_proj.weight"
    "language_model.model.norm.weight"
))

(loop name test_names
    (if (model MLXModel (has_weight name))
        (println "  ✓ " name)
        (println "  ✗ " name " (non trouvé)")))

; Test du tokenizer
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
; Test simple du forward pass
; =============================================================================

; Vérifie la présence des poids nécessaires
(defun test_weights(model)
    (println "")
    (println "Vérification des poids du modèle...")
    (println (fill "-" 50))
    
    ; Utiliser la méthode get_weight de la classe
    ; Vérifier les poids clés
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
            (println "  ✗ " name " - NON TROUVÉ")))
    
    (println (fill "-" 50))
)

; =============================================================================
; Test de génération réel
; =============================================================================

; Test de génération avec une question simple
(defun test_generation(model (question "What is the capital of France?") (max_tokens 50))
    (println "")
    (println (fill "=" 60))
    (println "TEST DE GÉNÉRATION")
    (println (fill "=" 60))
    (println "Question: " question)
    (println "Max tokens: " max_tokens)
    (println "")
    
    ; Vérifier que le modèle est valide
    (check (nullp model)
        (println "ERREUR: Modèle non chargé")
        (return nil))
    
    ; Vérifier le tokenizer
    (check (not (model MLXModel (vocab_size)))
        (println "ERREUR: Tokenizer non disponible")
        (return nil))
    
    ; Formater le prompt pour le chat
    (setq prompt (model  MLXModel (format_prompt question)))
    (println "Prompt formaté:")
    (println prompt)
    (println (fill "-" 60))
    
    ; Générer la réponse avec la nouvelle API orientée objet
    (println "")
    (println "Réponse du modèle:")
    (println "")
    
    ; Mesurer le temps avec elapse
    (setq elapsed (elapse (setq response (model  MLXModel (chat question max_tokens 0.7)))))
    
    (println "")
    (println (fill "-" 60))
    (println "Temps de génération: " elapsed " ms")
    (print_memory_stats))

; =============================================================================
; Programme principal - Test
; =============================================================================

(println "")
(println "Le modèle est prêt à être utilisé!")
(println "Variable 'model' contient l'objet MLXModel")
(println "")
(println "Méthodes disponibles:")
(println "  - (model (encode \"texte\"))     ; Tokenizer")
(println "  - (model (decode tokens))      ; Détokenizer")
(println "  - (model (get_weight \"nom\"))  ; Accès aux poids")
(println "  - (model (info))               ; Informations modèle")
(println "  - (model (reset_cache))        ; Réinitialise KV cache")
(println "")
(println "Génération de texte:")
(println "  - (model (chat \"message\"))                    ; Chat simple")
(println "  - (model (chat \"message\" 256 0.7 \"system\"))  ; Chat avec options")
(println "  - (model (generate \"prompt\" max_tokens temp)) ; Génération brute")
(println "")
(println "Fonctions de test:")
(println "  - (test_weights model)         ; Vérifie les poids")
(println "  - (test_generation model)      ; Test génération réelle")
(println "")
(println "Exemple:")
(println "  (model (chat \"Bonjour, qui es-tu?\"))")
(println "")

; =============================================================================
; Test de génération
; =============================================================================

(println "")
(println "Lancement du test...")
(test_generation model "Write a short story about a robot who learns to paint." 100)
