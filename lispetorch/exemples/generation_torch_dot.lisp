#!/usr/bin/env lispe
; Test du KV-Cache pour génération autoregressive
; Compare les performances avec et sans KV-Cache

(use 'lispe_torch)
(use 'lispe_tiktoken)

(setq model_path (+ _current "llama3.1-8B/model"))
(setq tok_path (+ _current "/llama3.1-8B/tokenizer"))

(class@ Tokeniser (tok_path init)
   (defun config()
         ; Charger le tokenizer
      (println "\n[2/4] Chargement du tokenizer...")
      (setq spec_tokens (json_parse (fread (+ tok_path "/special_tokens_map.json"))))
      (setq tok_file (json_parse (fread (+ tok_path "/tokenizer.json"))))

      ; Récupérer les tokens spéciaux
      (setq bos_token (@ spec_tokens "bos_token" "content"))
      (setq eos_token (@ spec_tokens "eos_token" "content"))
      (setq pattern (@ tok_file "pre_tokenizer" "pretokenizers" 0 "pattern" "Regex"))

      ; Créer le tokenizer
      (setqi tokenizer (tiktoken_create
         (@ tok_file "model" "vocab")
         (@ tok_file "added_tokens")
         pattern))

      ; Récupérer les IDs des tokens spéciaux
      (setqi bos_id (tiktoken_special_encode tokenizer bos_token))
      (setqi eos_id (tiktoken_special_encode tokenizer eos_token))
   )
   (defun encode(prompt)
      (pushfirst (tiktoken_encode tokenizer prompt) bos_id)
   )

   (defun decode(token_id)
      (tiktoken_decode tokenizer (integers token_id))
   )
)

(setq prompt `Problem: A lady goes to a hotel for the weekend and takes a room for 3 nights.
She arrives at the hotel in the evening and decides to upgrade to a suite for an additional 20€ per night.
She also orders room service for dinner on the first night, which costs 30€.
The next day, she goes to the hotel spa and gets a massage for 60€.
On the last day, she checks out of the hotel early and doesn't take a breakfast.
A normal night in the hotel is 50€, a breakfast is 10€, and there is a tax of 10% on all charges.
`)

(setq max_tokens 150)
(setq temperature 0.3)
(setq top_k 50)
(setq top_p 0.9)

(println "=" (fill "=" 60))
(println "TEST KV-CACHE - LLAMA 3.1-8B")
(println "=" (fill "=" 60))

; Charger le modèle
(println "\n[1/4] Chargement du modèle...")
(setq config (dictionary "device" "mps"))
(setq load_time
   (elapse
      (setq model_id (torch_hf_load_model model_path config))
   )
)
(println "✓ Modèle chargé en " (/ load_time 1000.0) "s (" load_time "ms)")


; We load the tokenizer
(setq tok (Tokeniser tok_path (config)))

; Encoder le prompt
(setq prompt_tokens (tok (encode prompt)))
(println "✓ Prompt tokenisé: " (size prompt_tokens) " tokens")

(defun kv-avec-cache (model_path prompt_tokens max_tokens temperature eos_id kv-cache)
   ; === TEST 2: AVEC KV-CACHE ===
   (println "\n" (fill "=" 60))
   (println "TEST 2: GÉNÉRATION AVEC KV-CACHE")
   (println (fill "=" 60))

   ; Reset la génération
   (setq generated_tokens (clone prompt_tokens))
   (setq current_input (tensor_unsqueeze (torch_to_mps (tensor_create (clone prompt_tokens))) 0))
   (setq time_with_cache
      (elapse
         (loopcount max_tokens  i
            ; Pour le premier passage: envoyer tout le prompt
            ; Pour les suivants: envoyer seulement le dernier token
            ; Forward pass (utilisera automatiquement le cache)
            (setq logits (torch_hf_forward model_path current_input kv-cache))
            (setq logits_shape (tensor_shape logits))

            ; Extraire logits du dernier token
            (setq last_logits (tensor_select logits 1 (- (@ logits_shape 1) 1)))
            (setq last_logits (tensor_select last_logits 0 0))
            
            ; Appliquer répétition penalty pour éviter les boucles
            (setq recent_tokens (@@ generated_tokens (max 0 (- (size generated_tokens) 20)) 0))
            (loop token recent_tokens
               (setq token_logit (tensor_select last_logits 0 token))
               (setq penalized_logit (tensor_mul_scalar token_logit 0.85))
               (torch_index_put_ last_logits (integers token) penalized_logit)
            )
            
            ; Appliquer température
            (setq last_logits (torch_div_scalar last_logits temperature))
            
            ; Top-k filtering
            (setq sorted_logits (torch_sort last_logits true))
            (setq top_k_values (@ sorted_logits 0))
            (setq top_k_indices (@ sorted_logits 1))
            
            ; Garder seulement les top-k tokens
            (setq k_threshold (tensor_select top_k_values 0 (min (- top_k 1) (- (torch_numel top_k_values) 1))))
            (setq mask (torch_ge last_logits k_threshold))
            (setq filtered_logits (torch_where mask last_logits (torch_full_like last_logits -1e9)))

            ; Sampling avec probabilités filtrées
            (setq probs (torch_softmax filtered_logits -1))
            (setq next_token_tensor (torch_multinomial probs 1 true))
            (setq next_token_id (torch_item next_token_tensor))

            (printerr  (tok (decode next_token_id)))
            ; Ajouter à la séquence
            (push generated_tokens next_token_id)

            ; Vérifier EOS
            (check (eq next_token_id eos_id)
               (break)
            )
            
            ; Détecter les boucles de répétition
            (check (> (size generated_tokens) 10)
               (setq last_5_tokens (@@ generated_tokens (- (size generated_tokens) 5) 0))
               (setq prev_5_tokens (@@ generated_tokens (- (size generated_tokens) 10) (- (size generated_tokens) 5)))
               (check (eq last_5_tokens prev_5_tokens)
                  (println "\n⚠ Boucle de répétition détectée, arrêt de la génération")
                  (break)
               )
            )
            (setq current_input (torch_unsqueeze (torch_to_mps (tensor_create (integers next_token_id))) 0))
         )
      )
   )

   (println)
   ;(printerrln (tiktoken_decode tokenizer (@@ generated_tokens 1 0)))


   (setq tokens_generated_cache (- (size generated_tokens) (size prompt_tokens)))

   (println "✓ Résultats AVEC cache:")
   (println "  Tokens générés: " tokens_generated_cache)
   (println "  Temps total: " time_with_cache "ms")
   (println "  ms/token: " (/ time_with_cache tokens_generated_cache))
   (println "  tokens/s: " (/ (* tokens_generated_cache 1000.0) time_with_cache))
)

(println (fill "=" 60))
(setq kv_cache (torch_hf_enable_kv_cache model_path true))

(kv-avec-cache model_path prompt_tokens max_tokens temperature (@ tok 'eos_id) kv_cache)
(println (fill "=" 60))

