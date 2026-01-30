(use 'lispe_torch)

(defun display(w tok)
    (printerr (tok (decode . integers w)))
)

(class@ Model (model_path config tok init)
    (defun configure()
      (setqi model_id (torch_hf_load_model model_path config))
      (setqi temperature 0.3)
      (setqi eosid (tok (eos)))
    )

    (defun generate_sans(prompt max_tokens)
        ; === TEST 1: SANS KV-CACHE ===
        (torch_hf_enable_kv_cache model_id false)
        (println "\n" (fill "=" 60))
        (println "TEST 1: GÉNÉRATION SANS KV-CACHE")
        (println (fill "=" 60))
        (print "Texte: " prompt)

        (setq prompt_tokens (tok (encode prompt)))

        (setq generated_tokens (clone prompt_tokens))

        (setq time_without_cache
            (elapse
                (loopcount max_tokens i
                    ; Forward pass avec TOUTE la séquence
                    (setq current_input (tensor_unsqueeze (torch_to_mps (tensor_create (clone generated_tokens))) 0))
                    (setq logits (torch_hf_forward model_id current_input))
                    (setq logits_shape (tensor_shape logits))

                    ; Extraire logits du dernier token
                    (setq last_logits (tensor_select logits 1 (- (@ logits_shape 1) 1)))
                    (setq last_logits (tensor_select last_logits 0 0))

                    ; Argmax (greedy) pour test
                    (setq next_token_id (tensor_item (tensor_argmax last_logits -1)))

                    ; Afficher le token généré
                    (printerr (tok (decode (integers next_token_id))))

                    ; Ajouter à la séquence
                    (push generated_tokens next_token_id)

                    ; Vérifier EOS
                    (check (in eosid next_token_id)
                        (break)
                    )
                )
            )
        )

        (println "")  ; Retour à la ligne après la génération

        (setq tokens_generated (- (size generated_tokens) (size prompt_tokens)))

        (println "✓ Résultats SANS cache:")
        (println "  Tokens générés: " tokens_generated)
        (println "  Temps total: " time_without_cache "ms")
        (println "  ms/token: " (/ time_without_cache tokens_generated))
        (println "  tokens/s: " (/ (* tokens_generated 1000.0) time_without_cache))
    )

        
    (defun generate(prompt max_tokens)
        ; Activer le KV-Cache
        (setq prompt_tokens (tok (encode prompt)))

        (setq context_id (torch_hf_enable_kv_cache model_id true))
        (print "Texte: " prompt)

        (setq generated_tokens (clone prompt_tokens))
        (setq current_input (tensor_unsqueeze (torch_to_mps (tensor_create (clone prompt_tokens))) 0))
        (setq generated ())

        (setq time_with_cache
            (elapse
                (loopcount max_tokens  i
                    ; Pour le premier passage: envoyer tout le prompt
                    ; Pour les suivants: envoyer seulement le dernier token
                    ; Forward pass (utilisera automatiquement le cache)
                    (setq logits (torch_hf_forward_manual model_id current_input context_id))
                    (setq logits_shape (tensor_shape logits))

                    ; Extraire logits du dernier token
                    (setq last_logits (tensor_select logits 1 (- (@ logits_shape 1) 1)))
                    (setq last_logits (tensor_select last_logits 0 0))
                    (setq last_logits (tensor_div_scalar last_logits temperature))

                    ; Sampling
                    (setq probs (tensor_softmax last_logits -1))
                    (setq next_token_tensor (tensor_multinomial probs 1 true))
                    (setq next_token_id (tensor_item next_token_tensor))

                    ; Afficher le token généré
                    (setq tok_display (tok (decode (integers next_token_id))))
                    (printerr tok_display)
                    ;(push generated (list tok_display next_token_id))

                    ; Ajouter à la séquence
                    (push generated_tokens next_token_id)

                    ; Vérifier EOS
                    (check (in eosid next_token_id)
                        (break)
                    )
                    (setq current_input (tensor_unsqueeze (torch_to_mps (tensor_create (integers next_token_id))) 0))
                )
            )
        )

        (println "")  ; Retour à la ligne après la génération

        (setq tokens_generated_cache (- (size generated_tokens) (size prompt_tokens)))

        (println "✓ Résultats AVEC cache:")
        (println "  Tokens générés: " tokens_generated_cache)
        (println "  Temps total: " time_with_cache "ms")
        (println "  ms/token: " (/ time_with_cache tokens_generated_cache))
        (println "  tokens/s: " (/ (* tokens_generated_cache 1000.0) time_with_cache))
    )

    (defun generate_interne(prompt size_sequence)
        (setq options 
            (dictionary
                "topk" 30
                "callback" 'display
                "data" tok
            )
        )

        (setq prompt_tokens (tok (encode prompt)))
        (setq time_with_cache 
            (elapse 
                (setq generated_tokens 
                    (torch_hf_generate model_id prompt_tokens (tok (eos)) size_sequence)
                )
            )
        )

        (setq tokens_generated_cache (- (size generated_tokens) (size prompt_tokens)))

        (println (tok (decode generated_tokens)))
        
        (println "✓ Résultats AVEC cache:")
        (println "  Tokens générés: " tokens_generated_cache)
        (println "  Temps total: " time_with_cache "ms")
        (println "  ms/token: " (/ time_with_cache tokens_generated_cache))
        (println "  tokens/s: " (/ (* tokens_generated_cache 1000.0) time_with_cache))

    )
)
