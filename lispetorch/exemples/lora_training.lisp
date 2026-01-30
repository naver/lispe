; ==============================================
; FINE-TUNING LORA V2 - NOUVELLE API
; Utilise la nouvelle architecture HuggingFaceLoaderLoRA
; avec polymorphisme C++ et gestion automatique des adaptateurs
; ==============================================

(use 'lispe_torch)
(use 'lispe_tiktoken)

(printerrln "\n" (fill "=" 70))
(printerrln "  FINE-TUNING LORA V2 - Architecture Moderne")
(printerrln "  Modèle: Llama 3.1-8B | Dataset: Tamgu Programming Language")
(printerrln (fill "=" 70) "\n")

; ==============================================
; CONFIGURATION GLOBALE
; ==============================================

(setq model-path (+ _current "llama3.1-8B/model"))
(setq tiktoken-path (+ _current "llama3.1-8B/tokenizer"))
(setq dataset-path (+ _current "tamgu_dataset.json"))
(setq output-dir (+ _current "tamgu_lora_adapters_v2"))
(setq checkpoint-dir (+ _current "checkpoints_v2"))
(command (+ "mkdir -p " checkpoint-dir))
(command (+ "mkdir -p " output-dir))

; Configuration LoRA
(setq lora-config (dictionary
    "rank" 16
    "alpha" 32
    "target_modules" (strings "q_proj" "k_proj" "v_proj" "o_proj")
))

; Configuration d'entraînement
(setq training-config (dictionary
    "learning_rate" 2e-4
    "weight_decay" 0.01
    "num_epochs" 3
    "batch_size" 1
    "gradient_accumulation_steps" 4
    "max_seq_length" 256
    "logging_steps" 10
    "save_steps" 100
    "eval_steps" 50
    "warmup_steps" 100
    "max_grad_norm" 1.0
    "scheduler_type" "linear_warmup_cosine"
    "min_lr" 1e-6
    "device" "mps"  ; "mps" pour Apple Silicon, "cuda" pour NVIDIA, "cpu" sinon
))

; ==============================================
; CLASSE TOKENIZER TIKTOKEN
; ==============================================

(class@ TiktokenTokenizer (tokenizer_path init)
    (defun configure()
        (printerrln "📝 Configuration du tokenizer tiktoken...")
        (setq spec_tokens (json_parse (fread (+ tokenizer_path "/special_tokens_map.json"))))
        (setq tok_file (json_parse (fread (+ tokenizer_path "/tokenizer.json"))))
        (setqi bos_token (@ spec_tokens "bos_token" "content"))
        (setqi eos_token (@ spec_tokens "eos_token" "content"))
        (setq pattern (@ tok_file "pre_tokenizer" "pretokenizers" 0 "pattern" "Regex"))

        (setqi tokenizer_obj (tiktoken_create
            (@ tok_file "model" "vocab")
            (@ tok_file "added_tokens")
            pattern))

        (setqi bos_id (tiktoken_special_encode tokenizer_obj bos_token))
        (setqi eos_id (tiktoken_special_encode tokenizer_obj eos_token))
        (setqi pad_id 0)

        (printerrln "✓ Tokenizer configuré - vocab size:" (tiktoken_vocab_size tokenizer_obj))
    )

    (defun encode_chat_format(instruction response)
        (setq formatted_text (+
            "<|begin_of_text|>"
            "<|start_header_id|>system<|end_header_id|>\n\n"
            "You are a helpful assistant knowledgeable about programming languages."
            "<|eot_id|>"
            "<|start_header_id|>user<|end_header_id|>\n\n"
            instruction
            "<|eot_id|>"
            "<|start_header_id|>assistant<|end_header_id|>\n\n"
            response
            "<|eot_id|>"
        ))

        (setq tokens (tiktoken_encode tokenizer_obj formatted_text))
        (pushfirst tokens bos_id)

        (if (> (size tokens) (@ training-config "max_seq_length"))
            (setq tokens (@@ tokens 0 (@ training-config "max_seq_length")))
        )

        tokens
    )

    (defun pad_sequence(token_ids max_length)
        (setq current_length (size token_ids))
        (if (< current_length max_length)
            (block
                (setq padded (copy token_ids))
                (setq padding_needed (- max_length current_length))
                (setq padding (rho padding_needed (integers pad_id)))
                (extend padded padding)
                padded
            )
            (if (> current_length max_length)
                (@@ token_ids 0 max_length)
                token_ids
            )
        )
    )

    (defun decode(token_ids)
        (tiktoken_decode tokenizer_obj token_ids)
    )

    (defun decode_bytes(token_ids)
        (chrbyte (tiktoken_decode_bytes tokenizer_obj token_ids))
    )
)

; ==============================================
; CLASSE DATASET MANAGER
; ==============================================

(class@ DatasetManager (dataset_path tiktokenizer init)
    (defun configure()
        (printerrln "📚 Chargement du dataset...")
        (setq raw_data (json_parse (fread dataset_path)))
        (setqi samples (list))
        (setqi validation_samples (list))

        (setq total_size (size raw_data))
        (setq train_size (floor (* total_size 0.8)))

        (printerrln "⚙️  Préparation des échantillons...")
        (loopcount total_size i
            (setq sample (@ raw_data i))
            (setq instruction (@ sample "instruction"))
            (setq response (@ sample "response"))

            (setq tokens (tiktokenizer TiktokenTokenizer (encode_chat_format instruction response)))
            (setq token_tensor (tensor_create tokens))

            (if (< i train_size)
                (push samples token_tensor)
                (push validation_samples token_tensor)
            )

            ; Progress indicator
            (if (== (% i 100) 0)
                (printerr ".")
            )
        )
        (printerrln "")

        (printerrln "✓ Dataset préparé:")
        (printerrln "  • Train:" (size samples) "échantillons")
        (printerrln "  • Validation:" (size validation_samples) "échantillons")
    )

    (defun get_batch(start_idx batch_size)
        (setq batch (list))
        (setq end_idx (min (+ start_idx batch_size) (size samples)))

        (loopcount (- end_idx start_idx) i
            (push batch (@ samples (+ start_idx i)))
        )

        batch
    )

    (defun get_validation_batch(max_samples)
        (setq val_batch (list))
        (setq num_samples (min max_samples (size validation_samples)))

        (loopcount num_samples i
            (push val_batch (@ validation_samples i))
        )

        val_batch
    )

    (defun size_train()
        (size samples)
    )

    (defun size_validation()
        (size validation_samples)
    )
)

; ==============================================
; CLASSE LOSS CALCULATOR
; ==============================================

(class@ LossCalculator (init)
    (defun configure()
        (setqi loss_history (floats))
        (setqi loss_history_capacity 1000)
        true
    )

    (defun calculate_language_modeling_loss(logits input_tokens)
        (setq logits_shape (tensor_shape logits))
        (setq input_shape (tensor_shape input_tokens))

        (setq batch_size (@ logits_shape 0))
        (setq seq_len (@ logits_shape 1))
        (setq vocab_size (@ logits_shape 2))

        (check (> seq_len 1)
            ; Logits: [batch, seq_len, vocab] -> [batch, seq_len-1, vocab]
            ; Prédire le token suivant à chaque position
            (setq pred_logits (tensor_slice logits 1 0 (- seq_len 1)))
            

            ; Target: [batch, seq_len] -> [batch, seq_len-1]
            ; Les tokens à prédire (décalés de 1)
            (setq target_tokens (torch_to_mps . tensor_slice input_tokens 1 1 seq_len))

            ; Reshape pour cross_entropy: [batch * (seq_len-1), vocab]
            (setq pred_flat (tensor_reshape pred_logits (integers (* batch_size (- seq_len 1)) vocab_size)))
            (setq target_flat (tensor_reshape target_tokens (integers (* batch_size (- seq_len 1)))))

            ; S'assurer que les tenseurs sont contiguous et correctement alloués sur MPS
            ;(setq pred_flat (torch_to_mps (tensor_contiguous pred_flat)))
            ;(setq target_flat (torch_to_mps (tensor_contiguous target_flat)))

            ; Calculer la loss
            (setq loss (torch_cross_entropy pred_flat target_flat))

            ; Enregistrer l'historique
            (setq loss_value (@ loss 0))
            (push loss_history loss_value)

            (if (> (size loss_history) loss_history_capacity)
                (setqi loss_history (@@ loss_history
                    (- (size loss_history) loss_history_capacity)
                    (size loss_history)))
            )

            ; 🧹 SYNC OBLIGATOIRE : Les variables locales (pred_flat, target_flat, etc.)
            ; seront détruites au return (nettoyage pile), MAIS le GPU les utilise encore.
            ; On DOIT synchroniser AVANT que la pile soit nettoyée.
            (torch_mps_synchronize)

            loss
        )
    )

    (defun get_average_loss(last_n)
        (setq history_size (size loss_history))
        (if (== history_size 0)
            0.0
            (block
                (setq start_idx (max 0 (- history_size last_n)))
                (setq recent_losses (@@ loss_history start_idx history_size))
                (setq sum_val 0.0)
                (loop loss recent_losses
                    (setq sum_val (+ sum_val loss))
                )
                (/ sum_val (size recent_losses))
            )
        )
    )
)

; ==============================================
; CLASSE LORA TRAINER V2 - NOUVELLE API
; ==============================================

(class@ LoRATrainerV2 (model_path tokenizer_path dataset_path config)
    (defun configure()
        (printerrln "⚙️  Configuration du LoRA Trainer V2...")
        (toclean 'cleanup_trainer)

        (setqi model_name "llama31_lora")
        (setqi tiktokenizer nil)
        (setqi dataset_manager nil)
        (setqi loss_calculator nil)
        (setqi optimizer nil)
        (setqi scheduler nil)
        (setqi global_step 0)
        (setqi best_loss 1000.0)
        (setqi accumulation_step 0)

        true
    )

    (defun cleanup_trainer()
        (printerrln "🧹 Nettoyage du trainer...")
    )

    (defun load_model()
        (printerrln "\n🚀 Chargement du modèle avec LoRA...")

        ; ✅ NOUVELLE API: Charge le modèle + infrastructure LoRA
        (torch_hf_load_model_lora
            model_name
            model_path
            (dictionary "device" (@ config "device")))

        ; Afficher les infos du modèle
        (setq memory_usage (torch_hf_memory_usage model_name))
        (setq memory_gb (/ memory_usage 1073741824.0))

        (printerrln "✓ Modèle HuggingFace chargé avec support LoRA:")
        (printerrln "  • Nom:" model_name)
        (printerrln "  • Chemin:" model_path)
        (printerrln "  • Device:" (@ config "device"))
        (printerrln "  • Mémoire:" memory_gb "GB")

        true
    )

    (defun setup_lora()
        (printerrln "\n🎨 Initialisation des adaptateurs LoRA...")

        ; ✅ NOUVELLE API: Initialise les matrices LoRA A et B avec le dtype du modèle
        (torch_hf_lora_init
            model_name
            (@ lora-config "rank")
            (@ lora-config "alpha")
            (@ lora-config "target_modules")
            "bfloat16")

        (printerrln "✓ Adaptateurs LoRA initialisés:")
        (printerrln "  • Rank:" (@ lora-config "rank"))
        (printerrln "  • Alpha:" (@ lora-config "alpha"))
        (printerrln "  • Modules:" (@ lora-config "target_modules"))

        ; ✅ NOUVELLE API: Récupère les paramètres entraînables LoRA
        (setq lora_params (torch_hf_lora_get_parameters model_name))

        ; Dans setup_lora(), après ligne 338
        (setq lora_params (torch_hf_lora_get_parameters model_name))
        (printerrln "  • Paramètres LoRA:" (size lora_params) "tenseurs")

        ; Calculer le nombre total de paramètres
        (setq total_params 0)
        (loop param lora_params
            (setq param_shape (tensor_shape param))
            (setq param_size 1)
            (loop dim param_shape
                (setq param_size (* param_size dim))
            )
            (setq total_params (+ total_params param_size))
        )
        (setq total_params_m (/ total_params 1000000.0))
        (printerrln "  • Total paramètres entraînables:" (round (* total_params_m 100) 100) "M")

        lora_params
    )

    (defun setup_components()
        (printerrln "\n⚙️  Configuration des composants...")

        ; Tokenizer
        (setqi tiktokenizer (TiktokenTokenizer tokenizer_path (configure)))

        ; Dataset
        (setqi dataset_manager (DatasetManager dataset_path tiktokenizer (configure)))

        ; Loss calculator
        (setqi loss_calculator (LossCalculator (configure)))

        ; Setup LoRA et récupérer les paramètres
        (setq lora_params (setup_lora))

        ; ✅ Créer l'optimiseur AVEC les paramètres LoRA
        (printerrln "\n🔧 Configuration de l'optimiseur...")
        (setqi optimizer (torch_optimizer_add_params
            lora_params
            (@ config "learning_rate")
            (@ config "weight_decay")))

        (printerrln "✓ Optimiseur AdamW créé avec" (size lora_params) "paramètres LoRA")

        ; Scheduler
        (setq accum_steps (@ config "gradient_accumulation_steps"))
        (setq dataset_train_size (dataset_manager DatasetManager (size_train)))
        (setq total_steps (* (@ config "num_epochs")
                            (floor (/ dataset_train_size
                                     (* (@ config "batch_size") accum_steps)))))

        (setqi scheduler (torch_lr_scheduler optimizer (@ config "scheduler_type")
            (dictionary
                "initial_lr" (@ config "learning_rate")
                "min_lr" (@ config "min_lr")
                "total_steps" total_steps
                "warmup_steps" (@ config "warmup_steps")
            )
        ))

        (printerrln "✓ Composants configurés:")
        (printerrln "  • Scheduler:" (@ config "scheduler_type"))
        (printerrln "  • Total steps:" total_steps)
        (printerrln "  • Warmup steps:" (@ config "warmup_steps"))
        (printerrln "  • Gradient accumulation:" accum_steps "steps")

        true
    )

    (defun train_step(input_tensor is_accumulating)
        ; Zero grad au début d'une nouvelle accumulation
        (check (not is_accumulating)
            (torch_optimizer_zero_grad optimizer)
        )

        ; ✅ Forward pass - LoRA appliqué AUTOMATIQUEMENT via polymorphisme
        ; HuggingFaceLoaderLoRA::forward() appelle applyAttention() override
        ; qui ajoute les corrections LoRA: output = W_base(x) + LoRA(x)
        ; Ajouter dimension batch: (seq_len) -> (1, seq_len)
        (setq input_2d (tensor_reshape input_tensor (integers 1 -1)))
        (setq output (torch_hf_forward model_name input_2d))

        ; Calculer la loss
        (setq loss (loss_calculator LossCalculator
            (calculate_language_modeling_loss output input_2d)))

        (check loss
            ; Normaliser la loss par le nombre d'accumulations
            (setq accum_steps (@ config "gradient_accumulation_steps"))
            (setq scaled_loss (tensor_div loss . torch_to_mps . tensor_create . floats accum_steps))
            ; Backward sur la loss normalisée
            (torch_backward scaled_loss)

            ; 🧹 GESTION MÉMOIRE MPS : Synchroniser avant le return
            ; Les variables locales (input_2d, output, scaled_loss) seront
            ; automatiquement détruites au return (nettoyage pile)
            (torch_mps_synchronize)

            ; Retourner la loss originale (non-normalisée) - PROTÉGÉE
            loss
        )
    )

    (defun should_update_weights()
        ; Vérifie si on doit faire le step optimizer
        (setq accum_steps (@ config "gradient_accumulation_steps"))
        (setq should_update (== (% (+ accumulation_step 1) accum_steps) 0))


        (check should_update
            ; Gradient clipping
            (setq max_norm (@ config "max_grad_norm"))
            (check (> max_norm 0)
                (torch_clip_grad_norm optimizer max_norm)
            )

            ; Step optimizer et scheduler
            (torch_optimizer_step optimizer)
            (torch_scheduler_step scheduler)

            ; 🔍 DIAGNOSTIC: Nettoyer les gradients APRÈS l'update
            ; pour s'assurer qu'ils ne s'accumulent pas
            (torch_optimizer_zero_grad optimizer)

            ; Incrémenter le compteur global
            (+= global_step 1)
        )

        ; Incrémenter le compteur d'accumulation
        (+= accumulation_step 1)

        should_update
    )

    (defun validate()
        (printerrln "\n🔍 Validation en cours...")
        (setq total_val_loss 0.0)
        (setq valid_samples 0)

        ; Désactiver le calcul des gradients pendant la validation pour économiser la mémoire
        (torch_set_grad_enabled false)

        ; Traiter les échantillons de validation un par un pour éviter OOM (36GB RAM partagée)
        (setq num_val_samples 5)
        (loopcount num_val_samples i 
            ; Obtenir un seul échantillon à la fois
            (setq val_batch (dataset_manager DatasetManager (get_validation_batch 1)))
            (check (> (size val_batch) 0)
                (setq input_tensor (@ val_batch 0))
                ; Forward avec LoRA (reshape en 2D pour le modèle)
                (setq input_2d (tensor_reshape input_tensor (integers 1 -1)))
                (setq output (torch_hf_forward model_name input_2d))
                (setq loss (loss_calculator LossCalculator
                    (calculate_language_modeling_loss output input_2d)))
                (check loss
                    (setq total_val_loss (+ total_val_loss (@ loss 0)))
                    (setq valid_samples (+ valid_samples 1))
                )
            )

            ; 🧹 GESTION MÉMOIRE MPS : Synchroniser après chaque échantillon
            ; Crucial car les variables sont réutilisées à l'itération suivante.
            ; Sans sync, les tenseurs seraient détruits (réassignation) pendant que le GPU travaille.
            (torch_mps_synchronize)
        )

        ; Réactiver le calcul des gradients pour l'entraînement
        (torch_set_grad_enabled true)

        (setq avg_val_loss (if (> valid_samples 0)
            (/ total_val_loss valid_samples)
            0.0))
        (printerrln "✓ Validation loss:" (round (* avg_val_loss 10000) 10000))

        avg_val_loss
    )

    (defun save_checkpoint(epoch)
        (printerrln "\n💾 Sauvegarde du checkpoint epoch" epoch "...")

        (setq checkpoint_path (+ checkpoint-dir "/lora_adapters_epoch_" epoch ".pt"))

        ; ✅ NOUVELLE API: Sauvegarde seulement les adaptateurs LoRA (léger!)
        (torch_hf_lora_save model_name checkpoint_path)

        ; Sauvegarder les métadonnées
        (setq checkpoint_info (dictionary
            "epoch" epoch
            "global_step" global_step
            "best_loss" best_loss
            "current_lr" (torch_scheduler_get_lr scheduler)
            "lora_config" lora-config
            "training_config" config
        ))

        (setq info_file (+ checkpoint-dir "/training_info_epoch_" epoch ".json"))
        (fwrite info_file (json checkpoint_info))

        (printerrln "✓ Checkpoint sauvegardé:")
        (printerrln "  • Adaptateurs:" checkpoint_path)
        (printerrln "  • Métadonnées:" info_file)
    )

    (defun train()
        (printerrln "\n" (fill "=" 70))
        (printerrln "  🚀 DÉMARRAGE DE L'ENTRAÎNEMENT LORA V2")
        (printerrln (fill "=" 70))

        (setq num_epochs (@ config "num_epochs"))
        (setq batch_size (@ config "batch_size"))
        (setq accum_steps (@ config "gradient_accumulation_steps"))
        (setq logging_steps (@ config "logging_steps"))
        (setq save_steps (@ config "save_steps"))
        (setq eval_steps (@ config "eval_steps"))

        (setq train_size (dataset_manager DatasetManager (size_train)))
        (setq steps_per_epoch (floor (/ train_size batch_size)))
        (setq effective_steps_per_epoch (floor (/ steps_per_epoch accum_steps)))

        ; 💾 Monitoring mémoire initial
        (setq initial_memory_bytes (torch_hf_memory_usage model_name))
        (setq initial_memory_gb (/ initial_memory_bytes 1073741824.0))

        ; 🔍 DIAGNOSTIC: Taille du dataset en mémoire
        (printerrln "\n🔍 Diagnostic dataset:")
        (printerrln "  • Nombre d'échantillons:" train_size)
        (printerrln "  • Longueur max séquence:" (@ config "max_seq_length"))
        (setq dataset_mem_estimate (* train_size (@ config "max_seq_length") 4))
        (setq dataset_gb (/ dataset_mem_estimate 1073741824.0))
        (printerrln "  • Mémoire dataset estimée:" (round (* dataset_gb 100) 100) "GB")

        (printerrln "\n📊 Configuration:")
        (printerrln "  • Échantillons d'entraînement:" train_size)
        (printerrln "  • Epochs:" num_epochs)
        (printerrln "  • Batch size:" batch_size)
        (printerrln "  • Gradient accumulation:" accum_steps "steps")
        (printerrln "  • Effective batch size:" (* batch_size accum_steps))
        (printerrln "  • Steps par epoch:" steps_per_epoch)
        (printerrln "  • Effective steps par epoch:" effective_steps_per_epoch)
        (printerrln "  • Learning rate:" (@ config "learning_rate"))
        (printerrln "  • 💾 Tenseurs en mémoire:" (tensor_in_memory))
        (setq tempstotal 0)

        (loopcount num_epochs epoch
            (printerrln "\n" (fill "─" 70))
            (printerrln "📖 Epoch" (+ epoch 1) "/" num_epochs)
            (printerrln (fill "─" 70))

            (setq epoch_loss 0.0)
            (setq epoch_count 0)
            (setq temps 0)
            (setq memoire (memory_size))

            (loopcount steps_per_epoch step
                (+= temps
                    (elapse
                        (setq batch_start_idx (* step batch_size))
                        (setq batch (dataset_manager DatasetManager (get_batch batch_start_idx batch_size)))

                        ; Déterminer si on accumule
                        (setq is_accumulating (> (% accumulation_step accum_steps) 0))

                        (setq step_loss 0.0)
                        (setq step_count 0)

                        (loop input_tensor batch
                            (printerr ".")
                            (setq loss_tensor (train_step input_tensor is_accumulating))
                            (check loss_tensor
                                (setq loss_val (@ loss_tensor 0))
                                (setq step_loss (+ step_loss loss_val))
                                (setq step_count (+ step_count 1))
                            )
                            ; 🧹 Libérer explicitement le tenseur pour éviter l'accumulation
                            (setq loss_tensor nil)
                            (torch_mps_synchronize)
                        )
                        (setq avg_step_loss (if (> step_count 0)
                            (/ step_loss step_count)
                            0.0))

                        (+= epoch_loss avg_step_loss)
                        (+= epoch_count 1)

                        ; Mise à jour des poids
                        (setq did_update (should_update_weights))

                        ; Mettre à jour best_loss
                        (check (and (> avg_step_loss 0) (< avg_step_loss best_loss))
                            (setq best_loss avg_step_loss)
                        )
                    )
                )

                ; Logging avec monitoring mémoire
                (check (and did_update (== (% global_step logging_steps) 0))
                    (setq avg_recent (loss_calculator LossCalculator
                        (get_average_loss (* logging_steps accum_steps))))
                    (setq current_lr (torch_scheduler_get_lr scheduler))

                    ; 💾 Monitoring mémoire DÉTAILLÉ à chaque logging
                    (setq current_memory_bytes (torch_hf_memory_usage model_name))
                    (setq current_memory_gb (/ current_memory_bytes 1073741824.0))

                    ; 🔍 DIAGNOSTIC: Taille de loss_history
                    (setq history_size (size (@ loss_calculator 'loss_history)))
                    (+= tempstotal temps)

                    (printerrln "\n  📊 Step" global_step
                        "| Loss:" (round (* avg_step_loss 10000) 10000)
                        "| Avg:" (round (* avg_recent 10000) 10000)
                        "| Best:" (round (* best_loss 10000) 10000)
                        "| LR:" (round (* current_lr 1000000) 1000000)
                        "| Mémoire delta:" (- (memory_size) memoire)
                        "| 💾 NB Tenseurs:" (tensor_in_memory)
                        "| 💾 Temps:" (round (/ temps 1000)) "s"
                        "| 💾 Total:" (/ tempstotal 60000) "mn"
                        "| Hist:" history_size)
                    (setq temps 0)
                    (setq memoire (memory_size))
                )
                    
                ; Validation avec torch_set_grad_enabled pour économiser la mémoire
                (check (and did_update (== (% global_step eval_steps) 0))
                    (+= temps
                        (elapse (validate)))
                    ; 🧹 Vider le cache MPS après validation
                )

                ; Sauvegarde
                (check (and did_update (== (% global_step save_steps) 0))
                    (save_checkpoint (+ epoch 1))
                    ; 🧹 Vider le cache MPS après sauvegarde
                )
            )

            (setq avg_epoch_loss (/ epoch_loss epoch_count))

            ; 💾 Monitoring mémoire fin d'epoch
            (setq epoch_memory_bytes (torch_hf_memory_usage model_name))
            (setq epoch_memory_gb (/ epoch_memory_bytes 1073741824.0))

            (printerrln "\n✓ Epoch" (+ epoch 1) "terminée - Loss moyenne:"
                (round (* avg_epoch_loss 10000) 10000)
                "| Mémoire:" (round (* epoch_memory_gb 100) 100) "GB")

            ; 🧹 Vider le cache MPS à la fin de chaque epoch
            (torch_mps_synchronize)
        )

        ; Sauvegarde finale
        (save_checkpoint num_epochs)

        (setq final_avg_loss (loss_calculator LossCalculator (get_average_loss 100)))
        (setq final_lr (torch_scheduler_get_lr scheduler))

        ; 🧹 Vider le cache MPS avant le monitoring final


        (printerrln "\n" (fill "=" 70))
        (printerrln "  🎉 ENTRAÎNEMENT TERMINÉ AVEC SUCCÈS")
        (printerrln (fill "=" 70))
        (printerrln "\n📈 Statistiques finales:")
        (printerrln "  • Total steps:" global_step)
        (printerrln "  • Meilleure loss:" (round (* best_loss 10000) 10000))
        (printerrln "  • Loss finale (avg 100):" (round (* final_avg_loss 10000) 10000))
        (printerrln "  • Learning rate final:" (round (* final_lr 1000000) 1000000))

        true
    )
)

; ==============================================
; FONCTION PRINCIPALE
; ==============================================

(printerrln "🚀 Initialisation du fine-tuning LoRA V2...\n")

(setq trainer (LoRATrainerV2
    model-path
    tiktoken-path
    dataset-path
    training-config))

(withclass LoRATrainerV2
    (trainer (configure))
    (if (trainer (load_model))
        (if (trainer (setup_components))
            (if (trainer (train))
                (printerrln "\n✅ Fine-tuning LoRA V2 terminé avec succès! 🎉")
                (printerrln "\n❌ Échec de l'entraînement")
            )
            (printerrln "\n❌ Échec configuration des composants")
        )
        (printerrln "\n❌ Échec chargement du modèle")
    )
)
