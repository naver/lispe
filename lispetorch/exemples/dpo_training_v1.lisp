; ==============================================
; DPO TRAINING V1 - Direct Preference Optimization
; Utilise l'architecture HuggingFaceLoaderLoRA avec DPO loss
; Basé sur lora_training_v2.lisp
; ==============================================

(use 'lispe_torch)
(use 'lispe_tiktoken)

(printerrln "\n" (fill "=" 70))
(printerrln "  DPO TRAINING V1 - Direct Preference Optimization")
(printerrln "  Modèle: Llama 3.1-8B | Alignment sur préférences humaines")
(printerrln (fill "=" 70) "\n")

; ==============================================
; CONFIGURATION GLOBALE
; ==============================================

(setq model-path (+ _current "llama3.1-8B/model"))
(setq tiktoken-path (+ _current "llama3.1-8B/tokenizer"))
(setq dataset-path (+ _current "dpo_preferences_dataset.json"))
(setq sft-model-path (+ _current "tamgu_lora_adapters_v2"))  ; Modèle après fine-tuning
(setq output-dir (+ _current "tamgu_dpo_adapters_v1"))
(setq checkpoint-dir (+ _current "checkpoints_dpo_v1"))
(command (+ "mkdir -p " checkpoint-dir))
(command (+ "mkdir -p " output-dir))

; Configuration LoRA (même config que le SFT pour compatibilité)
(setq lora-config (dictionary
    "rank" 16
    "alpha" 32
    "target_modules" (strings "q_proj" "k_proj" "v_proj" "o_proj")
))

; Configuration DPO
(setq dpo-config (dictionary
    "beta" 0.1              ; Coefficient de pénalité KL
    "learning_rate" 5e-5    ; Plus petit que SFT
    "weight_decay" 0.01
    "num_epochs" 1          ; DPO converge rapidement
    "batch_size" 1
    "gradient_accumulation_steps" 4
    "max_seq_length" 256
    "logging_steps" 5
    "save_steps" 50
    "eval_steps" 25
    "warmup_steps" 50
    "max_grad_norm" 1.0
    "scheduler_type" "linear_warmup_cosine"
    "min_lr" 1e-6
    "device" "mps"
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

        (if (> (size tokens) (@ dpo-config "max_seq_length"))
            (setq tokens (@@ tokens 0 (@ dpo-config "max_seq_length")))
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
)

; ==============================================
; CLASSE DPO DATASET MANAGER
; ==============================================

(class@ DPODatasetManager (dataset_path tiktokenizer init)
    (defun configure()
        (printerrln "📚 Chargement du dataset DPO...")
        (setq raw_data (json_parse (fread dataset_path)))
        (setqi samples (list))
        (setqi validation_samples (list))

        (setq total_size (size raw_data))
        (setq train_size (floor (* total_size 0.8)))

        (printerrln "⚙️  Préparation des paires de préférences...")
        (loopcount total_size i
            (setq sample (@ raw_data i))
            (setq prompt (@ sample "prompt"))
            (setq chosen (@ sample "chosen"))
            (setq rejected (@ sample "rejected"))

            ; Encoder les deux réponses
            (setq chosen_tokens (tiktokenizer TiktokenTokenizer (encode_chat_format prompt chosen)))
            (setq rejected_tokens (tiktokenizer TiktokenTokenizer (encode_chat_format prompt rejected)))

            ; Créer une paire
            (setq pair (dictionary
                "chosen" (tensor_create chosen_tokens)
                "rejected" (tensor_create rejected_tokens)
            ))

            (if (< i train_size)
                (push samples pair)
                (push validation_samples pair)
            )

            ; Progress indicator
            (if (== (% i 50) 0)
                (printerr ".")
            )
        )
        (printerrln "")

        (printerrln "✓ Dataset DPO préparé:")
        (printerrln "  • Train:" (size samples) "paires")
        (printerrln "  • Validation:" (size validation_samples) "paires")
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
; CLASSE DPO LOSS CALCULATOR
; ==============================================

(class@ DPOLossCalculator (beta init)
    (defun configure()
        (setqi beta_value beta)
        (setqi loss_history (floats))
        (setqi accuracy_history (floats))
        (setqi reward_margin_history (floats))
        (printerrln "✓ DPO Loss Calculator configuré - beta:" beta_value)
        true
    )

    ; Calcule la log-probabilité moyenne d'une séquence
    ; logits: [batch, seq_len, vocab_size]
    ; tokens: [batch, seq_len]
    (defun compute_sequence_logprob(logits tokens)
        (setq logits_shape (tensor_shape logits))
        (setq batch_size (@ logits_shape 0))
        (setq seq_len (@ logits_shape 1))
        (setq vocab_size (@ logits_shape 2))

        ; Logits pour prédire le prochain token: [batch, seq_len-1, vocab]
        (setq pred_logits (tensor_slice logits 1 0 (- seq_len 1)))

        ; Tokens cibles (décalés): [batch, seq_len-1]
        (setq target_tokens (tensor_slice tokens 1 1 seq_len))

        ; Calculer log_softmax sur les logits: [batch, seq_len-1, vocab]
        (setq log_probs (tensor_log_softmax pred_logits 2))

        ; Gather les log-probs des tokens cibles
        (setq batch_seq_len (* batch_size (- seq_len 1)))

        ; Reshape log_probs: [batch * (seq_len-1), vocab]
        (setq log_probs_flat (tensor_reshape log_probs
            (integers batch_seq_len vocab_size)))

        ; Reshape targets: [batch * (seq_len-1)]
        (setq targets_flat (tensor_reshape target_tokens
            (integers batch_seq_len)))

        ; Convertir targets en long pour gather
        (setq targets_flat (torch_to_mps (tensor_contiguous targets_flat)))

        ; Unsqueeze targets pour gather: [batch * (seq_len-1), 1]
        (setq targets_unsqueezed (tensor_unsqueeze targets_flat 1))

        ; Gather: [batch * (seq_len-1), 1]
        (setq selected_log_probs (tensor_gather log_probs_flat 1 targets_unsqueezed))

        ; Squeeze: [batch * (seq_len-1)]
        (setq selected_log_probs (tensor_squeeze selected_log_probs 1))

        ; Reshape back: [batch, seq_len-1]
        (setq selected_log_probs (tensor_reshape selected_log_probs
            (integers batch_size (- seq_len 1))))

        ; Moyenne sur la séquence (dimension 1): [batch]
        (setq mean_log_prob (tensor_mean_dim selected_log_probs 1))

        ; Moyenne sur le batch: scalaire
        (setq final_log_prob (tensor_mean mean_log_prob))

        ; Synchroniser avant le return
        (torch_mps_synchronize)

        final_log_prob
    )

    ; Calcule la loss DPO
    ; Loss = -log(σ(β * (log π_θ(y_w|x)/π_ref(y_w|x) - log π_θ(y_l|x)/π_ref(y_l|x))))
    (defun calculate_dpo_loss(policy_chosen_logprob policy_rejected_logprob
                              reference_chosen_logprob reference_rejected_logprob)

        ; Calcul des log-ratios
        ; log_ratio_chosen = log π_θ(y_w|x) - log π_ref(y_w|x)
        (setq log_ratio_chosen (tensor_sub policy_chosen_logprob reference_chosen_logprob))

        ; log_ratio_rejected = log π_θ(y_l|x) - log π_ref(y_l|x)
        (setq log_ratio_rejected (tensor_sub policy_rejected_logprob reference_rejected_logprob))

        ; Différence des ratios (reward margin)
        ; logits = β * (log_ratio_chosen - log_ratio_rejected)
        (setq logits_diff (tensor_sub log_ratio_chosen log_ratio_rejected))
        (setq scaled_logits (tensor_mul_scalar logits_diff beta_value))

        ; Calculer -log(sigmoid(scaled_logits))
        ; Formule stable: -log(sigmoid(x)) = log(1 + exp(-x))
        ; = softplus(-x)
        (setq neg_logits (tensor_mul_scalar scaled_logits -1.0))
        (setq exp_neg_logits (tensor_exp neg_logits))
        (setq one_plus_exp (tensor_add_scalar exp_neg_logits 1.0))
        (setq loss (tensor_log one_plus_exp))

        ; Calculer les métriques
        ; Accuracy implicite: est-ce que le modèle préfère chosen > rejected ?
        (setq reward_margin (@ scaled_logits 0))
        (setq accuracy (if (> reward_margin 0.0) 1.0 0.0))

        ; Enregistrer dans l'historique
        (setq loss_value (@ loss 0))
        (push loss_history loss_value)
        (push accuracy_history accuracy)
        (push reward_margin_history reward_margin)

        ; Limiter la taille de l'historique
        (if (> (size loss_history) 1000)
            (block
                (setqi loss_history (@@ loss_history (- (size loss_history) 1000) (size loss_history)))
                (setqi accuracy_history (@@ accuracy_history (- (size accuracy_history) 1000) (size accuracy_history)))
                (setqi reward_margin_history (@@ reward_margin_history (- (size reward_margin_history) 1000) (size reward_margin_history)))
            )
        )

        ; Synchroniser avant le return
        (torch_mps_synchronize)

        loss
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

    (defun get_average_accuracy(last_n)
        (setq history_size (size accuracy_history))
        (if (== history_size 0)
            0.0
            (block
                (setq start_idx (max 0 (- history_size last_n)))
                (setq recent_acc (@@ accuracy_history start_idx history_size))
                (setq sum_val 0.0)
                (loop acc recent_acc
                    (setq sum_val (+ sum_val acc))
                )
                (/ sum_val (size recent_acc))
            )
        )
    )

    (defun get_average_reward_margin(last_n)
        (setq history_size (size reward_margin_history))
        (if (== history_size 0)
            0.0
            (block
                (setq start_idx (max 0 (- history_size last_n)))
                (setq recent_margins (@@ reward_margin_history start_idx history_size))
                (setq sum_val 0.0)
                (loop margin recent_margins
                    (setq sum_val (+ sum_val margin))
                )
                (/ sum_val (size recent_margins))
            )
        )
    )
)

; ==============================================
; CLASSE DPO TRAINER V1
; ==============================================

(class@ DPOTrainerV1 (model_path tokenizer_path dataset_path config)
    (defun configure()
        (printerrln "⚙️  Configuration du DPO Trainer V1...")
        (toclean 'cleanup_trainer)

        (setqi policy_model_name "llama31_dpo_policy")
        (setqi reference_model_name "llama31_dpo_reference")
        (setqi tiktokenizer nil)
        (setqi dataset_manager nil)
        (setqi dpo_loss_calculator nil)
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

    (defun load_models()
        (printerrln "\n🚀 Chargement des modèles...")

        ; ✅ POLICY MODEL: Charge le modèle + LoRA (entraînable)
        (printerrln "  • Chargement du policy model avec LoRA...")
        (torch_hf_load_model_lora
            policy_model_name
            model_path
            (dictionary "device" (@ config "device")))

        ; ✅ REFERENCE MODEL: Charge le modèle de référence (frozen)
        (printerrln "  • Chargement du reference model (frozen)...")
        (torch_hf_load_model_lora
            reference_model_name
            model_path
            (dictionary "device" (@ config "device")))

        ; Afficher les infos
        (setq policy_memory (torch_hf_memory_usage policy_model_name))
        (setq ref_memory (torch_hf_memory_usage reference_model_name))
        (setq total_memory_gb (/ (+ policy_memory ref_memory) 1073741824.0))

        (printerrln "✓ Modèles chargés:")
        (printerrln "  • Policy model:" policy_model_name)
        (printerrln "  • Reference model:" reference_model_name)
        (printerrln "  • Device:" (@ config "device"))
        (printerrln "  • Mémoire totale:" (round (* total_memory_gb 100) 100) "GB")

        true
    )

    (defun setup_lora()
        (printerrln "\n🎨 Initialisation des adaptateurs LoRA...")

        ; ✅ Initialiser LoRA UNIQUEMENT pour le policy model
        (torch_hf_lora_init
            policy_model_name
            (@ lora-config "rank")
            (@ lora-config "alpha")
            (@ lora-config "target_modules")
            "bfloat16")

        ; ✅ Initialiser LoRA pour le reference model (mais ne sera pas entraîné)
        (torch_hf_lora_init
            reference_model_name
            (@ lora-config "rank")
            (@ lora-config "alpha")
            (@ lora-config "target_modules")
            "bfloat16")

        ; Si un checkpoint SFT existe, le charger dans les DEUX modèles
        (check (file_exists sft-model-path)
            (printerrln "\n📥 Chargement du checkpoint SFT...")
            (torch_hf_lora_load policy_model_name (+ sft-model-path "/lora_adapters_epoch_3.pt"))
            (torch_hf_lora_load reference_model_name (+ sft-model-path "/lora_adapters_epoch_3.pt"))
            (printerrln "✓ Checkpoint SFT chargé dans policy et reference models")
        )

        (printerrln "✓ Adaptateurs LoRA initialisés:")
        (printerrln "  • Rank:" (@ lora-config "rank"))
        (printerrln "  • Alpha:" (@ lora-config "alpha"))
        (printerrln "  • Modules:" (@ lora-config "target_modules"))

        ; ✅ Récupère les paramètres entraînables du policy model UNIQUEMENT
        (setq lora_params (torch_hf_lora_get_parameters policy_model_name))

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
        (setqi dataset_manager (DPODatasetManager dataset_path tiktokenizer (configure)))

        ; DPO Loss calculator
        (setqi dpo_loss_calculator (DPOLossCalculator (@ config "beta") (configure)))

        ; Setup LoRA et récupérer les paramètres
        (setq lora_params (setup_lora))

        ; ✅ Créer l'optimiseur AVEC les paramètres LoRA du policy model
        (printerrln "\n🔧 Configuration de l'optimiseur...")
        (setqi optimizer (torch_optimizer_add_params
            lora_params
            (@ config "learning_rate")
            (@ config "weight_decay")))

        (printerrln "✓ Optimiseur AdamW créé avec" (size lora_params) "paramètres LoRA")

        ; Scheduler
        (setq accum_steps (@ config "gradient_accumulation_steps"))
        (setq dataset_train_size (dataset_manager DPODatasetManager (size_train)))
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

    (defun dpo_train_step(chosen_tensor rejected_tensor is_accumulating)
        ; Zero grad au début d'une nouvelle accumulation
        (check (not is_accumulating)
            (torch_optimizer_zero_grad optimizer)
        )

        ; Reshape pour batch dimension: (seq_len) -> (1, seq_len)
        (setq chosen_2d (tensor_reshape chosen_tensor (integers 1 -1)))
        (setq rejected_2d (tensor_reshape rejected_tensor (integers 1 -1)))

        ; ========== POLICY MODEL (θ) - avec gradients ==========
        (torch_set_grad_enabled true)

        (setq policy_chosen_logits (torch_hf_forward policy_model_name chosen_2d))
        (setq policy_rejected_logits (torch_hf_forward policy_model_name rejected_2d))

        ; Calculer les log-probs pour le policy model
        (setq policy_chosen_logprob (dpo_loss_calculator DPOLossCalculator
            (compute_sequence_logprob policy_chosen_logits chosen_2d)))
        (setq policy_rejected_logprob (dpo_loss_calculator DPOLossCalculator
            (compute_sequence_logprob policy_rejected_logits rejected_2d)))

        ; ========== REFERENCE MODEL (π_ref) - sans gradients ==========
        (torch_set_grad_enabled false)

        (setq ref_chosen_logits (torch_hf_forward reference_model_name chosen_2d))
        (setq ref_rejected_logits (torch_hf_forward reference_model_name rejected_2d))

        ; Calculer les log-probs pour le reference model
        (setq ref_chosen_logprob (dpo_loss_calculator DPOLossCalculator
            (compute_sequence_logprob ref_chosen_logits chosen_2d)))
        (setq ref_rejected_logprob (dpo_loss_calculator DPOLossCalculator
            (compute_sequence_logprob ref_rejected_logits rejected_2d)))

        ; Réactiver les gradients
        (torch_set_grad_enabled true)

        ; ========== CALCULER LA LOSS DPO ==========
        (setq dpo_loss (dpo_loss_calculator DPOLossCalculator
            (calculate_dpo_loss
                policy_chosen_logprob
                policy_rejected_logprob
                ref_chosen_logprob
                ref_rejected_logprob)))

        (check dpo_loss
            ; Normaliser la loss par le nombre d'accumulations
            (setq accum_steps (@ config "gradient_accumulation_steps"))
            (setq scaled_loss (tensor_div dpo_loss . torch_to_mps . tensor_create . floats accum_steps))

            ; Backward sur la loss normalisée
            (torch_backward scaled_loss)

            ; 🧹 GESTION MÉMOIRE MPS
            (torch_mps_synchronize)

            ; Retourner la loss originale (non-normalisée)
            dpo_loss
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

            ; Nettoyer les gradients APRÈS l'update
            (torch_optimizer_zero_grad optimizer)

            ; Incrémenter le compteur global
            (+= global_step 1)
        )

        ; Incrémenter le compteur d'accumulation
        (+= accumulation_step 1)

        should_update
    )

    (defun validate()
        (printerrln "\n🔍 Validation DPO en cours...")
        (setq total_val_loss 0.0)
        (setq total_val_accuracy 0.0)
        (setq valid_samples 0)

        ; Désactiver les gradients pendant la validation
        (torch_set_grad_enabled false)

        ; Traiter quelques échantillons de validation
        (setq num_val_samples 3)
        (loopcount num_val_samples i
            (setq val_batch (dataset_manager DPODatasetManager (get_validation_batch 1)))
            (check (> (size val_batch) 0)
                (setq pair (@ val_batch 0))
                (setq chosen_tensor (@ pair "chosen"))
                (setq rejected_tensor (@ pair "rejected"))

                ; Reshape
                (setq chosen_2d (tensor_reshape chosen_tensor (integers 1 -1)))
                (setq rejected_2d (tensor_reshape rejected_tensor (integers 1 -1)))

                ; Forward policy
                (setq policy_chosen_logits (torch_hf_forward policy_model_name chosen_2d))
                (setq policy_rejected_logits (torch_hf_forward policy_model_name rejected_2d))
                (setq policy_chosen_logprob (dpo_loss_calculator DPOLossCalculator
                    (compute_sequence_logprob policy_chosen_logits chosen_2d)))
                (setq policy_rejected_logprob (dpo_loss_calculator DPOLossCalculator
                    (compute_sequence_logprob policy_rejected_logits rejected_2d)))

                ; Forward reference
                (setq ref_chosen_logits (torch_hf_forward reference_model_name chosen_2d))
                (setq ref_rejected_logits (torch_hf_forward reference_model_name rejected_2d))
                (setq ref_chosen_logprob (dpo_loss_calculator DPOLossCalculator
                    (compute_sequence_logprob ref_chosen_logits chosen_2d)))
                (setq ref_rejected_logprob (dpo_loss_calculator DPOLossCalculator
                    (compute_sequence_logprob ref_rejected_logits rejected_2d)))

                ; Loss DPO
                (setq loss (dpo_loss_calculator DPOLossCalculator
                    (calculate_dpo_loss
                        policy_chosen_logprob
                        policy_rejected_logprob
                        ref_chosen_logprob
                        ref_rejected_logprob)))

                (check loss
                    (setq total_val_loss (+ total_val_loss (@ loss 0)))
                    ; Accuracy: est-ce que policy préfère chosen ?
                    (setq reward_margin (- (@ policy_chosen_logprob 0) (@ policy_rejected_logprob 0)))
                    (setq acc (if (> reward_margin 0.0) 1.0 0.0))
                    (setq total_val_accuracy (+ total_val_accuracy acc))
                    (setq valid_samples (+ valid_samples 1))
                )
            )

            ; Synchroniser après chaque échantillon
            (torch_mps_synchronize)
        )

        ; Réactiver les gradients
        (torch_set_grad_enabled true)

        (setq avg_val_loss (if (> valid_samples 0)
            (/ total_val_loss valid_samples)
            0.0))
        (setq avg_val_accuracy (if (> valid_samples 0)
            (/ total_val_accuracy valid_samples)
            0.0))

        (printerrln "✓ Validation - Loss:" (round (* avg_val_loss 10000) 10000)
                    "| Accuracy:" (round (* avg_val_accuracy 100) 100) "%")

        avg_val_loss
    )

    (defun save_checkpoint(epoch)
        (printerrln "\n💾 Sauvegarde du checkpoint epoch" epoch "...")

        (setq checkpoint_path (+ checkpoint-dir "/dpo_lora_adapters_epoch_" epoch ".pt"))

        ; ✅ Sauvegarder UNIQUEMENT les adaptateurs LoRA du policy model
        (torch_hf_lora_save policy_model_name checkpoint_path)

        ; Sauvegarder les métadonnées
        (setq checkpoint_info (dictionary
            "epoch" epoch
            "global_step" global_step
            "best_loss" best_loss
            "current_lr" (torch_scheduler_get_lr scheduler)
            "lora_config" lora-config
            "dpo_config" config
        ))

        (setq info_file (+ checkpoint-dir "/training_info_epoch_" epoch ".json"))
        (fwrite info_file (json checkpoint_info))

        (printerrln "✓ Checkpoint sauvegardé:")
        (printerrln "  • Adaptateurs:" checkpoint_path)
        (printerrln "  • Métadonnées:" info_file)
    )

    (defun train()
        (printerrln "\n" (fill "=" 70))
        (printerrln "  🚀 DÉMARRAGE DE L'ENTRAÎNEMENT DPO")
        (printerrln (fill "=" 70))

        (setq num_epochs (@ config "num_epochs"))
        (setq batch_size (@ config "batch_size"))
        (setq accum_steps (@ config "gradient_accumulation_steps"))
        (setq logging_steps (@ config "logging_steps"))
        (setq save_steps (@ config "save_steps"))
        (setq eval_steps (@ config "eval_steps"))

        (setq train_size (dataset_manager DPODatasetManager (size_train)))
        (setq steps_per_epoch (floor (/ train_size batch_size)))
        (setq effective_steps_per_epoch (floor (/ steps_per_epoch accum_steps)))

        (printerrln "\n📊 Configuration:")
        (printerrln "  • Paires d'entraînement:" train_size)
        (printerrln "  • Epochs:" num_epochs)
        (printerrln "  • Batch size:" batch_size)
        (printerrln "  • Gradient accumulation:" accum_steps "steps")
        (printerrln "  • Effective batch size:" (* batch_size accum_steps))
        (printerrln "  • Steps par epoch:" steps_per_epoch)
        (printerrln "  • Effective steps par epoch:" effective_steps_per_epoch)
        (printerrln "  • Learning rate:" (@ config "learning_rate"))
        (printerrln "  • Beta (DPO):" (@ config "beta"))

        (setq tempstotal 0)

        (loopcount num_epochs epoch
            (printerrln "\n" (fill "─" 70))
            (printerrln "📖 Epoch" (+ epoch 1) "/" num_epochs)
            (printerrln (fill "─" 70))

            (setq epoch_loss 0.0)
            (setq epoch_count 0)
            (setq temps 0)

            (loopcount steps_per_epoch step
                (+= temps
                    (elapse
                        (setq batch_start_idx (* step batch_size))
                        (setq batch (dataset_manager DPODatasetManager (get_batch batch_start_idx batch_size)))

                        ; Déterminer si on accumule
                        (setq is_accumulating (> (% accumulation_step accum_steps) 0))

                        (setq step_loss 0.0)
                        (setq step_count 0)

                        (loop pair batch
                            (printerr ".")
                            (setq chosen_tensor (@ pair "chosen"))
                            (setq rejected_tensor (@ pair "rejected"))

                            (setq loss_tensor (dpo_train_step chosen_tensor rejected_tensor is_accumulating))

                            (check loss_tensor
                                (setq loss_val (@ loss_tensor 0))
                                (setq step_loss (+ step_loss loss_val))
                                (setq step_count (+ step_count 1))
                            )

                            ; Libérer le tenseur
                            (setq loss_tensor nil)
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

                ; Logging avec métriques DPO
                (check (and did_update (== (% global_step logging_steps) 0))
                    (setq avg_recent (dpo_loss_calculator DPOLossCalculator
                        (get_average_loss (* logging_steps accum_steps))))
                    (setq avg_accuracy (dpo_loss_calculator DPOLossCalculator
                        (get_average_accuracy (* logging_steps accum_steps))))
                    (setq avg_margin (dpo_loss_calculator DPOLossCalculator
                        (get_average_reward_margin (* logging_steps accum_steps))))
                    (setq current_lr (torch_scheduler_get_lr scheduler))

                    (+= tempstotal temps)

                    (printerrln "\n  📊 Step" global_step
                        "| Loss:" (round (* avg_step_loss 10000) 10000)
                        "| Avg:" (round (* avg_recent 10000) 10000)
                        "| Best:" (round (* best_loss 10000) 10000)
                        "| Acc:" (round (* avg_accuracy 100) 100) "%"
                        "| Margin:" (round (* avg_margin 1000) 1000)
                        "| LR:" (round (* current_lr 1000000) 1000000)
                        "| ⏱️" (round (/ temps 1000)) "s")
                    (setq temps 0)
                )

                ; Validation
                (check (and did_update (== (% global_step eval_steps) 0))
                    (+= temps (elapse (validate)))
                )

                ; Sauvegarde
                (check (and did_update (== (% global_step save_steps) 0))
                    (save_checkpoint (+ epoch 1))
                )
            )

            (setq avg_epoch_loss (/ epoch_loss epoch_count))

            (printerrln "\n✓ Epoch" (+ epoch 1) "terminée - Loss moyenne:"
                (round (* avg_epoch_loss 10000) 10000))
        )

        ; Sauvegarde finale
        (save_checkpoint num_epochs)

        (setq final_avg_loss (dpo_loss_calculator DPOLossCalculator (get_average_loss 100)))
        (setq final_avg_accuracy (dpo_loss_calculator DPOLossCalculator (get_average_accuracy 100)))
        (setq final_lr (torch_scheduler_get_lr scheduler))

        (printerrln "\n" (fill "=" 70))
        (printerrln "  🎉 ENTRAÎNEMENT DPO TERMINÉ AVEC SUCCÈS")
        (printerrln (fill "=" 70))
        (printerrln "\n📈 Statistiques finales:")
        (printerrln "  • Total steps:" global_step)
        (printerrln "  • Meilleure loss:" (round (* best_loss 10000) 10000))
        (printerrln "  • Loss finale (avg 100):" (round (* final_avg_loss 10000) 10000))
        (printerrln "  • Accuracy finale (avg 100):" (round (* final_avg_accuracy 100) 100) "%")
        (printerrln "  • Learning rate final:" (round (* final_lr 1000000) 1000000))

        true
    )
)

; ==============================================
; FONCTION PRINCIPALE
; ==============================================

(printerrln "🚀 Initialisation du DPO Training V1...\n")

(setq trainer (DPOTrainerV1
    model-path
    tiktoken-path
    dataset-path
    dpo-config))

(withclass DPOTrainerV1
    (trainer (configure))
    (if (trainer (load_models))
        (if (trainer (setup_components))
            (if (trainer (train))
                (printerrln "\n✅ DPO Training V1 terminé avec succès! 🎉")
                (printerrln "\n❌ Échec de l'entraînement")
            )
            (printerrln "\n❌ Échec configuration des composants")
        )
        (printerrln "\n❌ Échec chargement des modèles")
    )
)
