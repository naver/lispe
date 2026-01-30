# Manuel LispE PyTorch - Guide Complet

Un guide complet pour l'utilisation de la librairie LispE PyTorch, offrant une intégration complète entre le langage LispE et l'écosystème PyTorch pour l'apprentissage automatique.

## Table des Matières

1. [Introduction](#introduction)
2. [Installation et Configuration](#installation-et-configuration)
3. [Types de Données](#types-de-données)
4. [Opérations de Base sur les Tenseurs](#opérations-de-base-sur-les-tenseurs)
5. [Modèles et Réseaux de Neurones](#modèles-et-réseaux-de-neurones)
6. [Chargement de Modèles Hugging Face](#chargement-de-modèles-hugging-face)
7. [Fine-tuning avec LoRA](#fine-tuning-avec-lora)
8. [Génération de Texte](#génération-de-texte)
9. [Tokenisation](#tokenisation)
10. [Flash Attention](#flash-attention)
11. [Optimisation et Quantification](#optimisation-et-quantification)
12. [Tutoriel Complet d'Entraînement LoRA](#tutoriel-complet-dentraînement-lora)
13. [Exemples Pratiques](#exemples-pratiques)

## Introduction

LispE PyTorch est une librairie qui intègre les capacités de PyTorch dans le langage de programmation fonctionnel LispE. Elle permet de créer, entraîner et déployer des modèles d'apprentissage automatique avec la syntaxe élégante de LispE.

### Caractéristiques Principales

- **Intégration Native PyTorch** : Accès direct aux tenseurs et opérations PyTorch
- **Support GPU** : CUDA pour NVIDIA et Metal Performance Shaders (MPS) pour Apple Silicon
- **Modèles Hugging Face** : Chargement et exécution de modèles pré-entraînés
- **LoRA Fine-tuning** : Fine-tuning efficace avec Low-Rank Adaptation
- **Flash Attention** : Attention mémoire-efficace pour longues séquences
- **Génération de Texte** : Système complet avec multiples stratégies d'échantillonnage
- **Quantification** : Support FP16 et INT8 pour l'optimisation des modèles

## Installation et Configuration

### Chargement de la Librairie

```lisp
; Charger la librairie PyTorch
(use 'lispe_torch)

; Vérifier la disponibilité GPU
(if (torch_cuda_is_available)
    (println "✓ CUDA disponible")
    (if (torch_mps_is_available) 
        (println "✓ MPS (Apple Silicon) disponible")
        (println "• Utilisation du CPU")))
```

### Configuration de Base

```lisp
; Définir le device par défaut
(setq device (cond 
    ((torch_cuda_is_available) "cuda")
    ((torch_mps_is_available) "mps")
    (true "cpu")))

(println "Device sélectionné:" device)
```

## Types de Données

### Types LispE Optimisés pour PyTorch

LispE fournit des types de données optimisés qui s'intègrent efficacement avec PyTorch :

```lisp
; Types de listes optimisées (zero-copy)
(setq entiers (integers 1 2 3 4 5))       ; Liste d'entiers
(setq flotants (floats 1.0 2.0 3.0 4.0))  ; Liste de float
(setq nombres (numbers 1.0 2.0 3.0))      ; Liste de double
(setq courts (shorts 1 2 3))              ; Liste de short

; Générateurs de séquences
(setq seq1 (iota0 5))    ; [0, 1, 2, 3, 4]
(setq seq2 (iota 5))     ; [1, 2, 3, 4, 5]
```

### Types PyTorch dans LispE

```lisp
; Types principaux
; tensor_create    - Encapsule torch::Tensor
; torch_model     - Encapsule les modèles PyTorch
; torch_optimizer - Encapsule les optimiseurs
; torch_tokenizer - Interface de tokenisation
```

## Opérations de Base sur les Tenseurs

### Création de Tenseurs

```lisp
; Création à partir de listes LispE
(setq data (floats 1.0 2.0 3.0 4.0))
(setq tensor (tensor_create data))

; Tenseurs avec formes spécifiques
(setq zeros_tensor (tensor_zeros (integers 3 4)))
(setq ones_tensor (tensor_ones (integers 2 3)))
(setq random_tensor (tensor_randn (integers 4 4)))

; Tenseurs avec device spécifique
(setq cuda_tensor (torch_to_cuda (tensor_randn (integers 2 2))))
(setq mps_tensor (torch_to_mps (tensor_randn (integers 2 2))))
```

### Opérations Mathématiques

```lisp
; Opérations de base
(setq a (tensor_randn (integers 3 3)))
(setq b (tensor_randn (integers 3 3)))

(setq somme (tensor_add a b))
(setq produit (tensor_matmul a b))
(setq transposee (tensor_transpose a 0 1))

; Fonctions d'activation
(setq tensor (tensor_randn (integers 10)))
(setq relu_result (tensor_relu tensor))
(setq sigmoid_result (tensor_sigmoid tensor))
(setq softmax_result (tensor_softmax tensor -1))

; Opérations par élément
(setq div_result (tensor_div_scalar tensor 2.0))
(setq shape (tensor_shape tensor))
(setq item_value (tensor_item (tensor_select tensor 0)))
```

### Manipulation de Formes

```lisp
; Obtenir les dimensions
(setq shape (tensor_shape tensor))
(println "Forme du tensor:" shape)

; Redimensionner
(setq reshaped (tensor_reshape tensor (integers 2 2)))
(setq squeezed (tensor_squeeze tensor))
(setq unsqueezed (tensor_unsqueeze tensor 0))
```

## Modèles et Réseaux de Neurones

### Création de Modèles Simples

```lisp
; Créer des couches individuelles PyTorch
(setq linear1 (torch_linear 784 128))    ; couche linéaire input->hidden
(setq linear2 (torch_linear 128 10))     ; couche linéaire hidden->output

; Créer des données d'exemple
(setq input_data (tensor_randn (integers 32 784)))  ; batch_size=32, features=784

; Propagation avant manuelle
(setq h1 (torch_linear_forward linear1 input_data))  ; première couche
(setq h1_relu (tensor_relu h1))                       ; activation ReLU
(setq output (torch_linear_forward linear2 h1_relu)) ; deuxième couche

; Autres composants disponibles
(setq embedding (torch_embedding 1000 128))   ; vocabulary=1000, dim=128
(setq layer_norm (torch_layer_norm 128))      ; normalisation
(setq attention (torch_multihead_attention 128 8))  ; attention multi-têtes
```

### Chargement de Modèles Pré-entraînés

```lisp
; Pour charger des modèles pré-entraînés (différent de créer des modèles)
(setq model_path "path/to/huggingface/model")
(setq config (dictionary "device" "mps"))
(setq model_id (torch_hf_load_model model_path config))

; Forward pass avec modèle HuggingFace
(setq input_tokens (tensor_create (integers 15496 318 257)))
(setq input_batch (tensor_unsqueeze input_tokens 0))
(setq logits (torch_hf_forward model_id input_batch))
```

### Blocs Transformer

```lisp
; Créer un bloc Transformer
(setq transformer_block (torch_transformer_block 512 8 2048))

; Forward pass avec masque  
(setq input_seq (tensor_randn (integers 32 128 512)))  ; [batch, seq, dim]
(setq mask (tensor_ones (integers 32 128 128)))        ; attention mask
(setq output (torch_transformer_forward transformer_block input_seq mask))
```

### Entraînement

```lisp
; Créer un optimiseur
(setq learning_rate 0.001)
(setq optimizer (torch_adamw_optimizer learning_rate))
(torch_optimizer_add_params optimizer mlp)

; Boucle d'entraînement
(loop i 100
    ; Forward pass
    (setq predictions (torch_forward mlp input_data))
    
    ; Calcul de la loss
    (setq loss (torch_mse_loss predictions targets))
    
    ; Backward pass
    (torch_optimizer_zero_grad optimizer)
    (torch_backward loss)
    (torch_optimizer_step optimizer)
    
    ; Affichage du progrès
    (if (== (% i 10) 0)
        (println "Epoch" i "Loss:" (tensor_item loss)))
)
```

## Chargement de Modèles Hugging Face

### Configuration et Chargement

```lisp
; Configuration complète pour le chargement HuggingFace
(setq model_path "/path/to/model")
(setq config (dictionary 
    ; === PARAMÈTRES DE BASE ===
    "device" "mps"                    ; Device : "cuda", "mps", "cpu"
    
    ; === PARAMÈTRES DE SÉQUENCE ===
    "max_seq_len" 2048                ; Longueur maximale de séquence supportée
    "rope_scaling" 1.0                ; Facteur d'échelle pour RoPE (Rotary Position Embedding)
    
    ; === PARAMÈTRES DE GÉNÉRATION ===
    "temperature" 0.7                 ; Température pour le sampling (0.1-2.0)
    "top_p" 0.9                       ; Nucleus sampling - probabilité cumulative
    "top_k" 50                        ; Top-K sampling - nombre de tokens considérés
    "repetition_penalty" 1.1          ; Pénalité de répétition (1.0 = pas de pénalité)
    
    ; === PARAMÈTRES DE CACHE ===
    "use_kv_cache" true               ; Activer le cache Key-Value pour génération
    "max_cache_len" 4096              ; Taille maximale du cache (fenêtre glissante)
    
    ; === PARAMÈTRES AVANCÉS ===
    "manual_attention" false          ; Mode attention manuelle (debugging/contrôle fin)
))

; Charger le modèle avec configuration complète
(setq model_id (torch_hf_load_model model_path config))
(println "✓ Modèle chargé avec ID:" model_id)
```

### Inférence avec Cache KV

```lisp
; Activer le cache KV pour une génération efficace
(setq context_id (torch_hf_enable_kv_cache model_id true))

; Préparer les tokens d'entrée
(setq input_tokens (tensor_create (integers 15496 318 257)))  ; "This is a"
(setq input_batch (tensor_unsqueeze input_tokens 0))         ; Ajouter dimension batch

; Forward pass (utilise automatiquement le cache)
(setq logits (torch_hf_forward model_id input_batch context_id))

; Extraire les logits du dernier token
(setq last_logits (tensor_select logits 1 -1))
(setq next_token (tensor_argmax last_logits -1))
```

## Fine-tuning avec LoRA

### Initialisation LoRA

```lisp
; Charger le modèle avec support LoRA
(setq model_name "llama_lora")
(torch_hf_load_model_lora 
    model_path 
    config 
    model_name)

; Configuration LoRA
(setq lora_config (dictionary
    "rank" 16
    "alpha" 32.0
    "target_modules" (strings "q_proj" "k_proj" "v_proj" "o_proj")
    "dtype" "float16"
))

; Initialiser les adaptateurs LoRA
(torch_hf_lora_init 
    model_name 
    lora_config)
```

### Entraînement LoRA

```lisp
; Créer optimiseur pour paramètres LoRA uniquement  
(setq lora_params (torch_hf_lora_get_parameters model_name))
(setq learning_rate 2e-4)
(setq optimizer (torch_adamw_optimizer learning_rate))
(torch_optimizer_add_params optimizer lora_params)

; Boucle d'entraînement
(loop epoch 3
    (println "Epoch" (+ epoch 1))
    
    ; Pour chaque batch de données
    (loop batch_idx num_batches
        ; Préparer les données
        (setq input_ids (get_batch_data batch_idx))
        
        ; Forward pass avec LoRA
        (setq logits (torch_hf_forward model_name input_ids))
        
        ; Calcul de la loss
        (setq loss (calculate_lm_loss logits input_ids))
        
        ; Backward pass
        (torch_optimizer_zero_grad optimizer)
        (torch_backward loss)
        (torch_optimizer_step optimizer)
        
        ; Logging
        (if (== (% batch_idx 10) 0)
            (println "  Batch" batch_idx "Loss:" (tensor_item loss)))
    )
    
    ; Sauvegarder les adaptateurs LoRA
    (torch_hf_lora_save model_name (+ output_dir "/epoch_" epoch))
)
```

## Génération de Texte

### Classe Model pour Génération

Voici un exemple d'implémentation de classe pour la génération de texte :

```lisp
; Classe Model pour l'inférence
(class@ Model (model_path config tokenizer init)
    (defun configure()
        ; Initialiser le modèle HuggingFace
        (setqi model_id (torch_hf_load_model model_path config))
        (setqi temperature 0.7)
        (setqi top_p 0.9)
        (setqi max_tokens 100)
    )
    
    (defun generate(prompt max_length)
        ; Encoder le prompt
        (setq prompt_tokens (tokenizer (encode prompt)))
        (setq generated_tokens (clone prompt_tokens))
        
        ; Activer le cache KV pour performance
        (setq context_id (torch_hf_enable_kv_cache model_id true))
        
        ; Génération token par token
        (setq current_input (tensor_unsqueeze (tensor_create prompt_tokens) 0))
        
        (loop i max_length
            ; Forward pass
            (setq logits (torch_hf_forward model_id current_input context_id))
            (setq last_logits (tensor_select logits 1 -1))
            (setq last_logits (tensor_select last_logits 0 0))
            
            ; Application de la température
            (setq scaled_logits (tensor_div_scalar last_logits temperature))
            
            ; Sampling avec top-p (nucleus sampling)
            (setq probs (tensor_softmax scaled_logits -1))
            (setq next_token (tensor_multinomial probs 1 true))
            (setq next_token_id (tensor_item next_token))
            
            ; Afficher le token généré
            (print (tokenizer (decode (integers next_token_id))))
            
            ; Ajouter à la séquence
            (push generated_tokens next_token_id)
            
            ; Vérifier token de fin
            (check (== next_token_id (tokenizer (eos_id)))
                (break)
            )
            
            ; Préparer input suivant (seulement le nouveau token)
            (setq current_input (tensor_unsqueeze (tensor_create (integers next_token_id)) 0))
        )
        
        (println)  ; Nouvelle ligne
        generated_tokens
    )
)
```

### Génération Avancée avec Paramètres

```lisp
; Générateur avec contrôle fin
(setq generator_config (dictionary
    "temperature" 0.8
    "top_k" 50
    "top_p" 0.95
    "repetition_penalty" 1.1
    "max_length" 200
    "do_sample" true
))

; Génération avec configuration complète
(defun generate_with_config(model_path prompt_tokens eos_id)
    ; Exemples avec différentes stratégies de sampling
    
    ; 1. Génération par défaut (sampling simple)
    (setq result_default (torch_hf_generate model_path prompt_tokens eos_id 100))
    
    ; 2. Génération gloutonne (déterministe)
    (setq greedy_options (dictionary "greedy" true))
    (setq result_greedy (torch_hf_generate model_path prompt_tokens eos_id 100 greedy_options))
    
    ; 3. Top-K sampling (diversité contrôlée)
    (setq topk_options (dictionary "topk" 50))
    (setq result_topk (torch_hf_generate model_path prompt_tokens eos_id 100 topk_options))
    
    ; 4. Nucleus/Top-P sampling (diversité dynamique)
    (setq topp_options (dictionary "topp" 0.9))
    (setq result_topp (torch_hf_generate model_path prompt_tokens eos_id 100 topp_options))
    
    ; 5. Génération avec callback pour monitoring en temps réel
    ; EXEMPLE CONCRET: Fonction display pour afficher chaque token généré
    (defun display_token(token_id tokenizer)
        (printerr (tokenizer (decode (integers token_id))))
    )
    
    (setq callback_options (dictionary 
        "topk" 30                      ; Top-K sampling avec K=30
        "callback" 'display_token      ; Référence à la fonction (avec quote)
        "data" tokenizer               ; Passer le tokenizer comme données
    ))
    (setq result_callback (torch_hf_generate model_path prompt_tokens eos_id 100 callback_options))
    
    ; Alternative avec lambda inline
    (setq lambda_options (dictionary
        "topp" 0.95
        "callback" (lambda (token_id data)
            (println "Token généré:" token_id)
            (printerr (data (decode (integers token_id))))  ; data = tokenizer
        )
        "data" tokenizer
    ))
    
    result_callback
)
```

## Tokenisation

### Tokenizer TikToken

```lisp
; Classe Tokenizer utilisant TikToken
(class@ Tokenizer (tokenizer_path init)
    (defun configure()
        ; Charger la configuration
        (setq config (json_parse (fread (+ tokenizer_path "/tokenizer_config.json"))))
        (setq vocab (json_parse (fread (+ tokenizer_path "/tokenizer.json"))))
        
        ; Créer le tokenizer
        (setqi tokenizer_obj (tiktoken_create
            (@ vocab "model" "vocab")
            (@ vocab "added_tokens") 
            (@ vocab "pre_tokenizer" "pretokenizers" 0 "pattern" "Regex")
        ))
        
        ; Récupérer les tokens spéciaux
        (setqi bos_id (tiktoken_special_encode tokenizer_obj "<|begin_of_text|>"))
        (setqi eos_id (tiktoken_special_encode tokenizer_obj "<|end_of_text|>"))
    )
    
    (defun encode(text)
        ; Encoder le texte avec tokens spéciaux
        (setq tokens (tiktoken_encode tokenizer_obj text))
        (pushfirst tokens bos_id)
        tokens
    )
    
    (defun decode(token_list)
        ; Décoder les tokens
        (tiktoken_decode tokenizer_obj token_list)
    )
    
    (defun eos_id()
        eos_id
    )
)
```

### Utilisation du Tokenizer

```lisp
; Créer et configurer le tokenizer
(setq tok (Tokenizer "/path/to/tokenizer"))
(withclass Tokenizer
    (tok (configure))
)

; Encoder du texte
(setq tokens (tok (encode "Bonjour, comment allez-vous ?")))
(println "Tokens:" tokens)

; Décoder les tokens
(setq text (tok (decode tokens)))
(println "Texte décodé:" text)
```

## Flash Attention

### Attention Mémoire-Efficace

```lisp
; Créer les tenseurs d'attention pour longues séquences
(setq batch_size 2)
(setq num_heads 8) 
(setq seq_length 4096)  ; Séquence longue
(setq head_dim 64)

; Tenseurs Query, Key, Value
(setq query (tensor_randn (integers batch_size num_heads seq_length head_dim)))
(setq key (tensor_randn (integers batch_size num_heads seq_length head_dim)))
(setq value (tensor_randn (integers batch_size num_heads seq_length head_dim)))

; Flash Attention - O(N) en mémoire au lieu de O(N²)
(setq scale (/ 1.0 (sqrt head_dim)))
(setq attention_output (torch_flash_attention query key value scale))

(println "✓ Flash Attention calculée efficacement")
(println "Forme de sortie:" (tensor_shape attention_output))
```

### Flash Attention avec Masque

```lisp
; Créer un masque causal (utiliser triu pour triangulaire supérieur)
(setq causal_mask (tensor_triu (tensor_ones (integers seq_length seq_length)) 1))

; Créer module Flash Attention et appliquer avec masque
(setq flash_attention (torch_flash_attention_create (* num_heads head_dim) num_heads 0.0 false))
(setq masked_output (torch_flash_attention_with_mask 
    flash_attention query key value causal_mask))
```

## Optimisation et Quantification

### Quantification FP16

```lisp
; Quantification d'un modèle en FP16 (50% de réduction mémoire)
(setq model_weights (tensor_randn (integers 512 768)))
(setq fp16_weights (torch_quantize_fp16 model_weights))

; Vérification de la taille
(setq original_size (tensor_size model_weights))
(setq compressed_size (tensor_size fp16_weights))
(println "Compression FP16 - Ratio:" (/ compressed_size original_size))
```

### Quantification INT8

```lisp
; Quantification INT8 (75% de réduction mémoire)
(setq int8_weights (torch_quantize_int8 model_weights))

; Déquantification pour vérification
(setq reconstructed (torch_dequantize int8_weights))
(setq mse_error (torch_mse_loss model_weights reconstructed))
(println "Erreur de reconstruction INT8:" (tensor_item mse_error))
```

### Quantification Dynamique de Modèle

```lisp
; Quantification complète d'un modèle
(setq quantized_model (torch_model_quantize_dynamic model "qint8"))
(println "✓ Modèle quantifié pour déploiement production")

; Comparaison des performances (exemple conceptuel)
(println "✓ Modèle quantifié pour déploiement production")
(println "Note: La quantification réduit l'usage mémoire et peut accélérer l'inférence")
```

## Tutoriel Complet d'Entraînement LoRA

Cette section fournit un guide complet pour implémenter le fine-tuning LoRA (Low-Rank Adaptation) avec LispE PyTorch, basé sur un exemple concret avec le modèle Llama 3.1-8B.

### Vue d'ensemble

LoRA (Low-Rank Adaptation) est une technique révolutionnaire de fine-tuning qui résout l'un des défis majeurs de l'adaptation des grands modèles de langage : la consommation excessive de ressources computationnelles et mémoire.

#### Le Problème du Fine-tuning Traditionnel

Le fine-tuning classique nécessite de mettre à jour tous les paramètres d'un modèle, ce qui pour un modèle comme Llama 3.1-8B représente :
- **8 milliards de paramètres** à entraîner
- **32+ GB de mémoire** pour les gradients seuls
- **Heures ou jours d'entraînement** sur GPU haut de gamme
- **Risque de catastrophic forgetting** des connaissances pré-apprises

#### La Solution LoRA

LoRA se base sur l'hypothèse que les mises à jour lors du fine-tuning ont un "rang intrinsèque faible". Au lieu de modifier directement les poids W d'une couche, LoRA introduit deux petites matrices A et B telles que :

```
W' = W + α/r × A × B
```

Où :
- **W** : matrice de poids originale (figée)
- **A** : matrice de rang faible (r × d)
- **B** : matrice de rang faible (d × r) 
- **α** : facteur d'échelle
- **r** : rang de décomposition (typiquement 8-64)

#### Avantages Techniques

1. **Réduction Drastique des Paramètres** :
   - Pour une couche 4096×4096 avec r=16 : 16M → 131K paramètres (99% de réduction)
   - Mémoire GPU divisée par 10-100x

2. **Préservation des Connaissances** :
   - Les poids pré-entraînés restent intacts
   - Pas de catastrophic forgetting
   - Possibilité de combiner plusieurs adaptateurs

3. **Flexibilité de Déploiement** :
   - Adaptateurs de quelques MB vs modèles de GB
   - Changement d'adaptateur à la volée
   - Stockage et distribution simplifiés

4. **Performance Maintenue** :
   - Résultats comparables au fine-tuning complet
   - Convergence souvent plus rapide
   - Moins de sur-apprentissage

#### Applications Pratiques

- **Adaptation domaine-spécifique** : médical, juridique, technique
- **Personnalisation** : style d'écriture, ton, format de réponse
- **Multi-tâches** : plusieurs adaptateurs pour différentes tâches
- **Prototypage rapide** : tests d'hypothèses avec ressources limitées

Cette implémentation LispE PyTorch offre une interface simple et puissante pour exploiter LoRA avec une gestion mémoire optimisée, particulièrement adaptée aux environnements avec contraintes de ressources.

### Structure du Projet

```
projet_entrainement/
├── llama3.1-8B/
│   ├── model/                 # Fichiers du modèle (pytorch_model.bin, config.json, etc.)
│   └── tokenizer/            # Fichiers du tokenizer (vocab, special_tokens_map.json, etc.)
├── tamgu_dataset.json        # Dataset d'entraînement au format chat
├── tamgu_lora_adapters_v2/   # Répertoire de sortie pour les adaptateurs LoRA
├── checkpoints_v2/           # Checkpoints d'entraînement
└── lora_training.lisp        # Script d'entraînement principal
```

### Configuration Initiale

```lisp
; Charger les librairies requises
(use 'lispe_torch)
(use 'lispe_tiktoken)

; Configuration globale
(setq model-path (+ _current "llama3.1-8B/model"))
(setq tiktoken-path (+ _current "llama3.1-8B/tokenizer"))
(setq dataset-path (+ _current "tamgu_dataset.json"))
(setq output-dir (+ _current "tamgu_lora_adapters_v2"))
(setq checkpoint-dir (+ _current "checkpoints_v2"))

; Configuration LoRA - POINT CLÉ: Paramètres de la décomposition W' = W + α/r × A × B
(setq lora-config (dictionary
    "rank" 16                                          ; 📊 RANG r=16: détermine la taille des matrices A(r×d) et B(d×r)
                                                        ; Plus petit = moins de paramètres mais capacité réduite
    "alpha" 32                                         ; 📈 FACTEUR α=32: contrôle l'amplitude des adaptations LoRA
                                                        ; Ratio α/r = 32/16 = 2.0 (scaling des corrections)
    "target_modules" (strings "q_proj" "k_proj" "v_proj" "o_proj")  ; 🎯 MODULES CIBLÉS: seulement les projections d'attention
                                                                      ; Évite q_proj et k_proj pour préserver l'alignement sémantique
))

; Configuration d'entraînement - OPTIMISÉE pour LoRA
(setq training-config (dictionary
    "learning_rate" 2e-4                               ; 🎯 LR ÉLEVÉ: LoRA peut supporter des taux plus élevés (vs 5e-5 classique)
                                                        ; car les matrices A,B sont initialisées à zéro → pas de perturbation initiale
    "weight_decay" 0.01                                ; 🛡️ RÉGULARISATION FAIBLE: évite de contraindre les petites matrices LoRA
    "num_epochs" 3                                     ; ⚡ CONVERGENCE RAPIDE: LoRA converge plus vite que le fine-tuning complet
    "batch_size" 1                                     ; 💾 BATCH RÉDUIT: compensé par l'accumulation pour économiser la mémoire
    "gradient_accumulation_steps" 4                    ; 🔄 ACCUMULATION: simule batch_size=4 sans surcharge mémoire
                                                        ; Essentiel avec les contraintes mémoire de LoRA
    "max_seq_length" 256                               ; 📏 SÉQUENCES COURTES: permet plus d'échantillons avec mémoire limitée
    "logging_steps" 10
    "save_steps" 100                                   ; 💾 CHECKPOINTS FRÉQUENTS: adaptateurs LoRA sont légers (quelques MB)
    "eval_steps" 50
    "warmup_steps" 100                                 ; 🔥 WARMUP IMPORTANT: stabilise l'entraînement des petites matrices
    "max_grad_norm" 1.0                                ; ✂️ GRADIENT CLIPPING: prévient l'instabilité des matrices de rang faible
    "scheduler_type" "linear_warmup_cosine"
    "min_lr" 1e-6
    "device" "mps"  ; "mps" pour Apple Silicon, "cuda" pour NVIDIA, "cpu" sinon
))
```

### Implémentation du Tokenizer TikToken

```lisp
; Classe tokenizer TikToken pour les modèles Llama
(class@ TiktokenTokenizer (tokenizer_path init)
    (defun configure()
        (printerrln "📝 Configuration du tokenizer tiktoken...")
        
        ; Charger les fichiers de configuration du tokenizer
        (setq spec_tokens (json_parse (fread (+ tokenizer_path "/special_tokens_map.json"))))
        (setq tok_file (json_parse (fread (+ tokenizer_path "/tokenizer.json"))))
        
        ; Extraire les tokens spéciaux
        (setqi bos_token (@ spec_tokens "bos_token" "content"))
        (setqi eos_token (@ spec_tokens "eos_token" "content"))
        (setq pattern (@ tok_file "pre_tokenizer" "pretokenizers" 0 "pattern" "Regex"))

        ; Créer l'objet tokenizer
        (setqi tokenizer_obj (tiktoken_create
            (@ tok_file "model" "vocab")
            (@ tok_file "added_tokens")
            pattern))

        ; Obtenir les IDs des tokens spéciaux
        (setqi bos_id (tiktoken_special_encode tokenizer_obj bos_token))
        (setqi eos_id (tiktoken_special_encode tokenizer_obj eos_token))
        (setqi pad_id 0)

        (printerrln "✓ Tokenizer configuré - taille vocab:" (tiktoken_vocab_size tokenizer_obj))
    )

    ; Formater le texte au format template de chat
    (defun encode_chat_format(instruction response)
        (setq formatted_text (+
            "<|begin_of_text|>"
            "<|start_header_id|>system<|end_header_id|>\n\n"
            "Vous êtes un assistant utile qui connaît bien les langages de programmation."
            "<|eot_id|>"
            "<|start_header_id|>user<|end_header_id|>\n\n"
            instruction
            "<|eot_id|>"
            "<|start_header_id|>assistant<|end_header_id|>\n\n"
            response
            "<|eot_id|>"
        ))

        ; Encoder et ajouter le token BOS
        (setq tokens (tiktoken_encode tokenizer_obj formatted_text))
        (pushfirst tokens bos_id)

        ; Tronquer si trop long
        (if (> (size tokens) (@ training-config "max_seq_length"))
            (setq tokens (@@ tokens 0 (@ training-config "max_seq_length")))
        )

        tokens
    )

    ; Décoder les tokens en texte
    (defun decode(token_ids)
        (tiktoken_decode tokenizer_obj token_ids)
    )
)
```

### Gestion du Dataset

```lisp
; Classe gestionnaire de dataset pour les données d'entraînement
(class@ DatasetManager (dataset_path tiktokenizer init)
    (defun configure()
        (printerrln "📚 Chargement du dataset...")
        
        ; Charger et parser le dataset JSON
        (setq raw_data (json_parse (fread dataset_path)))
        (setqi samples (list))
        (setqi validation_samples (list))

        ; Diviser en train/validation (80/20)
        (setq total_size (size raw_data))
        (setq train_size (floor (* total_size 0.8)))

        (printerrln "⚙️  Préparation des échantillons...")
        (loopcount total_size i
            (setq sample (@ raw_data i))
            (setq instruction (@ sample "instruction"))
            (setq response (@ sample "response"))

            ; Tokeniser en utilisant le format chat
            (setq tokens (tiktokenizer TiktokenTokenizer 
                (encode_chat_format instruction response)))
            (setq token_tensor (tensor_create tokens))

            ; Ajouter au dataset approprié
            (if (< i train_size)
                (push samples token_tensor)
                (push validation_samples token_tensor)
            )

            ; Indicateur de progression
            (if (== (% i 100) 0)
                (printerr ".")
            )
        )

        (printerrln "\n✓ Dataset préparé:")
        (printerrln "  • Entraînement:" (size samples) "échantillons")
        (printerrln "  • Validation:" (size validation_samples) "échantillons")
    )

    ; Obtenir un batch d'entraînement
    (defun get_batch(start_idx batch_size)
        (setq batch (list))
        (setq end_idx (min (+ start_idx batch_size) (size samples)))

        (loopcount (- end_idx start_idx) i
            (push batch (@ samples (+ start_idx i)))
        )
        batch
    )

    ; Obtenir un batch de validation
    (defun get_validation_batch(max_samples)
        (setq val_batch (list))
        (setq num_samples (min max_samples (size validation_samples)))

        (loopcount num_samples i
            (push val_batch (@ validation_samples i))
        )
        val_batch
    )
)
```

### Chargement du Modèle et Configuration LoRA

```lisp
; Classe principale d'entraînement LoRA
(class@ LoRATrainerV2 (model_path tokenizer_path dataset_path config)
    (defun configure()
        (printerrln "⚙️  Configuration du LoRA Trainer V2...")
        (setqi model_name "llama31_lora")
        (setqi optimizer nil)
        (setqi scheduler nil)
        (setqi global_step 0)
        (setqi best_loss 1000.0)
        true
    )

    ; Charger le modèle avec support LoRA
    (defun load_model()
        (printerrln "\n🚀 Chargement du modèle avec LoRA...")

        ; Charger le modèle HuggingFace avec infrastructure LoRA
        (torch_hf_load_model_lora
            model_name
            model_path
            (dictionary "device" (@ config "device")))

        ; Afficher les informations du modèle
        (setq memory_usage (torch_hf_memory_usage model_name))
        (setq memory_gb (/ memory_usage 1073741824.0))

        (printerrln "✓ Modèle HuggingFace chargé avec support LoRA:")
        (printerrln "  • Nom:" model_name)
        (printerrln "  • Chemin:" model_path)
        (printerrln "  • Device:" (@ config "device"))
        (printerrln "  • Mémoire:" memory_gb "GB")

        true
    )

    ; Initialiser les adaptateurs LoRA
    (defun setup_lora()
        (printerrln "\n🎨 Initialisation des adaptateurs LoRA...")

            ; 🔧 INITIALISATION CRITIQUE des matrices LoRA A et B
        ; POINT CLÉ: Implémente la décomposition W' = W + α/r × A × B
        (torch_hf_lora_init
            model_name
            (@ lora-config "rank")                      ; r=16: taille des matrices A(16×4096) et B(4096×16)
            (@ lora-config "alpha")                     ; α=32: facteur d'échelle pour contrôler l'amplitude
            (@ lora-config "target_modules")            ; Seulement q_proj, k_proj, v_proj, o_proj de l'attention
            "bfloat16")                                 ; 🎯 PRÉCISION: même dtype que le modèle base pour cohérence

        (printerrln "✓ Adaptateurs LoRA initialisés:")
        (printerrln "  • Rang:" (@ lora-config "rank"))
        (printerrln "  • Alpha:" (@ lora-config "alpha"))
        (printerrln "  • Modules:" (@ lora-config "target_modules"))

        ; 📊 RÉCUPÉRATION des paramètres LoRA UNIQUEMENT (matrices A et B)
        ; AVANTAGE CLÉ: Seules ces matrices sont entraînables, pas les poids W originaux
        (setq lora_params (torch_hf_lora_get_parameters model_name))
        (printerrln "  • Paramètres LoRA:" (size lora_params) "tenseurs")
        
        ; 💡 CALCUL DE LA RÉDUCTION: pour une couche 4096×4096 avec r=16
        ; Paramètres originaux: 4096×4096 = 16M
        ; Paramètres LoRA: 2×(4096×16) = 131K → réduction de 99.2%

        lora_params
    )

    ; Étape d'entraînement avec accumulation de gradients
    (defun train_step(input_tensor is_accumulating)
        ; Remettre les gradients à zéro au début du cycle d'accumulation
        (check (not is_accumulating)
            (torch_optimizer_zero_grad optimizer)
        )

        ; 🚀 PROPAGATION AVANT avec LoRA intégré automatiquement
        ; POINT CLÉ: torch_hf_forward applique W' = W + α/r × A × B de façon transparente
        (setq input_2d (tensor_reshape input_tensor (integers 1 -1)))
        (setq output (torch_hf_forward model_name input_2d))           ; Les corrections LoRA sont ajoutées automatiquement
                                                                       ; aux projections d'attention q, k, v, o

        ; Calculer la loss de modélisation du langage
        (setq loss (calculate_language_modeling_loss output input_2d))

        (check loss
            ; Normaliser la loss par les étapes d'accumulation
            (setq accum_steps (@ config "gradient_accumulation_steps"))
            (setq scaled_loss (tensor_div loss (tensor_create (floats accum_steps))))
            
            ; Propagation arrière
            (torch_backward scaled_loss)

            ; Synchronisation mémoire pour MPS
            (torch_mps_synchronize)

            loss
        )
    )

    ; Boucle d'entraînement principale
    (defun train()
        (printerrln "\n🚀 Début de l'entraînement LoRA...")

        (setq num_epochs (@ config "num_epochs"))
        (setq batch_size (@ config "batch_size"))

        (loopcount num_epochs epoch
            (printerrln "\n📖 Époque" (+ epoch 1) "/" num_epochs)

            ; Implémentation de la boucle d'entraînement
            ; ... (traitement des batches, calcul de loss, validation)

            (printerrln "✓ Époque" (+ epoch 1) "terminée")
        )

        (printerrln "🎉 Entraînement terminé avec succès !")
        true
    )
)
```

### Calcul de la Loss

```lisp
; Calculateur de loss pour modélisation du langage
(class@ LossCalculator (init)
    (defun calculate_language_modeling_loss(logits input_tokens)
        (setq logits_shape (tensor_shape logits))
        (setq batch_size (@ logits_shape 0))
        (setq seq_len (@ logits_shape 1))
        (setq vocab_size (@ logits_shape 2))

        (check (> seq_len 1)
            ; Prédictions : logits pour le token suivant à chaque position
            (setq pred_logits (tensor_slice logits 1 0 (- seq_len 1)))
            
            ; Cibles : tokens suivants réels (décalés de 1)
            (setq target_tokens (tensor_slice input_tokens 1 1 seq_len))

            ; Redimensionner pour la loss d'entropie croisée
            (setq pred_flat (tensor_reshape pred_logits 
                (integers (* batch_size (- seq_len 1)) vocab_size)))
            (setq target_flat (tensor_reshape target_tokens 
                (integers (* batch_size (- seq_len 1)))))

            ; Calculer la loss d'entropie croisée
            (setq loss (torch_cross_entropy pred_flat target_flat))

            ; Synchroniser pour la gestion mémoire
            (torch_mps_synchronize)

            loss
        )
    )
)
```

### Exécution de l'Entraînement

```lisp
; Exécution principale
(printerrln "🚀 Initialisation du fine-tuning LoRA...\n")

; Créer l'instance de trainer
(setq trainer (LoRATrainerV2
    model-path
    tiktoken-path
    dataset-path
    training-config))

; Exécuter le pipeline d'entraînement
(withclass LoRATrainerV2
    (trainer (configure))
    (if (trainer (load_model))
        (if (trainer (setup_components))
            (if (trainer (train))
                (printerrln "\n✅ Fine-tuning LoRA terminé avec succès ! 🎉")
                (printerrln "\n❌ Échec de l'entraînement")
            )
            (printerrln "\n❌ Échec de la configuration des composants")
        )
        (printerrln "\n❌ Échec du chargement du modèle")
    )
)
```

### Fonctionnalités Clés

1. **Efficacité Mémoire** : Utilise l'accumulation de gradients et la synchronisation MPS pour une utilisation mémoire optimale
2. **Design Modulaire** : Classes séparées pour tokenizer, dataset, calcul de loss et entraînement
3. **Monitoring** : Journalisation complète de la loss, du taux d'apprentissage et de l'utilisation mémoire
4. **Checkpointing** : Sauvegarde régulière des adaptateurs LoRA et de l'état d'entraînement
5. **Validation** : Évaluation périodique sur des données de test
6. **Support Multi-Device** : Détection automatique du meilleur device disponible (CUDA/MPS/CPU)

### Format du Dataset

Le dataset d'entraînement doit être au format JSON avec des paires instruction-réponse :

```json
[
    {
        "instruction": "Comment définir une fonction en Tamgu ?",
        "response": "En Tamgu, vous définissez une fonction en utilisant le mot-clé 'function'..."
    },
    {
        "instruction": "Quels sont les types de données de base en Tamgu ?",
        "response": "Tamgu supporte plusieurs types de données incluant..."
    }
]
```

Cet exemple complet démontre comment implémenter un fine-tuning LoRA efficace avec une gestion mémoire appropriée, un monitoring complet et une organisation modulaire du code en utilisant LispE PyTorch.

## Exemples Pratiques

### Exemple 1 : Classification Simple

```lisp
; Entraîner un classificateur simple avec tenseurs PyTorch
(defun train_simple_classifier()
    ; Créer des tenseurs d'exemple
    (setq train_data (tensor_randn (integers 100 784)))  ; 100 exemples
    (setq train_labels (tensor_randint 0 10 (integers 100)))  ; 10 classes
    
    ; Créer couches
    (setq linear1 (torch_linear 784 128))
    (setq linear2 (torch_linear 128 10))
    
    ; Créer optimiseur
    (setq optimizer (torch_adamw_optimizer 0.001))
    (torch_optimizer_add_params optimizer linear1)
    (torch_optimizer_add_params optimizer linear2)
    
    ; Boucle d'entraînement
    (loop epoch 50
        ; Forward pass
        (setq h1 (tensor_relu (torch_linear_forward linear1 train_data)))
        (setq predictions (torch_linear_forward linear2 h1))
        (setq loss (torch_crossentropy_loss predictions train_labels))
        
        (torch_optimizer_zero_grad optimizer)
        (torch_backward loss)
        (torch_optimizer_step optimizer)
        
        (if (== (% epoch 10) 0)
            (println "Epoch" epoch "Loss:" (tensor_item loss)))
    )
    
    model
)
```

### Exemple 2 : Fine-tuning d'un Modèle de Langage

```lisp
; Fine-tuning complet avec LoRA
(defun finetune_language_model(model_path dataset_path)
    ; Configuration
    (setq config (dictionary
        "device" "mps"
        "dtype" "float16"
        "low_memory" true
    ))
    
    ; Charger modèle et tokenizer
    (setq model_name "llama_ft")
    (torch_hf_load_model_lora model_path config model_name)
    
    (setq tokenizer (Tokenizer (+ model_path "/tokenizer")))
    (withclass Tokenizer (tokenizer (configure)))
    
    ; Initialiser LoRA
    (setq lora_config (dictionary
        "rank" 16
        "alpha" 32
        "target_modules" (strings "q_proj" "k_proj" "v_proj" "o_proj")
    ))
    (torch_hf_lora_init model_name lora_config)
    
    ; Charger dataset
    (setq dataset (json_parse (fread dataset_path)))
    
    ; Configuration d'entraînement
    (setq train_config (dictionary
        "learning_rate" 2e-4
        "num_epochs" 3
        "batch_size" 1
        "max_seq_length" 512
    ))
    
    ; Créer optimiseur
    (setq lora_params (torch_hf_lora_get_parameters model_name))
    (setq learning_rate (@ train_config "learning_rate"))
    (setq optimizer (torch_adamw_optimizer learning_rate))
    (torch_optimizer_add_params optimizer lora_params)
    
    ; Boucle d'entraînement
    (loop epoch (@ train_config "num_epochs")
        (println "=== Epoch" (+ epoch 1) "===")
        
        (loop sample_idx (size dataset)
            (setq sample (@ dataset sample_idx))
            (setq instruction (@ sample "instruction"))
            (setq response (@ sample "output"))
            
            ; Préparer les tokens
            (setq formatted_text (+
                "<|start_header_id|>user<|end_header_id|>\n"
                instruction
                "<|eot_id|><|start_header_id|>assistant<|end_header_id|>\n"
                response
                "<|eot_id|>"
            ))
            
            (setq tokens (tokenizer (encode formatted_text)))
            (setq input_tensor (tensor_unsqueeze (tensor_create tokens) 0))
            
            ; Forward pass
            (setq logits (torch_hf_forward model_name input_tensor))
            
            ; Calcul de la loss (language modeling)
            (setq loss (calculate_language_modeling_loss logits input_tensor))
            
            ; Backward pass
            (torch_optimizer_zero_grad optimizer)
            (torch_backward loss)
            (torch_optimizer_step optimizer)
            
            ; Logging
            (if (== (% sample_idx 50) 0)
                (println "  Sample" sample_idx "/" (size dataset) 
                        "Loss:" (tensor_item loss)))
        )
        
        ; Sauvegarder les adaptateurs LoRA
        (setq checkpoint_path (+ "./checkpoints/epoch_" epoch))
        (torch_hf_lora_save model_name checkpoint_path)
        (println "✓ Checkpoint sauvegardé:" checkpoint_path)
    )
    
    model_name
)
```

### Exemple 3 : Génération Interactive

```lisp
; Système de génération interactive
(defun interactive_generation(model_path tokenizer_path)
    ; Charger le modèle
    (setq config (dictionary "device" "mps"))
    (setq model_id (torch_hf_load_model model_path config))
    
    ; Charger le tokenizer
    (setq tok (Tokenizer tokenizer_path))
    (withclass Tokenizer (tok (configure)))
    
    ; Boucle interactive
    (loop true
        (print "Prompt (ou 'quit' pour sortir): ")
        (setq user_input (input))
        
        (check (== user_input "quit")
            (break)
        )
        
        ; Générer la réponse
        (println "\nGénération en cours...")
        (setq response (generate_response model_id tok user_input))
        (println "Réponse générée.")
        (println)
    )
)

(defun generate_response(model_id tokenizer prompt)
    ; Encoder le prompt
    (setq prompt_tokens (tokenizer (encode prompt)))
    (setq context_id (torch_hf_enable_kv_cache model_id true))
    
    ; Générer la réponse
    (setq current_input (tensor_unsqueeze (tensor_create prompt_tokens) 0))
    (setq generated (clone prompt_tokens))
    
    (print "Réponse: ")
    (loop i 150  ; max_tokens
        (setq logits (torch_hf_forward model_id current_input context_id))
        (setq last_logits (tensor_select logits 1 -1))
        (setq last_logits (tensor_select last_logits 0 0))
        
        ; Sampling avec température
        (setq scaled_logits (tensor_div_scalar last_logits 0.7))
        (setq probs (tensor_softmax scaled_logits -1))
        (setq next_token (tensor_multinomial probs 1 true))
        (setq next_token_id (tensor_item next_token))
        
        ; Afficher le token
        (setq token_text (tokenizer (decode (integers next_token_id))))
        (print token_text)
        
        ; Vérifier fin de génération
        (check (== next_token_id (tokenizer (eos_id)))
            (break)
        )
        
        (push generated next_token_id)
        (setq current_input (tensor_unsqueeze (tensor_create (integers next_token_id)) 0))
    )
    
    (println)
    generated
)
```

## Bonnes Pratiques et Optimisations

### Gestion Mémoire

```lisp
; Utiliser des tenseurs optimisés LispE
(setq data (floats 1.0 2.0 3.0))  ; Plus efficace que (list 1.0 2.0 3.0)

; Libérer explicitement la mémoire GPU si nécessaire
(torch_cuda_empty_cache)  ; CUDA
```

### Performance

```lisp
; Utiliser le bon device pour vos tenseurs
(setq tensor (tensor_randn (integers 100 100)))
; Déplacer vers le device approprié selon le besoin
(if (equal device "cuda")
    (setq tensor (torch_to_cuda tensor))
    (if (equal device "mps")
        (setq tensor (torch_to_mps tensor))))

; Grouper les opérations pour éviter les synchronisations
(setq result (tensor_add 
    (tensor_mul a b) 
    (tensor_div c d)))
```

### Débogage

```lisp
; Vérifier les formes des tenseurs
(defun debug_tensor(tensor name)
    (println name "- Forme:" (tensor_shape tensor))
)

; Surveiller l'utilisation mémoire
(defun memory_info()
    (if (torch_cuda_is_available)
        (println "Mémoire CUDA utilisée:" (torch_cuda_memory_allocated) "bytes")
        (println "Utilisation CPU"))
)
```

# Fonctions Disponibles

Cette section liste toutes les fonctions disponibles dans la librairie LispE PyTorch, organisées par famille et par ordre alphabétique.

## Fonctions Attention

### `torch_attention_forward(attention query key value)`
Applique l'attention multi-têtes aux tenseurs query, key et value avec un module d'attention pré-créé.

### `torch_attention_mask(sequences pad_token)`
Crée un masque d'attention pour masquer les tokens de padding dans les séquences.

### `torch_flash_attention(query key value)`
Applique l'algorithme Flash Attention optimisé pour la mémoire aux tenseurs Q, K, V.

### `torch_flash_attention_create(embed_dim num_heads dropout bias)`
Crée un module Flash Attention avec la dimension d'embedding, le nombre de têtes, le taux de dropout et l'option bias.

### `torch_flash_attention_forward(flash_attention query key value)`
Effectue la propagation avant du module Flash Attention avec les tenseurs Q, K, V.

### `torch_flash_attention_with_dropout(flash_attention query key value dropout_p training)`
Applique Flash Attention avec un taux de dropout personnalisé en mode entraînement ou évaluation.

### `torch_flash_attention_with_mask(flash_attention query key value attn_mask)`
Applique Flash Attention avec un masque d'attention pour masquer certaines positions.

### `torch_(embed_dim num_heads)`
Crée un module d'attention multi-têtes avec la dimension d'embedding et le nombre de têtes spécifiés.

### `torch_scaled_dot_product_attention(query key value attn_mask dropout_p is_causal scale)`
Implémente l'attention scaled dot-product avec masque, dropout, option causale et facteur d'échelle.

## Fonctions Device et GPU

### `torch_cuda_device_count()`
Retourne le nombre de dispositifs CUDA disponibles sur le système.

### `torch_cuda_empty_cache()`
Vide le cache mémoire CUDA pour libérer la mémoire GPU non utilisée.

### `torch_cuda_is_available()`
Vérifie si CUDA est disponible sur le système.

### `torch_cuda_memory_allocated()`
Retourne la quantité de mémoire CUDA actuellement allouée en octets.

### `torch_cuda_memory_total()`
Retourne la quantité totale de mémoire CUDA disponible en octets.

### `torch_get_best_device()`
Détermine automatiquement le meilleur device disponible (CUDA, MPS ou CPU).

### `torch_mps_is_available()`
Vérifie si Metal Performance Shaders (MPS) est disponible sur Apple Silicon.

### `torch_mps_synchronize((safemode))`
Synchronise les opérations MPS avec un mode sécurisé optionnel.

### `torch_on_mps(tensor)`
Vérifie si un tenseur est sur le device MPS.

### `torch_set_device(device)`
Définit le device par défaut pour les opérations PyTorch.
Noms du device: "mps", "cuda", "cuda:n" ou "cpu".

### `torch_to_cpu(tensor)`
Déplace un tenseur vers le "cpu"".

### `torch_to_cuda(tensor (device))`
Déplace un tenseur vers un device CUDA spécifique ou par défaut.
Noms: "cuda", "cuda:n", où n est un index numérique définissant un GPU.

### `torch_to_mps(tensor)`
Déplace un tenseur vers le device "mps" (Apple Silicon).

### `torch_to_device(tensor)`
Déplace un tenseur vers un device "mps", "cuda" ou "cpu".

## Fonctions Embedding et Encodage

### `torch_apply_rotary_pos_emb(tensor cos sin)`
Applique l'embedding positionnel rotatif (RoPE) à un tenseur avec les cosinus et sinus précalculés.

### `torch_embedding(num_embeddings embedding_dim)`
Crée une couche d'embedding avec le nombre d'embeddings et la dimension spécifiés.

### `torch_embedding_forward(embedding input_data)`
Effectue la propagation avant d'une couche d'embedding avec les données d'entrée.

### `torch_positional_encoding(d_model max_len)`
Crée un module d'encodage positionnel avec la dimension du modèle et la longueur maximale.

### `torch_positional_forward(positional_encoding input_data)`
Applique l'encodage positionnel aux données d'entrée.

### `torch_rotary_embedding(dim max_seq_len)`
Crée un module d'embedding positionnel rotatif (RoPE) avec la dimension et la longueur maximale.

### `torch_rotary_forward(rotary_embedding seq_len device)`
Calcule les cosinus et sinus pour l'embedding rotatif pour une longueur de séquence donnée.

## Fonctions Flash Attention (voir Fonctions Attention)

## Fonctions Génération de Texte

### `torch_generate(generator input_ids strategy)`
Génère du texte à partir d'un générateur avec les tokens d'entrée et la stratégie spécifiée.



## Fonctions Gradient Checkpointing

### `torch_checkpoint_create(module)`
Crée un module avec gradient checkpointing pour économiser la mémoire pendant l'entraînement.

### `torch_checkpoint_disable(module)`
Désactive le gradient checkpointing pour un module.

### `torch_checkpoint_enable(module)`
Active le gradient checkpointing pour un module.

### `torch_checkpoint_forward(module input_data)`
Effectue la propagation avant avec gradient checkpointing.

## Fonctions Hugging Face

### `torch_hf_clear_attention_scores(path (kvcache))`
Efface les scores d'attention stockés dans le cache.

### `torch_hf_embeddings(path token_ids)`
Obtient les embeddings pour une liste de tokens à partir d'un modèle Hugging Face.

### `torch_hf_enable_kv_cache(path enable)`
Active ou désactive le cache Key-Value pour un modèle Hugging Face.

### `torch_hf_forward(path input_ids (kvcache))`
Effectue la propagation avant d'un modèle Hugging Face avec cache KV optionnel.

### `torch_hf_forward_attention_scores(path layer_index (kvcache))`
Obtient les scores d'attention pour une couche spécifique du modèle.

### `torch_hf_forward_attention_size(path (kvcache))`
Retourne la taille des tenseurs d'attention du modèle.

### `torch_hf_forward_manual(path input_ids (kvcache))`
Effectue une propagation avant manuelle avec contrôle détaillé du cache.

### `torch_hf_generate(path initial_tokens eos_id max_length (options))`
Génère du texte à partir d'un modèle Hugging Face avec tokens initiaux et paramètres.

**Paramètres :**
- `path` : Chemin/ID du modèle chargé
- `initial_tokens` : Tokens d'entrée (liste d'entiers)
- `eos_id` : ID(s) du token de fin (entier ou liste d'entiers)
- `max_length` : Nombre maximum de tokens à générer
- `options` : Dictionnaire optionnel avec :
  - `"topk"` (integer) : Top-K sampling - considère seulement les K meilleurs tokens
  - `"topp"` (float) : Top-P/Nucleus sampling - probabilité cumulative (0.0-1.0)
  - `"greedy"` (boolean) : Sampling glouton - sélectionne toujours le token le plus probable
  - `"callback"` (function) : Fonction appelée pour chaque token généré
    - Signature: `(callback token_id data)`
    - `token_id` (integer) : ID du token généré
    - `data` : Données passées via le paramètre "data"
    - Peut être une référence de fonction (avec quote) ou lambda
  - `"data"` (any) : Données passées au callback (ex: tokenizer, contexte)

**Stratégies de sampling (mutuellement exclusives) :**
- **Aucune** : Sampling simple avec softmax + multinomial (défaut)
- **`"greedy"`** : Déterministe, sélectionne le token le plus probable
- **`"topk"`** : Limite aux K tokens les plus probables
- **`"topp"`** : Nucleus sampling, limite à la probabilité cumulative P

### `torch_hf_get_down_weight(path layer)`
Récupère les poids de la projection down d'une couche FFN spécifique.

### `torch_hf_get_gate_up_fused_weight(path layer)`
Récupère les poids fusionnés gate et up d'une couche FFN.

### `torch_hf_get_gate_weight(path layer)`
Récupère les poids de la projection gate d'une couche FFN spécifique.

### `torch_hf_get_k_weight(path layer)`
Récupère les poids de la projection Key d'une couche d'attention spécifique.

### `torch_hf_get_o_weight(path layer)`
Récupère les poids de la projection Output d'une couche d'attention spécifique.

### `torch_hf_get_q_weight(path layer)`
Récupère les poids de la projection Query d'une couche d'attention spécifique.

### `torch_hf_get_qkv_fused_weight(path layer)`
Récupère les poids fusionnés Query, Key, Value d'une couche d'attention.

### `torch_hf_get_rms_norm_eps(path)`
Récupère la valeur epsilon utilisée pour la normalisation RMS du modèle.

### `torch_hf_get_up_weight(path layer)`
Récupère les poids de la projection up d'une couche FFN spécifique.

### `torch_hf_get_v_weight(path layer)`
Récupère les poids de la projection Value d'une couche d'attention spécifique.

### `torch_hf_get_weight(path name)`
Récupère un tenseur de poids spécifique du modèle par son nom.

### `torch_hf_list_weights(path)`
Liste tous les noms des tenseurs de poids disponibles dans le modèle.

### `torch_hf_load_model(path (config nil))`
Charge un modèle Hugging Face depuis un chemin avec configuration optionnelle.

**Paramètres du dictionnaire config :**
- `"device"` (string) : Device cible ("cuda", "mps", "cpu")
- `"max_seq_len"` (integer) : Longueur maximale de séquence (défaut: selon config.json)
- `"rope_scaling"` (float) : Facteur d'échelle RoPE (défaut: 1.0)
- `"temperature"` (float) : Température de génération (défaut: 1.0)
- `"top_p"` (float) : Nucleus sampling (défaut: 1.0)
- `"top_k"` (integer) : Top-K sampling (défaut: 0 = désactivé)
- `"repetition_penalty"` (float) : Pénalité de répétition (défaut: 1.0)
- `"use_kv_cache"` (boolean) : Activer cache KV (défaut: true)
- `"max_cache_len"` (integer) : Taille max cache (défaut: max_seq_len)
- `"manual_attention"` (boolean) : Mode attention manuel (défaut: false)

### `torch_hf_lora_enable(model_name enable)`
Active ou désactive les adaptateurs LoRA pour un modèle.

### `torch_hf_lora_get_parameters(model_name)`
Récupère les paramètres LoRA d'un modèle pour l'optimisation.

### `torch_hf_lora_init(model_name rank alpha target_modules (dtype))`
Initialise les adaptateurs LoRA pour un modèle avec rang, alpha et modules cibles.

### `torch_hf_lora_load(model_name path)`
Charge des adaptateurs LoRA pré-entraînés depuis un fichier.

### `torch_hf_lora_merge(model_name)`
Fusionne les adaptateurs LoRA avec les poids principaux du modèle.

### `torch_hf_lora_save(model_name path)`
Sauvegarde les adaptateurs LoRA entraînés dans un fichier.

### `torch_hf_lora_unmerge(model_name)`
Sépare les adaptateurs LoRA des poids principaux du modèle.

### `torch_hf_memory_usage(path)`
Retourne l'utilisation mémoire d'un modèle Hugging Face chargé.

### `torch_hf_model_info(path)`
Récupère des informations détaillées sur un modèle Hugging Face.

### `torch_hf_model_summary(path)`
Affiche un résumé des caractéristiques du modèle Hugging Face.

### `torch_hf_reset_kv_cache(kvcache)`
Remet à zéro le cache Key-Value pour repartir d'une séquence vide.

## Fonctions JIT (TorchScript)

### `torch_jit_load(model_path (device "cpu"))`
Charge un modèle TorchScript depuis un fichier avec device optionnel.

### `torch_jit_model_forward(model tensor)`
Effectue la propagation avant d'un modèle JIT avec un tenseur d'entrée.

### `torch_jit_model_forward_with_lora(model input)`
Effectue la propagation avant d'un modèle JIT avec adaptateurs LoRA.

### `torch_jit_model_get_buffer(model buffername)`
Récupère un buffer spécifique d'un modèle JIT par son nom.

### `torch_jit_model_get_intermediate_states(model input layer_names)`
Obtient les états intermédiaires de couches spécifiques pendant la propagation.

### `torch_jit_model_get_tensor(model tensorname)`
Récupère un tenseur spécifique d'un modèle JIT par son nom.

### `torch_jit_model_get_tensor_shape(model tensorname)`
Retourne la forme d'un tenseur spécifique dans un modèle JIT.

### `torch_jit_model_info(model)`
Affiche des informations détaillées sur un modèle JIT.

### `torch_jit_model_list_buffers(model)`
Liste tous les buffers disponibles dans un modèle JIT.

### `torch_jit_model_list_methods(model)`
Liste toutes les méthodes disponibles dans un modèle JIT.

### `torch_jit_model_list_parameter_names(model)`
Liste tous les noms de paramètres dans un modèle JIT.

### `torch_jit_model_list_tensor_names(model)`
Liste tous les noms de tenseurs disponibles dans un modèle JIT.

### `torch_jit_model_register_lora_hook(model layer_name lora_layer)`
Enregistre un hook LoRA pour une couche spécifique d'un modèle JIT.

### `torch_jit_model_to_best_device(model)`
Déplace un modèle JIT vers le meilleur device disponible.

### `torch_jit_model_to_device(model (device "cpu"))`
Déplace un modèle JIT vers un device spécifique.

### `torch_jit_model_to_mps(model)`
Déplace un modèle JIT vers le device MPS (Apple Silicon).

### `torch_jit_model_update_weight(model param_name new_weight)`
Met à jour un poids spécifique d'un modèle JIT.

### `torch_jit_unload(model)`
Décharge un modèle JIT de la mémoire.

## Fonctions Learning Rate Scheduling

### `torch_lr_scheduler(optimizer scheduler_type config)`
Crée un planificateur de taux d'apprentissage avec type et configuration.

### `torch_scheduler_get_lr(scheduler)`
Obtient le taux d'apprentissage actuel d'un planificateur.

### `torch_scheduler_set_lr(scheduler learning_rate)`
Définit un nouveau taux d'apprentissage pour un planificateur.

### `torch_scheduler_step(scheduler)`
Effectue une étape de planification pour mettre à jour le taux d'apprentissage.

## Fonctions LoRA

### `torch_hf_load_model_lora(model_name path config)`
Charge un modèle Hugging Face avec support LoRA intégré.

### `torch_lora_apply_to_linear(linear_layer rank alpha)`
Applique des adaptateurs LoRA à une couche linéaire existante.

### `torch_lora_compute_delta(lora_layer)`
Calcule la matrice delta (A×B) d'une couche LoRA.

### `torch_lora_forward(lora_layer input_data)`
Effectue la propagation avant d'une couche LoRA.

### `torch_lora_forward_with_gradients(lora_layer input retain_graph)`
Effectue la propagation avant LoRA en conservant le graphe de calcul pour les gradients.

### `torch_lora_get_adaptation_magnitude(lora_layer)`
Calcule la magnitude de l'adaptation LoRA par rapport aux poids originaux.

### `torch_lora_linear(in_features out_features rank alpha)`
Crée une couche linéaire avec adaptateurs LoRA intégrés.

### `torch_lora_load_adapters(model path)`
Charge des adaptateurs LoRA sauvegardés dans un modèle.

### `torch_lora_merge_weights(lora_layer)`
Fusionne les poids LoRA avec les poids de base de la couche.

### `torch_lora_save_adapters(model path)`
Sauvegarde les adaptateurs LoRA d'un modèle dans un fichier.

### `torch_lora_trainable_params(model)`
Récupère uniquement les paramètres LoRA entraînables d'un modèle.

## Fonctions Loss (Perte)

### `torch_backward(loss)`
Effectue la rétropropagation à partir d'un tenseur de perte.

### `torch_cross_entropy(predictions targets)`
Calcule la perte d'entropie croisée entre prédictions et cibles.

### `torch_crossentropy_loss(predictions targets)`
Calcule la perte d'entropie croisée (alias de torch_cross_entropy).

### `torch_mse_loss(predictions targets)`
Calcule la perte d'erreur quadratique moyenne entre prédictions et cibles.

## Fonctions Modèles

### `torch_forward(model input_data)`
Effectue la propagation avant d'un modèle avec des données d'entrée.

### `torch_load_checkpoint(path)`
Charge un checkpoint complet contenant modèle, optimiseur et époque.

### `torch_load_model(model path)`
Charge les poids d'un modèle depuis un fichier.

### `torch_load_state_dict(model state_dict)`
Charge un dictionnaire d'état dans un modèle.

### `torch_model(input_size hidden_size output_size)`
Crée un modèle MLP simple avec tailles d'entrée, cachée et sortie spécifiées.

### `torch_save_checkpoint(model optimizer epoch path)`
Sauvegarde un checkpoint complet avec modèle, optimiseur et numéro d'époque.

### `torch_save_model(model path)`
Sauvegarde les poids d'un modèle dans un fichier.

### `torch_state_dict(model)`
Récupère le dictionnaire d'état d'un modèle (tous les paramètres).

## Fonctions Neural Network

### `torch_linear(in_features out_features)`
Crée une couche linéaire (dense) avec nombre de features d'entrée et de sortie.

### `torch_layer_norm(normalized_shape)`
Crée une couche de normalisation avec la forme spécifiée.

### `torch_layer_norm_forward(layer_norm input_data)`
Applique la normalisation de couche aux données d'entrée.

### `torch_linear_forward(linear input_data)`
Effectue la propagation avant d'une couche linéaire.

### `torch_transformer_block(embed_dim num_heads ffn_dim)`
Crée un bloc Transformer avec dimension d'embedding, nombre de têtes et dimension FFN.

### `torch_transformer_forward(block input_data)`
Effectue la propagation avant d'un bloc Transformer.

## Fonctions Optimisation

### `torch_adam_optimizer(learning_rate)`
Crée un optimiseur Adam avec le taux d'apprentissage spécifié.

### `torch_adamw_optimizer(learning_rate)`
Crée un optimiseur AdamW avec le taux d'apprentissage spécifié.

### `torch_clip_grad_norm(optimizer max_norm)`
Applique le clipping de gradient par norme pour éviter l'explosion des gradients.

### `torch_optimizer(model learning_rate type)`
Crée un optimiseur générique pour un modèle avec taux d'apprentissage et type.

### `torch_optimizer_add_params(params learning_rate weight_decay)`
Ajoute des paramètres à un optimiseur avec taux d'apprentissage et décroissance de poids.

### `torch_optimizer_step(optimizer)`
Effectue une étape d'optimisation (mise à jour des poids).

### `torch_optimizer_zero_grad(optimizer)`
Remet à zéro les gradients de tous les paramètres de l'optimiseur.

### `torch_set_grad_enabled(enabled)`
Active ou désactive le calcul des gradients globalement.

### `torch_sgd_optimizer(learning_rate)`
Crée un optimiseur SGD (Stochastic Gradient Descent) avec le taux d'apprentissage.

## Fonctions Quantification

### `torch_dequantize(quantized_tensor)`
Dequantifie un tenseur quantifié vers sa représentation en virgule flottante.

### `torch_model_quantize_dynamic(model)`
Applique la quantification dynamique à un modèle complet.

### `torch_model_quantize_static(model calibration_data)`
Applique la quantification statique à un modèle avec données de calibration.

### `torch_quantize_dynamic(tensor dtype)`
Applique la quantification dynamique à un tenseur avec le type de données spécifié.

### `torch_quantize_fp16(tensor)`
Quantifie un tenseur en précision half (16-bit float).

### `torch_quantize_int8(tensor)`
Quantifie un tenseur en entiers 8-bit.

### `torch_quantize_linear(tensor scale zero_point)`
Applique la quantification linéaire avec facteur d'échelle et point zéro.

### `torch_quantize_per_channel(tensor scales zero_points axis)`
Applique la quantification par canal avec facteurs d'échelle et points zéro.

### `torch_quantize_static(tensor scale zero_point dtype)`
Applique la quantification statique avec paramètres fixes.

## Fonctions Sampling

### `tensor_multinomial(probs num_samples replacement)`
Échantillonne à partir d'une distribution multinomiale avec ou sans remplacement.

### `torch_sort(tensor dim descending)`
Trie un tenseur le long d'une dimension en ordre croissant ou décroissant.

### `torch_topk(tensor k dim largest)`
Retourne les k plus grandes (ou plus petites) valeurs le long d'une dimension.

## Fonctions Tenseur - Activation

### `tensor_abs(tensor)`
Calcule la valeur absolue de chaque élément du tenseur.

### `tensor_gelu(tensor)`
Applique la fonction d'activation GELU (Gaussian Error Linear Unit).

### `tensor_relu(tensor)`
Applique la fonction d'activation ReLU (Rectified Linear Unit).

### `tensor_sigmoid(tensor)`
Applique la fonction d'activation sigmoid.

### `tensor_silu(tensor)`
Applique la fonction d'activation SiLU (Sigmoid Linear Unit, aussi appelée Swish).

### `tensor_softmax(tensor dim)`
Applique la fonction softmax le long de la dimension spécifiée.

### `tensor_tanh(tensor)`
Applique la fonction d'activation tangente hyperbolique.

## Fonctions Tenseur - Arithmétique

### `tensor_add(tensor1 tensor2)`
Addition élément par élément de deux tenseurs.

### `tensor_add_scalar(tensor scalar)`
Addition d'un scalaire à tous les éléments d'un tenseur.

### `tensor_div(tensor1 tensor2)`
Division élément par élément de deux tenseurs.

### `tensor_div_scalar(tensor scalar)`
Division de tous les éléments d'un tenseur par un scalaire.

### `tensor_matmul(tensor1 tensor2)`
Multiplication matricielle de deux tenseurs.

### `tensor_mul(tensor1 tensor2)`
Multiplication élément par élément de deux tenseurs.

### `tensor_mul_scalar(tensor scalar)`
Multiplication de tous les éléments d'un tenseur par un scalaire.

### `tensor_neg(tensor)`
Calcule la négation de chaque élément du tenseur.

### `tensor_reciprocal(tensor)`
Calcule l'inverse (1/x) de chaque élément du tenseur.

### `tensor_sub(tensor1 tensor2)`
Soustraction élément par élément de deux tenseurs.

## Fonctions Tenseur - Création

### `tensor_cat(tensors dim)`
Concatène une liste de tenseurs le long de la dimension spécifiée.

### `tensor_full(shape value)`
Crée un tenseur de la forme spécifiée rempli avec une valeur.

### `tensor_full_like(tensor fill_value)`
Crée un tenseur de même forme qu'un tenseur existant, rempli avec une valeur.

### `tensor_ones(shape)`
Crée un tenseur de la forme spécifiée rempli de uns.

### `tensor_randn(shape)`
Crée un tenseur de la forme spécifiée avec des valeurs aléatoires normales.

### `tensor_randint(low high shape)`
Crée un tenseur d'entiers aléatoires entre low (inclus) et high (exclus).

### `tensor_create(thedata) ou torch_tensor(thedata)`
Crée un tenseur à partir de données LispE (listes, matrices).

### `tensor_zeros(shape)`
Crée un tenseur de la forme spécifiée rempli de zéros.

## Fonctions Tenseur - Information

### `tensor_item(tensor)`
Extrait la valeur scalaire d'un tenseur à un seul élément.

### `tensor_shape(tensor)`
Retourne la forme (dimensions) d'un tenseur.

### `tensor_size(tensor)`
Retourne la taille totale (nombre d'éléments) d'un tenseur.

### `tensor_to_list(tensor)`
Convertit un tenseur PyTorch en liste LispE.

## Fonctions Tenseur - Manipulation

### `tensor_clamp(tensor min_val max_val)`
Limite les valeurs d'un tenseur entre min_val et max_val.

### `tensor_contiguous(tensor)`
S'assure qu'un tenseur est stocké de manière contiguë en mémoire.

### `tensor_cumsum(tensor dim)`
Calcule la somme cumulative le long d'une dimension.

### `tensor_gather(input dim index)`
Collecte des valeurs le long d'une dimension selon un tenseur d'indices.

### `tensor_masked_fill_(tensor mask value)`
Remplace les éléments du tenseur par une valeur où le masque est vrai.

### `tensor_reshape(tensor shape)`
Change la forme d'un tenseur sans modifier ses données.

### `tensor_select(tensor dim index)`
Sélectionne une tranche le long d'une dimension à un index spécifique.

### `tensor_set_item(tensor indices value)`
Définit la valeur à des indices spécifiques dans un tenseur.

### `tensor_slice(tensor dim start end)`
Extrait une tranche d'un tenseur le long d'une dimension.

### `tensor_squeeze(tensor dim)`
Supprime les dimensions de taille 1 du tenseur.

### `tensor_transpose(tensor dim0 dim1)`
Transpose deux dimensions d'un tenseur.

### `tensor_triu(tensor diagonal)`
Retourne la partie triangulaire supérieure d'un tenseur matrice.

### `tensor_unsqueeze(tensor dim)`
Ajoute une dimension de taille 1 à la position spécifiée.

## Fonctions Tenseur - Mathématiques

### `tensor_acos(tensor)`
Calcule l'arc cosinus de chaque élément du tenseur.

### `tensor_asin(tensor)`
Calcule l'arc sinus de chaque élément du tenseur.

### `tensor_atan(tensor)`
Calcule l'arc tangente de chaque élément du tenseur.

### `tensor_ceil(tensor)`
Arrondit chaque élément du tenseur vers le haut (plafond).

### `tensor_cos(tensor)`
Calcule le cosinus de chaque élément du tenseur.

### `tensor_cosh(tensor)`
Calcule le cosinus hyperbolique de chaque élément du tenseur.

### `tensor_einsum(indices tensors)`
Effectue la sommation d'Einstein sur les tenseurs selon la notation spécifiée.

### `tensor_exp(tensor)`
Calcule l'exponentielle de chaque élément du tenseur.

### `tensor_floor(tensor)`
Arrondit chaque élément du tenseur vers le bas (plancher).

### `tensor_linear(tensor1 tensor2)`
Applique une transformation linéaire (multiplication matricielle + biais optionnel).

### `tensor_log(tensor)`
Calcule le logarithme naturel de chaque élément du tenseur.

### `tensor_log10(tensor)`
Calcule le logarithme en base 10 de chaque élément du tenseur.

### `tensor_log2(tensor)`
Calcule le logarithme en base 2 de chaque élément du tenseur.

### `tensor_log_softmax(tensor dim)`
Applique log(softmax(x)) de manière numériquement stable.

### `tensor_pow(tensor exponent)`
Élève chaque élément du tenseur à la puissance de l'exposant.

### `tensor_rms_norm(input weight (eps 1e-6))`
Applique la normalisation RMS (Root Mean Square) avec poids et epsilon.

### `tensor_round(tensor)`
Arrondit chaque élément du tenseur à l'entier le plus proche.

### `tensor_rsqrt(tensor)`
Calcule l'inverse de la racine carrée de chaque élément.

### `tensor_sin(tensor)`
Calcule le sinus de chaque élément du tenseur.

### `tensor_sinh(tensor)`
Calcule le sinus hyperbolique de chaque élément du tenseur.

### `tensor_sqrt(tensor)`
Calcule la racine carrée de chaque élément du tenseur.

### `tensor_tan(tensor)`
Calcule la tangente de chaque élément du tenseur.

## Fonctions Tenseur - Réduction

### `tensor_argmax(tensor dim (keepdim true))`
Retourne les indices des valeurs maximales le long d'une dimension.

### `tensor_max(tensor)`
Retourne la valeur maximale du tenseur.

### `tensor_mean(tensor)`
Calcule la moyenne de tous les éléments du tenseur.

### `tensor_mean_dim(tensor dim)`
Calcule la moyenne le long d'une dimension spécifique.

### `tensor_min(tensor)`
Retourne la valeur minimale du tenseur.

### `tensor_std(tensor)`
Calcule l'écart-type de tous les éléments du tenseur.

### `tensor_sum(tensor)`
Calcule la somme de tous les éléments du tenseur.

## Fonctions Tokenisation

### `torch_decode(tokenizer token_ids)`
Décode une liste d'IDs de tokens en texte avec un tokenizer.

### `torch_encode(tokenizer text)`
Encode un texte en liste d'IDs de tokens avec un tokenizer.

### `torch_pad_sequences(sequences max_length pad_token)`
Remplit des séquences pour qu'elles aient toutes la même longueur.

### `torch_sentencepiece_tokenizer(model_path)`
Crée un tokenizer SentencePiece à partir d'un modèle pré-entraîné.

### `torch_simple_tokenizer()`
Crée un tokenizer simple basé sur les espaces.

### `torch_train_sentencepiece(input_file model_prefix vocab_size model_type)`
Entraîne un nouveau modèle SentencePiece sur un fichier d'entrée.

### `torch_vocab_size(tokenizer)`
Retourne la taille du vocabulaire d'un tokenizer.

## Fonctions Tucker Decomposition

### `torch_khatri_rao_product(A B)`
Calcule le produit de Khatri-Rao de deux matrices.

### `torch_tucker_compression_ratio(original_shape core_shape factor_shapes)`
Calcule le ratio de compression d'une décomposition de Tucker.

### `torch_tucker_decomposition(tensor rank (max_iter 100) (tol 1e-6))`
Effectue la décomposition de Tucker d'un tenseur avec rang et paramètres.

### `torch_tucker_reconstruct(core factors)`
Reconstruit un tenseur à partir de son cœur et facteurs de décomposition Tucker.

## Fonctions Utilitaires

### `tensor_in_memory()`
Retourne des informations sur les tenseurs actuellement en mémoire.

---

Ce manuel couvre les aspects essentiels de la librairie LispE PyTorch. Pour des cas d'usage spécifiques ou des fonctionnalités avancées, consultez les exemples dans le dépôt et la documentation des fonctions individuelles.