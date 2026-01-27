; Test de la nouvelle bibliothèque GGUF basée sur llama.cpp
; Cette version utilise directement llama.cpp au lieu de PyTorch
; pour de meilleures performances avec MXFP4

(use 'lispe_gguf)

(println "=== Test GGUF avec llama.cpp (v2) ===\n")

; Chemin du modèle MXFP4
(setq model-path "/Users/clauderoux/.lmstudio/models/lmstudio-community/gpt-oss-20b-GGUF/gpt-oss-20b-MXFP4.gguf")

(println "Fichier:" model-path)
(println "")

; Test 1: Chargement du modèle
(println "Test 1: Chargement du modèle...")
(setq model (gguf_load model-path 0 2048))

(if (nullp model)
    (println "❌ Échec du chargement\n")
    (block
        (println "✓ Modèle chargé avec succès!")
        (println "")))

; Test 2: Tokenization
(if (not (nullp model))
    (block
        (println "Test 2: Tokenization...")
        (setq test-text "Hello, how are you?")
        (println "Texte:" test-text)
        (setq tokens (gguf_tokenize model test-text))
        (println "Tokens:" tokens)
        (println "Nombre de tokens:" (size tokens))
        (println "")))

; Test 3: Détokenization
(if (not (nullp model))
    (block
        (println "Test 3: Détokenization...")
        (setq decoded (gguf_detokenize model tokens))
        (println "Texte reconstruit:" decoded)
        (println "")))

; Test 4: Génération de texte
(if (not (nullp model))
    (block
        (println "Test 4: Génération de texte...")
        (println "Prompt: 'Once upon a time'")
        (println "")
        (setq generated (gguf_generate model "Once upon a time" 50 0.8 0.9 40))
        (println "")
        (println "Texte généré:" generated)
        (println "")))

(println "=== Tests terminés ===")
