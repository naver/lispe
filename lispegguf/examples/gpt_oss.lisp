; Exemple d'utilisation du modèle GPT-OSS-20B avec GGUF
; Modèle: gpt-oss-20b-MXFP4.gguf (format quantifié)

; Chargement de la bibliothèque
(use 'lispe_gguf)

; Chemin vers votre modèle
(setq model-path "/Users/clauderoux/.lmstudio/models/lmstudio-community/gpt-oss-20b-GGUF/gpt-oss-20b-MXFP4.gguf")

; ========================================
; EXEMPLE 1: Chargement et info du modèle
; ========================================

(defun load-gpt-oss ()
  (println "Chargement du modèle GPT-OSS-20B...")
  (println "Chemin:" model-path)

  ; Configuration optimisée pour un modèle de 20B
  (setq config {
    'use_mmap true           ; Memory mapping pour économiser la RAM
    'dequantize_on_load false ; Déquantification à la demande
    'num_threads 8            ; Nombre de threads (ajustez selon votre CPU)
  })

  ; Chargement sur CPU (changez en "cuda" si vous avez un GPU)
  (setq model (gguf-load-model model-path "mps" config))

  (if (null model)
      (block
        (println "ERREUR: Impossible de charger le modèle")
        (println "Vérifiez que le fichier existe et que vous avez assez de mémoire")
        null)
      (block
        (println "✓ Modèle chargé avec succès!\n")

        ; Afficher les informations du modèle
        (println "=== Informations du modèle ===")
        (gguf-print-info model)

        ; Récupérer les métadonnées importantes
        (setq info (gguf-model-info model))
        (println "\nDétails clés:")
        (println "- Architecture:" (@ info "architecture"))
        (println "- Vocabulaire:" (@ info "vocab_size") "tokens")
        (println "- Contexte max:" (@ info "context_length") "tokens")
        (println "- Dimensions embedding:" (@ info "embedding_length"))

        model)))

; ========================================
; EXEMPLE 2: Génération de texte simple
; ========================================

(defun generate-text (model prompt)
  (println "\n=== Génération de texte ===")
  (println "Prompt:" prompt)
  (println "")

  ; Tokenisation du prompt
  (setq tokens (gguf-tokenize model prompt))
  (println "Nombre de tokens du prompt:" (length tokens))

  ; Configuration de génération
  (setq gen-config {
    'max_new_tokens 100      ; Générer jusqu'à 100 nouveaux tokens
    'temperature 0.7         ; Température modérée (0.7 = équilibré)
    'top_p 0.9              ; Nucleus sampling
    'top_k 40               ; Top-k sampling
    'repetition_penalty 1.1  ; Pénalité contre les répétitions
  })

  ; Génération
  (println "Génération en cours...")
  (setq generated-tokens (gguf-generate model tokens gen-config))

  ; Conversion en texte
  (setq result (gguf-detokenize model generated-tokens))

  (println "\n--- Résultat ---")
  (println result)
  (println "----------------\n")

  result)

; ========================================
; EXEMPLE 3: Session interactive
; ========================================

(defun chat-gpt-oss (model)
  (println "\n=== Chat interactif avec GPT-OSS-20B ===")
  (println "Tapez 'quit' ou 'exit' pour quitter")
  (println "Tapez 'reset' pour réinitialiser le cache")
  (println "")

  (loop
    (print "\nVous: ")
    (setq input (read-line))

    ; Commandes spéciales
    (cond
      ((or (= input "quit") (= input "exit"))
       (block
         (println "Au revoir!")
         (break)))

      ((= input "reset")
       (block
         (gguf-reset-cache model)
         (println "Cache réinitialisé")))

      (true
       (block
         ; Génération de la réponse
         (setq tokens (gguf-tokenize model input))
         (setq response-tokens (gguf-generate model tokens {
           'max_new_tokens 150
           'temperature 0.7
           'top_p 0.9
           'repetition_penalty 1.15
         }))
         (setq response (gguf-detokenize model response-tokens))
         (println "\nGPT-OSS:" response))))))

; ========================================
; EXEMPLE 4: Test de créativité avec différentes températures
; ========================================

(defun test-temperatures (model prompt)
  (println "\n=== Test de différentes températures ===")
  (println "Prompt:" prompt)
  (println "")

  (setq tokens (gguf-tokenize model prompt))
  (setq temperatures '(0.3 0.7 1.0 1.5))

  (maplist temp temperatures
    (block
      (println "--- Température:" temp "(")
      (cond
        ((< temp 0.5) (println "    conservateur/déterministe)"))
        ((< temp 0.9) (println "    équilibré)"))
        ((< temp 1.2) (println "    créatif)"))
        (true (println "    très créatif/aléatoire)")))

      (setq result-tokens (gguf-generate model tokens {
        'max_new_tokens 50
        'temperature temp
        'top_p 0.9
      }))
      (setq result (gguf-detokenize model result-tokens))
      (println result)
      (println ""))))

; ========================================
; EXEMPLE 5: Complétion de code
; ========================================

(defun complete-code (model code-start)
  (println "\n=== Complétion de code ===")
  (println "Code de départ:")
  (println code-start)
  (println "")

  (setq tokens (gguf-tokenize model code-start))

  ; Configuration optimisée pour le code
  (setq result-tokens (gguf-generate model tokens {
    'max_new_tokens 200
    'temperature 0.2       ; Basse température pour plus de précision
    'top_p 0.95
    'repetition_penalty 1.05
  }))

  (setq completion (gguf-detokenize model result-tokens))

  (println "--- Code complété ---")
  (println completion)
  (println "--------------------\n")

  completion)

; ========================================
; FONCTION PRINCIPALE
; ========================================

(defun main ()
  (println "╔═══════════════════════════════════════╗")
  (println "║  Exemple GPT-OSS-20B GGUF            ║")
  (println "╚═══════════════════════════════════════╝\n")

  ; Chargement du modèle
  (setq my-model (load-gpt-oss))

  (if (not (null my-model))
      (block
        ; Exemple 1: Génération simple
        (generate-text my-model "La programmation fonctionnelle est")

        ; Exemple 2: Test de températures
        (test-temperatures my-model "Il était une fois")

        ; Exemple 3: Complétion de code
        (complete-code my-model "def fibonacci(n):\n    \"\"\"Calcule le n-ième nombre de Fibonacci\"\"\"\n    ")

        ; Exemple 4: Mode interactif (décommentez pour l'utiliser)
        ; (chat-gpt-oss my-model)

        (println "\n✓ Tous les exemples ont été exécutés avec succès!")
        (println "\nPour lancer le mode chat interactif, exécutez:")
        (println "  (chat-gpt-oss my-model)")

        my-model)
      (println "Échec du chargement du modèle")))

; ========================================
; UTILISATION RAPIDE
; ========================================

(println "")
(println "Commandes disponibles:")
(println "  (main)                          - Lance tous les exemples")
(println "  (setq m (load-gpt-oss))         - Charge seulement le modèle")
(println "  (generate-text m \"votre texte\") - Génère du texte")
(println "  (chat-gpt-oss m)                - Mode chat interactif")
(println "  (test-temperatures m \"texte\")   - Test de créativité")
(println "  (complete-code m \"code\")        - Complétion de code")
(println "")
