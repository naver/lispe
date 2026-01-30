; Example of using the GPT-OSS-20B model with GGUF
; Model: gpt-oss-20b-MXFP4.gguf (quantized format)

; Loading the library
(use 'lispe_gguf)

; Path to your model
(setq model-path "/Users/user/.lmstudio/models/lmstudio-community/gpt-oss-20b-GGUF/gpt-oss-20b-MXFP4.gguf")

; ========================================
; EXAMPLE 1: Loading and model info
; ========================================

(defun load-gpt-oss ()
  (println "Loading GPT-OSS-20B model...")
  (println "Path:" model-path)

  ; Optimized configuration for a 20B model
  (setq config {
    'use_mmap true           ; Memory mapping to save RAM
    'dequantize_on_load false ; Dequantization on demand
    'num_threads 8            ; Number of threads (adjust based on your CPU)
  })

  ; Loading on CPU (change to "cuda" if you have a GPU)
  (setq model (gguf-load-model model-path "mps" config))

  (if (null model)
      (block
        (println "ERROR: Unable to load the model")
        (println "Check that the file exists and you have enough memory")
        null)
      (block
        (println "✓ Model loaded successfully!\n")

        ; Display model information
        (println "=== Model Information ===")
        (gguf-print-info model)

        ; Retrieve important metadata
        (setq info (gguf-model-info model))
        (println "\nKey details:")
        (println "- Architecture:" (@ info "architecture"))
        (println "- Vocabulary:" (@ info "vocab_size") "tokens")
        (println "- Max context:" (@ info "context_length") "tokens")
        (println "- Embedding dimensions:" (@ info "embedding_length"))

        model)))

; ========================================
; EXAMPLE 2: Simple text generation
; ========================================

(defun generate-text (model prompt)
  (println "\n=== Text Generation ===")
  (println "Prompt:" prompt)
  (println "")

  ; Tokenization of the prompt
  (setq tokens (gguf-tokenize model prompt))
  (println "Number of prompt tokens:" (length tokens))

  ; Generation configuration
  (setq gen-config {
    'max_new_tokens 100      ; Generate up to 100 new tokens
    'temperature 0.7         ; Moderate temperature (0.7 = balanced)
    'top_p 0.9              ; Nucleus sampling
    'top_k 40               ; Top-k sampling
    'repetition_penalty 1.1  ; Penalty against repetitions
  })

  ; Generation
  (println "Generating...")
  (setq generated-tokens (gguf-generate model tokens gen-config))

  ; Convert to text
  (setq result (gguf-detokenize model generated-tokens))

  (println "\n--- Result ---")
  (println result)
  (println "\-----------\n")

  result)

; ========================================
; EXAMPLE 3: Interactive session
; ========================================

(defun chat-gpt-oss (model)
  (println "\n=== Interactive Chat with GPT-OSS-20B ===")
  (println "Type 'quit' or 'exit' to quit")
  (println "Type 'reset' to reset the cache")
  (println "")

  (loop
    (print "\nYou: ")
    (setq input (read-line))

    ; Special commands
    (cond
      ((or (= input "quit") (= input "exit"))
       (block
         (println "Goodbye!")
         (break)))

      ((= input "reset")
       (block
         (gguf-reset-cache model)
         (println "Cache reset")))

      (true
       (block
         ; Generate the response
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
; EXAMPLE 4: Testing creativity with different temperatures
; ========================================

(defun test-temperatures (model prompt)
  (println "\n=== Testing Different Temperatures ===")
  (println "Prompt:" prompt)
  (println "")

  (setq tokens (gguf-tokenize model prompt))
  (setq temperatures '(0.3 0.7 1.0 1.5))

  (maplist temp temperatures
    (block
      (println "--- Temperature:" temp "(")
      (cond
        ((< temp 0.5) (println "    conservative/deterministic)"))
        ((< temp 0.9) (println "    balanced)"))
        ((< temp 1.2) (println "    creative)"))
        (true (println "    very creative/random)"))))

      (setq result-tokens (gguf-generate model tokens {
        'max_new_tokens 50
        'temperature temp
        'top_p 0.9
      }))
      (setq result (gguf-detokenize model result-tokens))
      (println result)
      (println ""))))

; ========================================
; EXAMPLE 5: Code completion
; ========================================

(defun complete-code (model code-start)
  (println "\n=== Code Completion ===")
  (println "Starting code:")
  (println code-start)
  (println "")

  (setq tokens (gguf-tokenize model code-start))

  ; Configuration optimized for code
  (setq result-tokens (gguf-generate model tokens {
    'max_new_tokens 200
    'temperature 0.2       ; Low temperature for better accuracy
    'top_p 0.95
    'repetition_penalty 1.05
  }))

  (setq completion (gguf-detokenize model result-tokens))

  (println "--- Completed code ---")
  (println completion)
  (println "--------------------\n")

  completion)

; ========================================
; MAIN FUNCTION
; ========================================

(defun main ()
  (println "╔═══════════════════════════════════════╗")
  (println "║  GPT-OSS-20B GGUF Example            ║")
  (println "╚═══════════════════════════════════════╝\n")

  ; Load the model
  (setq my-model (load-gpt-oss))

  (if (not (null my-model))
      (block
        ; Example 1: Simple generation
        (generate-text my-model "Functional programming is")

        ; Example 2: Temperature testing
        (test-temperatures my-model "Once upon a time")

        ; Example 3: Code completion
        (complete-code my-model "def fibonacci(n):\n    \"\"\"Calculate the nth Fibonacci number\"\"\"\n    ")

        ; Example 4: Interactive mode (uncomment to use)
        ; (chat-gpt-oss my-model)

        (println "\n✓ All examples have been executed successfully!")
        (println "\nTo launch interactive chat mode, run:")
        (println "  (chat-gpt-oss my-model)")

        my-model)
      (println "Failed to load the model")))

; ========================================
; QUICK USAGE
; ========================================

(println "")
(println "Available commands:")
(println "  (main)                          - Run all examples")
(println "  (setq m (load-gpt-oss))         - Load the model only")
(println "  (generate-text m \"your text\")   - Generate text")
(println "  (chat-gpt-oss m)                - Interactive chat mode")
(println "  (test-temperatures m \"text\")    - Test creativity")
(println "  (complete-code m \"code\")        - Code completion")
(println "")
