; Test of the new GGUF library based on llama.cpp
; This version uses llama.cpp directly instead of PyTorch
; for better performance with MXFP4

(use 'lispe_gguf)

(println "=== GGUF Test with llama.cpp (v2) ===\n")

; MXFP4 model path
(setq model-path "/Users/user/.lmstudio/models/lmstudio-community/gpt-oss-20b-GGUF/gpt-oss-20b-MXFP4.gguf")

(println "File:" model-path)
(println "")

; Test 1: Model loading
(println "Test 1: Loading model...")
(setq model (gguf_load model-path 0 2048))

(if (nullp model)
    (println "❌ Loading failed\n")
    (block
        (println "✓ Model loaded successfully!")
        (println "")))

; Test 2: Tokenization
(if (not (nullp model))
    (block
        (println "Test 2: Tokenization...")
        (setq test-text "Hello, how are you?")
        (println "Text:" test-text)
        (setq tokens (gguf_tokenize model test-text))
        (println "Tokens:" tokens)
        (println "Number of tokens:" (size tokens))
        (println "")))

; Test 3: Detokenization
(if (not (nullp model))
    (block
        (println "Test 3: Detokenization...")
        (setq decoded (gguf_detokenize model tokens))
        (println "Reconstructed text:" decoded)
        (println "")))

; Test 4: Text generation
(if (not (nullp model))
    (block
        (println "Test 4: Text generation...")
        (println "Prompt: 'Once upon a time'")
        (println "")
        (setq generated (gguf_generate model "Once upon a time" 50 0.8 0.9 40))
        (println "")
        (println "Generated text:" generated)
        (println "")))

(println "=== Tests completed ===")
