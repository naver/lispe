; Test with standard Q8_0 model

(use 'lispe_gguf)

(println "=== GGUF Test with Qwen2-Math Q8_0 ===\n")

(setq model-path "/Users/clauderoux/.lmstudio/models/lmstudio-community/Qwen2-Math-1.5B-Instruct-GGUF/Qwen2-Math-1.5B-Instruct-Q8_0.gguf")

(println "File:" model-path)
(println "")


(println "Test 1: Loading model...")
; Configuration: uses GPU by default (n_gpu_layers=99)
; For CPU only, use: {"n_gpu_layers":0}
(setq model
   (gguf_load model-path
      {"n_ctx":4096
         "cache_type_k":"q8_0"
         "cache_type_v":"q8_0"
      }
   )
)

; Callback function called for each generated token
(defun on_token (token)
   ;(printerr (gguf_detokenize model (list token)))
   (printerr ".")
)

; 2. Generate text only if model is loaded
(ncheck (not (nullp model))
   (println "ERROR: Model could not be loaded")
   (println "Generating text...")
   (setq prompt "Hello, can you explain what functional programming is?")

   ; Direct generation with text prompt
   (println "\nPrompt:" prompt)
   (println "\nResponse:")
   (setq result (gguf_generate model prompt {"max_tokens":2000 "temperature":0.8 "repeat_penalty":1.2 "repeat_last_n":128}))
   (printerrln)
   (printerrln "-----------------------------------")
   (printerrln (gguf_detokenize model result)))

