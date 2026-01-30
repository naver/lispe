; Quick Start - Fast usage of GGUF models
; For beginners - simplest example possible

(use 'lispe_gguf)

; 1. Load model (with GPU by default)
(println "Loading model...")
(setq model (gguf_load
      "/Users/user/.lmstudio/models/lmstudio-community/gpt-oss-20b-GGUF/gpt-oss-20b-MXFP4.gguf"))

; 2. Generate text only if model is loaded
(ncheck (not (nullp model))
   (println "ERROR: Model could not be loaded")
   (println "Generating text...")
   (setq prompt "Hello, can you explain what functional programming is?")

   ; Direct generation with text prompt
   (println "\nPrompt:" prompt)
   (println "\nResponse:")
   (setq result (gguf_generate model prompt {"max_tokens":500 "temperature":0.8}))
   (println (gguf_detokenize model result)))

