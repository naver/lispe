; Interactive chat example using gguf_chat

(use 'lispe_gguf)

(println "=== GGUF Interactive Chat ===\n")

(setq model-path "/Users/clauderoux/.lmstudio/models/lmstudio-community/QwQ-32B-GGUF/QwQ-32B-Q4_K_M.gguf")

(println "Loading model:" model-path)
(println "")

; Load model with configuration
(setq model
   (gguf_load model-path
      {"n_ctx":4096
       "cache_type_k":"q4_0"
       "cache_type_v":"q4_0"
      }
   )
)

(check (nullp model)
    (println "ERROR: Model could not be loaded")
    (exit -1)
)

(println "Model loaded successfully!\n")

; Initialize messages with system prompt
(setq messages
   (list
      {"role": "system" "content" :"Tu es un assistant serviable et amical. Réponds de manière concise."}
   )
)

; Generation configuration
(setq config
   {"max_tokens" : 500
    "temperature" : 0.7
    "top_p" : 0.9
    "repeat_penalty" : 1.1
   }
)

; Callback to show tokens as they are generated
(defun on_token (token)
   (print (gguf_detokenize model (list token)))
)

; Add callback to config for streaming output
(setq config (key@ config "callback" 'on_token))

(println "Chat ready! Type 'quit' to exit.\n")
(println "-----------------------------------")

; Main chat loop
(while true
   ; Read user input
   (print "\nYou: ")
   (setq user-input (input@))
   ; Check for exit
   (check (== user-input "quit")
        (println "\nGoodbye!")
        (break)
   )
   
   ; Add user message to conversation
   (push messages {"role" : "user" "content" : user-input})
   
   ; Generate response (messages is updated with assistant response)
   (print "\nAssistant: ")
   (setq messages (gguf_chat model messages config))
   (println)
   
   ; Show last message (the assistant response)
   ; (println "\n[Debug] Last message:" (last messages))
)

; Clean up
(gguf_free model)
(println "\nModel freed.")
