;Date: 26/11/2024
;Author: Claude Roux
;Description: Ollama connection via cURL


(use 'lispe_curl)
(load (+ _current "formatting.lisp"))

;--------------------------------------------------------------------------------------------------------------------------------------------
; Set our parameters straight
(setq uri "http://localhost:11434")
(setq modele "mistral-small")
(setq maxtokens 1000)
(setq temperature 0.7)

; we use setg (set global) to initialize global variables
(defun setprofile(ur_i (mode_l nil) (maxtoke_n nil) (temperatur_e nil))
   (if ur_i   (setg uri ur_i))
   (if mode_l (setg modele mode_l))
   (if maxtoke_n (setg maxtokens maxtoke_n))
   (if temperatur_e (setg temperature temperatur_e))
)

;--------------------------------------------------------------------------------------------------------------------------------------------

(setq cURL (curl))

;You need to call chat_first in order to initialize your chat completion
(defun chat_first (uri model system_prompt prompt num_ctx temperature)
   (setq results ())
   ;(curl_set_function cURL 'appel results)

   (+= uri "/api/chat")

   (setq msg (prepare prompt))

   (if system_prompt
      (block
         (setq system_prompt (prepare system_prompt))
         (setq messages (list {"role": "system" "content": system_prompt}  {"role": "user" "content": msg}))
      )
      (setq messages (list {"role": "user" "content": msg}))
   )

   (setq m {"model": model "messages": messages "options": {"temperature":temperature "num_ctx": num_ctx} "stream":false})
   (setq m (json m))
   (curl_url cURL uri)
   (curl_options cURL "CURLOPT_POSTFIELDS" m)
   (setq res (json_parse (curl_execute cURL)))
   (push messages {"role":"assistant" "content":(prepare (@ res "message" "content"))})   
)

(defun chat_next (uri model messages prompt  num_ctx temperature)
   (+= uri "/api/chat")

   (push messages {"role": "user" "content": (prepare prompt}))
   
   (setq m {"model": model "messages": messages "options": {"temperature":temperature "num_ctx": num_ctx} "stream":false})
   (curl_url cURL uri)
   (curl_options cURL "CURLOPT_POSTFIELDS" (json m))
   (setq res (json_parse (curl_execute cURL)))
   (push messages {"role":"assistant" "content":(prepare (@ res "message" "content"))})   
)


(defun tchat(messages prompt (system ""))
   (if messages
      (chat_next uri modele messages prompt maxtokens temperature)      
      (chat_first uri modele system prompt maxtokens temperature)
   )
)



