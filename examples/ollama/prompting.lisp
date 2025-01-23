;Date: 03/12/2024
;Author: Claude Roux
;Description: Prompting


(load (+ _current "ollama.lisp"))

(setq system `You are provided a problem statement that requires you to reason over it and provide the correct solution. Please provide good reasons for your answer. `)

(setq prompt `Here is the problem statement - {context} The answer should be as concise as possible. Please provide your final answer now. `)

(setq problem `Sara and Joe have a combined height of 120 inches. Joe is 6 inches more than double Sara's height. How tall is Joe?`)

; We initialize our server address, by default it is http://localhost:11434
(setprofile "http://192.168.1.125:11434")

(setq msg (tchat () (replace prompt "{context}" problem) system))
(formatcontent (@ msg -1 "content"))

