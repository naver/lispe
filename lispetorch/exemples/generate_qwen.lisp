(load (+ _current "tokenize_qwen.lisp"))
(load (+ _current "model_qwen.lisp"))

(setq model_path (+ _current "qwen3/model"))
(setq tok_path (+ _current "qwen3/tokenizer"))
(setq prompt `Problem: A lady goes to a hotel for the weekend and takes a room for 3 nights.
She arrives at the hotel in the evening and decides to upgrade to a suite for an additional 20€ per night.
She also orders room service for dinner on the first night, which costs 30€.
The next day, she goes to the hotel spa and gets a massage for 60€.
On the last day, she checks out of the hotel early and doesn't take a breakfast.
A normal night in the hotel is 50€, a breakfast is 10€, and there is a tax of 10% on all charges.
`)

(println (fill "=" 60))
(println "CHARGEMENT DU MODELE")
(println (fill "=" 60))

; Charger le modèle
(println "\n[1/4] Chargement du modèle...")
(setq tok (Tokenizer tok_path (configure)))
(setq config (dictionary "device" "mps"))
(setq model (Model model_path config tok (configure)))

(println (fill "=" 60))
(println "GENERATION")
(println (fill "=" 60))

(println "Test 1: prompt original")
(model (generate "Gives me five capitals of five different European countrie?" 5000))




