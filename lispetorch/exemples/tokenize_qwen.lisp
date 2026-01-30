(use 'lispe_tiktoken)

(class@ Tokenizer (path init)
    (defun configure()
        (setq spec_tokens (json_parse (fread (+ path "/tokenizer_config.json"))))
        (setq tok_file (json_parse (fread (+ path "/tokenizer.json"))))

        ; Récupérer les tokens spéciaux
        (setq pattern (@ tok_file "pre_tokenizer" "pretokenizers" 0 "pattern" "Regex"))

        ; Créer le tokenizer
        (setqi tokenizer (tiktoken_create
            (@ tok_file "model" "vocab")
            (@ tok_file "added_tokens")
            pattern))

        ; Récupérer les IDs des tokens spéciaux
        (setqi bos_id (tiktoken_special_encode tokenizer "<|im_start|>"))
        (setqi eos_id (tiktoken_special_encode tokenizer "<|im_end|>"))
        (setqi eot_id (tiktoken_special_encode tokenizer "<|endoftext|>"))
    )

    (defun eos()
        (integers eos_id eot_id)
    )
    
    (defun encode(prompt)
        (integers bos_id 
            (tiktoken_encode 
                tokenizer 
                (+ "user\n" prompt)) 
                eos_id 
                (tiktoken_encode tokenizer "\nassistant:")))
                
    (defun encode_raw(prompt)
        ; Encoder sans ajouter bos_id (pour format de chat déjà formaté)
        (tiktoken_encode tokenizer prompt)
    )

    (defun encode_text(text)
        ; Encoder du texte brut (exposer tiktoken_encode)
        (tiktoken_encode tokenizer text)
    )

    (defun decode(lst)
        (tiktoken_decode tokenizer lst)
    )

    (defun get_bos_id()
        bos_id
    )

    (defun get_eos_id()
        eos_id
    )

    (defun get_eot_id()
        eot_id
    )
)

