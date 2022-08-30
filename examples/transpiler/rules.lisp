;Date: 21/07/2022
;Author: Claude Roux
;Description: Règles


(setq tok (tokenizer_rules))

(setq rg (get_tokenizer_rules tok))
(set@ rg 0 " +=#")

(defun remplace (base neu)
   (loop i (range 0 (size rg) 1)
      (setq x (@ rg i))
      (check (in x base)
         (insert rg neu i)
         (break)
      )
   )
)


(remplace ":=" ":==0")
(remplace "%=0" "%%.=0")
(remplace "$=0" "$~{%S %p %o}+=0")
(remplace ";=0" ";%d+=0")

(set_tokenizer_rules tok rg)





