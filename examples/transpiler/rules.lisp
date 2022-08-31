;Date: 21/07/2022
;Author: Claude Roux
;Description: Règles

; This program modifies the tokenization rules to better tokenize a grammar

; We create a tokenizer object
(setq tok (tokenizer_rules))

; We extract the rules associated to this object 
(setq rg (get_tokenizer_rules tok))

; rg is a list of rules
; we modify the first rule, which extracts spaces, to skip these spaces
; the =# indicates that the characters detected by the rules should ne skipped
(set@ rg 0 " +=#")

; We insert a new rule (neu) before a given rule (base)
(defun remplace (base neu)
   (loop i (range 0 (size rg) 1)
      (setq x (@ rg i))
      (check (in x base)
         (insert rg neu i)
         (break)
      )
   )
)

; We create a rule so that a % followed with any characters is a token
; Note that % is also the escape character in this formalism, hence the %% in the rule
(remplace "%=0" "%%.=0")

;We also add a rule to concatenate a $ with any sequence of characters: $TEST
(remplace "$=0" "$~{%S %p %o}+=0")

(set_tokenizer_rules tok rg)






