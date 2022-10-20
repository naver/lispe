;Date: 20/10/2022
;Author: Claude Roux
;Description: Apply rules


; Generation from a POS
; either it is a rule or it its lexical rule
; lexical rules are not LIST
(defpat generate([POS $ current_pos] tree)
   (setq r (key grammar POS))
   (if (consp r)
      (generate (nconcn (random_choice 1 r 30) current_pos) tree)
      (generate current_pos (nconc tree (random_choice 1 (key grammar r) 30)))
   )
)  

; All has been generated
(defpat generate ([] tree) (nconc tree "%%") )

; We have consumed all elements from current_pos
(defpat match ([] [] consume) 
   (nconcn consume "$$") 
)

; we check if the word list is empty
; We then generate our words by a random selection out of the grammar
; according to the available rules.
(defpat match ( current_pos  []  consume)   
   ; this symbol "*" indicates where the word list starts in the final tree
   (generate current_pos consume)
)

; We check the first POS from current_pos
; We check if it is a lexicon rule (r is not a list then)
; Otherwise, we take the rule and replace the current POS
; with its rule description.
(defpat match ( [POS $ current_pos] [w $ sentence] consume)
   (setq R (key grammar POS))
   (ncheck (consp R)
      (if (in (key grammar R) w)
         (match current_pos sentence (nconcn consume w))
      )
      (loop r R
         (setq  poslst (match (nconcn r current_pos) (cons w sentence) consume))
         (if poslst
            (return poslst)
         )
      )
   )
)

; POS is the first rule we start our analysis with
(defun parse (s POS tree) 
   (setq r (key grammar POS))
   (match (car r) s ())
)

(defun analyse(sentence)
   (setq sentence (parse (tokenize sentence) "S" ()))
   (@@ sentence 0 -1)
)

