;Date: 20/10/2022
;Author: Claude Roux
;Description: Apply rules


(defmacro islexicalrule (POS)
   (eq "%" (@ POS 0))
)


; Generation from a POS
; We go down our list to find a terminal POS (starting with a "%")
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

; If the first element in current_pos is a terminal element
; then we check if the word exists in the grammar
(defpat match ( [ (islexicalrule POS) $ current_pos]  [w $ sentence]  consume)
   ;If the word exists in the grammar
   ;We continue to analyse our sequence of POS
   ;Else we simply stop
   (if (in (key grammar POS) w)
      (match current_pos sentence  (nconcn consume w))
   )
)

; We check the first POS from current_pos
; We check if it is a lexicon rule (r is not a list then)
; Otherwise, we take the rule and replace the current POS
; with its rule description.
(defpat match ( [POS $ current_pos] s consume)
   (setq R (key grammar POS))
   (ncheck (consp R)
      (match (nconcn (list R) current_pos) s consume)
      (loop r R
         (setq  poslst (match (nconcn r current_pos) s consume))
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




