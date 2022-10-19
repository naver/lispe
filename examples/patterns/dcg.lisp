;Date: 03/06/2021
;Author: Claude Roux
;Description: CFG

(setq current_poss 
   `S : NP VP
   PP : PREP NNP
   NP : DET NOUN PP
   NP : DET ADJ NOUN
   NP : DET NOUN
   NNP : DET NLOC
   NNP : DET ADJ NLOC
   VP : VERB NP
   DET : "the"  "a" "this" "that"
   NOUN : "cat"  "dog"  "bat" "man" "woman" "child" "puppy"
   NLOC : "house" "barn" "flat" "city" "country"
   VERB : "eats"  "chases"  "bites" "sees"
   PREP : "of" "from" "in"
   ADJ : "big" "small" "blue" "red" "yellow" "petite"
   `
)


(defpat add ( ["\"" m "\"" $ r] n d)
   (push (@ d n) m)
   (add r n d)
)

(defpat add ( _ _ _) _)

; Note that for each terminal POS, we generate a: POS : %POS rule
; This POS:%POS is a dictionary entry where the value is NOT a list
; The %POS is the key to word lists in the grammar
(defpat build( [n ":" "\"" m "\"" $ r] d)
   (set@ d n (+ "%" n))
   (setq n (+ "%" n))
   (set@ d n (list m))
   (add r n d)
)

(defpat build( [n ":" $ r] d)
   (if (key d n)
      (push (@ d n) r)
      (set@ d n (list r))
   )
)

(defun compile(r)
   (setq s (maplist 'tokenize (maplist 'trim (split (trim current_poss) "\n"))))
   (setq d (dictionary))
   (maplist (\(x) (build x d)) s)
   d
)

(setq grammar (compile current_poss))

(defpat terminal( [POS $ _])
   (eq (@ POS 0) "%")
)

; Generation of a word
; First we check is the first POS in current_pos is a terminal POS
; A terminal POS starts with "%"
; Note that we randomly select a potential word from within the grammar
(defpat generate( [terminal current_pos] tree)
   (setq rg (random_choice 1 (key grammar (car current_pos)) 10))
   ; we proceed with the rest of the current_pos elements
   (generate (cdr current_pos) (nconc tree rg))
)

; Generation from a POS
; We go down our list to find a terminal POS (starting with a "%")
(defpat generate([POS $ current_pos] tree)
   (setq r (key grammar POS))
   (if (consp r)
      (generate (nconcn (random_choice 1 r 10) current_pos) tree)
      (generate (nconc (list r) current_pos) tree)
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
   (generate current_pos (cons "*" (reverse consume)))
)

; If the first element in current_pos is a terminal element
; then we check if the word exists in the grammar
(defpat match ( [terminal current_pos]  [w $ sentence]  consume)
   (setq rg (key grammar (car current_pos)))
   ;If the word exists in the grammar
   ;We continue to analyse of sequence of POS
   ;Else we simply stop
   (if (in rg w)
      (match (cdr current_pos) sentence  (cons w consume))
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
;   (println 'Tree sentence)
   (@@ sentence - "*" -1)
)

; we start with an incomplete sequence of words
; we start with an incomplete sequence of words
(loopcount 50
   (println (join (analyse "a cat") " "))
)






