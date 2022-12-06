;Date: 20/10/2022
;Author: Claude Roux
;Description: Compiler


; We push the different words, without their quotes, into the grammar
(defpat add ( ["\"" wrd "\"" $ words] POS gram)
   (push (@ gram POS) wrd)
   (add words POS gram)
)

; Final operation, the word list is empty
(defpat add ([] _ _) _)

; We use a pattern to detect lexical rules, in which each word is between quotes 
; Note that for each terminal POS, we generate a: POS : %POS rule
; This POS:%POS is a dictionary entry where the value is NOT a list
; The %POS is the key to word lists in the grammar
(defpat build( [POS ":" "\"" wrd "\"" $ words] gram)
   ; A double entry in the grammar.
   (setq lexPOS (+ "%" POS))
   ; POS points to %POS
   (set@ gram POS lexPOS)
   ; %POS points to the list of words
   (set@ gram lexPOS (list wrd)) ; this is an entry in the grammar
   (add words lexPOS gram)
)

; a rule pattern is: POS : POS1 POS2...POSN
(defpat build( [POS ":" $ rest] gram)
   (if (key gram POS)
      (push (@ gram POS) rest)
      (set@ gram POS (list rest))
   )
)

(defun compile(therules)
   (setq rules (maplist 'segment (maplist 'trim (split (trim therules) "\n"))))
   (println rules)
   (setg grammar (dictionary))
   (maplist (\(x) (build x grammar)) rules)
)


