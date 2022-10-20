;Date: 20/10/2022
;Author: Claude Roux
;Description: Compiler


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
   (setq s (maplist 'tokenize (maplist 'trim (split (trim r) "\n"))))
   (setq d (dictionary))
   (maplist (\(x) (build x d)) s)
   (setg grammar d)
)





