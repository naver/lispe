;Date: 06/08/2022
;Author: Claude Roux
;Description: We add a end# when an indented structure ends


(defmacro espace(x) (size (takelist (\(c) (eq c " ")) x)))
(defmacro inc(i) (+= (@ i 0) 1))

(defun skip(code i v)
   (setq ligne (@ code (car i)))
   (check 
      (or
         (eq (count ligne "\"\"\"") 1)
         (eq (count ligne "'''") 1)
      )
      (push v ligne)
      (inc i)
      (setq ligne (@ code (car i)))
      (while 
         (and 
            (not (in ligne "\"\"\""))
            (not (in ligne "'''"))
         )
         (inc i)
         (push v ligne)
         (setq ligne (@ code (car i)))
      )
      (push v ligne)
      (inc i)
      (setq ligne (@ code (car i)))
   )
   (while (eq (trim ligne) "")
      (inc i)
      (setq ligne (@ code (car i)))
   )
   ligne
)

(defun insert_label (code i v)
   (check (< (car i) (size code))
      (setq ligne (skip code i v))
      (setq cpt (espace ligne))
      (push v ligne)
      (setq ref cpt)
      (inc i)
      (setq colon 0)
      (while (< (car i) (size code))
         (setq ligne (skip code i v))
         (if (eq (last (trim ligne)) ":")
            (+= colon 1)
         )
         (setq cpt (espace ligne))  
         (cond
            ((< cpt ref) 
               (break)
            )
            ((eq cpt ref)
               (inc i)
               (push v ligne)
            )
            (true               
               (insert_label code i v)
               (check colon
                  (push v (+ (join (to_list " " ref) "") "end#"))
                  (-= colon 1)
               )
            )
         )
      )
   )
)


(setq code (split (fread (+ _current "descent.py")) "\n"))

(setq v ())
(insert_label code '(0) v)

(loop r v
   (println r)
)

