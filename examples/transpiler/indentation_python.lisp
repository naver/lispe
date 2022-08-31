;Date: 06/08/2022
;Author: Claude Roux
;Description: Indentation Python

(setq code (split (fread (+ _current "descent.py")) "\n"))

(defmacro espace(x) (size (takelist (\(c) (eq c " ")) x)))
(defmacro inc(i) (+= (@ i 0) 1))

(defun skip(i v)
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

(defun parcours (i v)
   (check (< (car i) (size code))
      (setq ligne (skip i v))
      (setq cpt (espace ligne))
      (push v ligne)
      (setq ref cpt)
      (inc i)
      (setq deuxpoints 0)
      (while (< (car i) (size code))
         (setq ligne (skip i v))
         (if (eq (last (trim ligne)) ":")
            (+= deuxpoints 1)
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
               (parcours i v)
               (check deuxpoints
                  (push v (+ (join (to_list " " ref) "") "end#"))
                  (-= deuxpoints 1)
               )
            )
         )
      )
   )
)

(setq v ())
(setq i '(0))
(parcours i v)

(loop r v
   (println r)
)
(println i)

