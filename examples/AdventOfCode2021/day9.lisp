
;Date: 12/01/2022
;Author: Claude Roux
;Description: Risk Level

(setq heightmap (fread (+ _current "data/codes_day9.txt")))

(setq carte (maplist 'trim (split (trim heightmap) "\n")))
(setq carte (matrix (maplist (\(x) (integers (split x ""))) carte)))

(setq dmx (size carte))
(setq dmy (size (@ carte 0)))

(defun verif (x y)
   (setq mx (integers))
   (setq my (integers))

   (if (neq x 0)
      (push mx (- x 1))
   )
   (if (< x (- dmx 1))
      (push mx (+ x 1))
   )
   (if (neq y 0)
      (push my (- y 1))   
   )
   (if (< y (- dmy 1))
      (push my (+ y 1))   
   )

   (setq v (@ carte x y))
   (loop i mx
      (if (>= v (@ carte i y))
         (return)
      )
   )
   (loop j my
      (if (>= v (@ carte x j))
         (return)
      )
   )
   (return true)
)

(setq res (integers))
(setq pos ())
(loop x (irange 0 dmx 1)
   (loop y (irange 0 dmy 1)
      (check (verif x y)
         (push res (+ 1 (@ carte x y)))
         (push pos (integers x y))
      )
   )
)

(println 'part1 (sum res))

(defun trouve(x y valeurs)
   (check 
      (and
         (>= x 0) (< x dmx)
         (>= y 0) (< y dmy)
         (neq (@ carte x y) 9)
         (not (in valeurs (integers x y)))
      )
      (insert valeurs (integers x y))
      (trouve (- x 1) y valeurs)
      (trouve (+ x 1) y valeurs)
      (trouve x (- y 1) valeurs)
      (trouve x (+ y 1) valeurs)
   )
)

(setq v (integers))
(loop c pos
   (setq valeurs (set))
   (trouve (car c) (cadr c) valeurs)
   (push v (size valeurs))
)

(println 'part2 (* (extract (sort '> v) 0 3)))






