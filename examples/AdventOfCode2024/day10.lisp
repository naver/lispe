;Date: 07/01/2025
;Author: Claude Roux
;Description: Advent of code 2024 day 10


(setq carte (maplist (\(x) (integers (split x ""))) (split (fread (+ _current "data/day10.txt")) "\n")))

(setq xcarte (size carte))
(setq ycarte (size (@ carte 0)))


(defun choisir(xx yy c)
   (setq l ())
   (loop i '(-1 1)
      (setq x (+ xx  i))
      (setq y (+ yy i))

      (check
         (and
            (< x xcarte)
            (>= x 0)
         )
         (if (eq (@ carte x yy) c)
            (push l (integers x yy))
         )
      )
      (check
         (and
            (< y ycarte)
            (>= y 0)
         )
         (if (eq (@ carte xx y) c)
            (push l (integers xx y))
         )
      )
   )
   l
)

(setq z ())
(loop i (enum carte)
   (nconc z (maplist (\(x) (integers (@ i 0) x)) (findall (@ i 1) 0)))
)

(defun parcours(e v p)
   (ife (eq v 10)
      (push p e)
      (setq l (choisir (@ e 0) (@ e 1) v))
      (loop n l
         (parcours n (+ v 1) p)
      )
   )
)


(setq first 0)
(setq second 0)
(loop e z
   (setq p ())
   (parcours e 1 p)
   (+= first (size (unique p)))
   (+= second (size p))
)

(println first second)

