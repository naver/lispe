;Date: 10/01/2025
;Author: Claude Roux
;Description: Advent of Code 2024 day 12


(setq m . maplist '(split _ "") . split (string (fread (+ _current "data/day12.txt"))) "\n")

(setq sz . size m)

(setq veg . unique . flatten m)

(setq carte (maplist 'clone m))

(defun compte(m a c)
   (setq nb 0)
   (setq (x y) a)
   (setq (xm ym xp yp) (list (- x 1) (- y 1) (+ x 1) (+ y 1)))

   (if (zerop x)
      (+= nb 1)
      (if (!= (@ m xm y)  c)
         (+= nb 1)
      )
   )
   (if (zerop y)
      (+= nb 1)
      (if (!= (@ m x ym)  c)
         (+= nb 1)
      )
   )

   (if (= xp sz)
      (+= nb 1)
      (if (!= (@ m xp y)  c)
         (+= nb 1)
      )
   )
   (if (= yp sz)
      (+= nb 1)
      (if (!= (@ m x yp)  c)
         (+= nb 1)
      )
   )

   nb
)

(defun comptesides(m region)
   (setq sides 0)
   (loop coord region
      (setq (x y) coord)
      (setq (xm ym xp yp) (list (- x 1) (- y 1) (+ x 1) (+ y 1)))

      ; Check left side
      (if (or (zerop x) (!= (@ m xm y) (@ m x y)))
         (+= sides 1)
      )
      ; Check right side
      (if (or (= xp sz) (!= (@ m xp y) (@ m x y)))
         (+= sides 1)
      )
      ; Check top side
      (if (or (zerop y) (!= (@ m x ym) (@ m x y)))
         (+= sides 1)
      )
      ; Check bottom side
      (if (or (= yp sz) (!= (@ m x yp) (@ m x y)))
         (+= sides 1)
      )
   )
   ; Divide by 2 since each internal fence is counted twice
   (/ sides 2)
)

(defun marque (il ic c ma)
   (setq sub ())
   (check 
      (and
         (>= il 0)
         (>= ic 0)
         (< il sz)
         (< ic sz)
         (= (@ m il ic) c)
      )
      (push sub (integers il ic))
      (set@ m il ic (+ c ma))
      (nconc sub (marque il (- ic 1) c 0))
      (nconc sub (marque (- il 1) ic c 0))
      (nconc sub (marque il (+ ic 1) c 0))
      (nconc sub (marque (+ il 1) ic c 0))
   )
   sub
)

(setq dico (dictionary))

(loop c veg
   (setq total ())
   (setq total2 ())
   (loop il (irange 0 sz 1)
      (loop ic (irange 0 sz 1)
         (check (= (@ m il ic) c)
            (setq ligne (marque il ic c 1))
            (push total ligne)
            (setq sub ())
         )
      )
   )
   (set@ dico c total)
)



(setq total 0)
(loop c veg
   (loop i (@ dico c)
      (setq nb 0)
      (loop e i
         (+= nb (compte carte e c))
      )
      (+= total (* (size i) nb))
   )   
)

(println total)
