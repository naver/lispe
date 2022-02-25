;Date: 05/01/2022
;Author: Claude Roux
;Description: Hydrothermal


(setq lignes (fread (+ _current "data/codes_day5.txt")))
  
(setq coords
   (maplist
      (\(x) (list (integers (split (@ x 0) ",")) (integers (split (@ x 1) ","))))
      (maplist
         (\(x) (split (trim x) " -> "))
         (split (trim lignes) "\n")
      )
   )
)

(setq c_max (+ 1 (max (flatten coords))))

(setq plan (rho c_max c_max (floats 0)))

(maplist
   (\(e)
      (cond
         ( (eq (@ e 0 0) (@ e 1 0))
            (setq x (@ e 0 0))
            (setq vy (minmax (@ e 0 1) (@ e 1 1)))
            (loop y (irange (@ vy 0) (+ 1 (@ vy 1)) 1)
               (set@ plan y x (+ 1 (@ plan y x)))
            )
         )
         ( (eq (@ e 0 1) (@ e 1 1))
            (setq vx (minmax (@ e 0 0) (@ e 1 0)))
            (setq y (@ e 0 1))
            (loop x (irange (@ vx 0) (+ 1 (@ vx 1)) 1)
               (set@ plan y x (+ 1 (@ plan y x)))
            )
         )
      )
   )
   coords
)

(println 'part1 (size (filterlist (\(x) (>= x 2)) (flatten plan))))

(setq plan (rho c_max c_max (floats 0)))
(maplist
   (\(e)
      (cond
         ( (eq (@ e 0 0) (@ e 1 0))
            (setq x (@ e 0 0))
            (setq vy (minmax (@ e 0 1) (@ e 1 1)))
            (loop y (irange (@ vy 0) (+ 1 (@ vy 1)) 1)
               (set@ plan y x (+ 1 (@ plan y x)))
            )
         )
         ( (eq (@ e 0 1) (@ e 1 1))
            (setq vx (minmax (@ e 0 0) (@ e 1 0)))
            (setq y (@ e 0 1))
            (loop x (irange (@ vx 0) (+ 1 (@ vx 1)) 1)
               (set@ plan y x (+ 1 (@ plan y x)))
            )
         )
         (true
            (setq ix (if (< (@ e 0 0) (@ e 1 0)) 1 -1))
            (setq iy (if (< (@ e 0 1) (@ e 1 1)) 1 -1))
            (setq x (@ e 0 0))
            (setq y (@ e 0 1))
            (setq mx (@ e 1 0))
            (setq my (@ e 1 1))
            (while (and (!= x mx) (!= y my))
               (set@ plan y x (+ 1 (@ plan y x)))
               (+= x ix)
               (+= y iy)
            )
            (set@ plan y x (+ 1 (@ plan y x)))
         )
      )
   )
   coords
)

(println 'part2 (size (filterlist (\(x) (>= x 2)) (flatten plan))))



