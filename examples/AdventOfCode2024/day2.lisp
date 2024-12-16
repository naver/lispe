;Date: 14/12/2024
;Author: Claude Roux
;Description: Advent of Code 2024 day 2


(setq v (maplist (\(x) (integers (split x " "))) (split (fread (+ _current "data/day2.txt")) "\n")))

(defun verifie(l)
   (eq 
      (- (size l) 1) 
      (fabs (sum 
            (maplist 
               (\(x) (if (< (iabs x) 4) (signp x) 0))
               (shift '- l)
            )
         )
      )
   )
)

(defun compte(v) 
   (filterlist
      'verifie
      v
   )
)


(setq enigm1 (size (compte v)))
(println enigm1)

(setq nb 0)
(size (filterlist
      (\(l)
         (if (verifie l)
            (return true)
            (loop i (irange 0 (size l) 1)
               (check (verifie (pop (clone l) i))
                  (return true)
               )
            )
         )
      )
      v
   )
)



