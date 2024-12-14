;Date: 14/12/2024
;Author: Claude Roux
;Description: Advent of Code 2024 day 2


(setq v (maplist (\(x) (integers (split x " "))) (split (fread (+ _current "data/day2.txt")) "\n")))

(filterlist
   (\(x) (and (not (in x 1000)) (eq (size x) (iabs (+ (maplist 'signp x))))))
   (maplist 
      (\(l) 
         (zipwith 
            (\(x y) 
               (setq d (- x y)) 
               (if 
                  (and 
                     d 
                     (< (iabs d) 4)
                  )
                  d
                  1000
               )
            )         
            (@@ l 0 -1) 
            (cdr l)
         )
      )
      v
   )
)

