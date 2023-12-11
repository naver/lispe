;Date: 11/12/2023
;Author: Claude Roux
;Description: Day 6 of Advent of code 2023


(setq dons (fread (+ _current "data/day6.txt")))

(setq données (maplist (\(x) (integers (@@ x 1 -))) (maplist '(split _ " ") (split dons "\n"))))

(product
   (maplist 
      (\(d)
         (setq x (@ d 0))
         (setq dst (@ d 1))
         (size 
            (filterlist '(<= dst)
               (maplist
                  (\(y)
                     (* y (- x y))
                  )
                  (range 0 x 1)
               )
            )
         )
      )
      (zip (@ données 0) (@ données 1))
   )
)

(setq données (maplist (\(x) (integer . join (@@ x 1 -) "")) (maplist '(split _ " ") (split dons "\n"))))

(setq tmps (@ données 0))
(setq dst (@ données 1))

; We look for the first value to break the record
(loop x (irange 0 tmps 1)
   (if (>= (* x (- tmps x)) dst)
      (break)
   )
)

; since values are symetrical, we can now compute our final value
(setq res (• 1 + tmps - (* 2  x)))
(println res)

