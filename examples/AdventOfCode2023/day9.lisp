;Date: 16/12/2023
;Author: Claude Roux
;Description: Day 9 of Advent of Code 2023

(setq dons `0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
`
)

(setq dons (fread (+ _current "data/day9.txt")))

(setq values (maplist (\(x) (integers . split x " ")) (split dons "\n")))

(defun decrement(v)
   (if  (filterlist '(!= 0) v)
      (+ (last v) (decrement (- (cdr v) v)))
      0
   )
)


(setq r (maplist 'decrement values))
(println (sum r));1877825184

(setq r (maplist (\(x) (decrement . reverse x true)) values))
(println (sum r)); 1108

