;Date: 14/12/2024
;Author: Claude Roux
;Description: Advent of code 2024: Day 1

(setq v (split (trim (fread (+ _current "data/day1.txt"))) "\n"))
(setq v (rho 1000 2 (integers (flatten (maplist (\(x) (split x " ")) v)))))
(setq colon1 (sort '< (maplist (\(x) (@ x 0)) v)))
(setq colon2 (sort '< (maplist (\(x) (@ x 1)) v)))

(setq r (+ (zipwith (\(x y) (integer (fabs (- x y)))) colon1 colon2)))

(+ (maplist (\(x) (* x (count colon2 x))) colon1))











   
 




