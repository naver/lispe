;Date: 17/12/2024
;Author: Claude Roux
;Description: Advent of code 2024 day 4


(setq message (fread (+ _current "data/day4.txt")))

(setq message (split message "\n"))
(setq lignes (size message))

(setq nb (+ (maplist (\(x) (+ (count x "XMAS") (count x "SAMX"))) message)))
(setq m (transpose (maplist (\(x) (split x "")) message)))

(setq m (maplist (\(x) (join x "")) m))
(println nb)
(+ nb (maplist (\(x) (+ (count x "XMAS") (count x "SAMX"))) m))












