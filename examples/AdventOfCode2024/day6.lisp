;Date: 18/12/2024
;Author: Claude Roux
;Description: Advent of code 2024 day 6

(setq enigma (fread (+ _current "data/day6.txt")))

(setq carte (split enigma "\n"))

(loop i (enum carte)
   (println (find (@ i 1) "^"))
)



