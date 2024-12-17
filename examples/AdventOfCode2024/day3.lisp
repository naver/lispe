;Date: 17/12/2024
;Author: Claude Roux
;Description: Advent of Code 2024 day 3




(setq code (fread (+ _current "data/day3.txt")))

(setq instructions (rgx_findall (rgx "mul%(%d+,%d+%)") code))

(maplist (apply '* (\(x) (integers (rgx_findall (rgx "%d+") x)))) instructions)


