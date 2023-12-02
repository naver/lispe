;Date: 02/12/2023
;Author: Claude Roux
;Description: Day2 2023 Advent of Code


(setq données (fread (+ _current "data/day2.txt")))

(setq données (split données "\n"))

(mapcar 
   (\(line)
      (@@ line ":" -)
   )
   données
)



