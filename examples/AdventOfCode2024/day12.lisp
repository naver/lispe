;Date: 10/01/2025
;Author: Claude Roux
;Description: Advent of Code 2024 day 12


(setq m . maplist '(split _ "") . split (fread (+ _current "data/day12_example.txt")) "\n")

(setq sz . size m)

(setq veg . unique . flatten m)

(setq dico (dictionary))

(loop e veg
   (setq l 
      (heap '(\(x y) 
            (select (>=< (@ x 0) (@ y 0)) (>=< (@ x 1) (@ y 1)))
         )
      )
   )
   (loop c (enum m)
      (setq r (findall (@ c 1) e))
      (check r
         (setq o (zipwith 'list (replicate (size r) (@c 0)) r))
         (loop x o
            (insert l x)
         )
      )
   )
   (set@ dico e l)
)


