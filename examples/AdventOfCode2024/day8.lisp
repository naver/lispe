;Date: 25/12/2024
;Author: Claude Roux
;Description: Advent of Code 2024 day 8

(setq carte (fread (+ _current "data/day8.txt")))

(setq carte (maplist (\(x) (split x "")) (split carte "\n")))

(setq codes (filterlist '(!= ".") (unique (flatten carte))))

(setq sz (size carte))

(setq réponses (rho sz sz '(".")))

(setq positions (dictionary))
(loop ligne (enum carte)
   (loop c codes
      (setq p (find (@ ligne 1) c))
      (check p
         (setq e (list (@ ligne 0) p))
         (if (key positions c)
            (insert (@ positions c) e)
            (set@ positions c (heap (\(x y) (>=< (@ x 0) (@ y 0))) e))
         )
      )
   )
)


(shift 'print (to_list (@ positions "3")))

