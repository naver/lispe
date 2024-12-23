;Date: 22/12/2024
;Author: Claude Roux
;Description: Advent of code 2024 day 7

(setq données (fread (+ _current "data/day7_example.txt")))

(setq données (split données "\n"))

(setq données (maplist (\(x) (split x ":")) données))

(setq v 0)
(loop equation données
   (setq valeur (integer (car equation)))
   (setq arguments (to_list (integers (split (trim (cadr equation)) " "))))
   (if (= valeur (* arguments))
      (+= v valeur)
      (if (= valeur (+ arguments))
         (+= v valeur)
         (loop i (irange 1 (size arguments) 1)
            (setq l1 (@@ arguments i 0))
            (setq l2 (@@ arguments 0 i))
            (setq eq1 (cons '+ (cons (cons '* l1) l2)))
            (println l2)
         )
      )
   )
)
(println v)


