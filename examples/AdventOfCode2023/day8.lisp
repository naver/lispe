;Date: 14/12/2023
;Author: Claude Roux
;Description: Day 8


(setq don (fread (+ _current "/data/day8.txt")))


(setq données (split don "\n"))

(setq actions (car données))

(setq plan (tree))
(setq apath ())
(loop c (cdr données)
   (setq x (rgx_findall (rgx "%C+") c))
   (set@ plan (car x) {"L":(@ x 1) "R":(@ x 2)})
   (if (== (last . car x) "A")
      (push apath (car x))
   )
)

(setq nb 0)
(setq etat "AAA")
(while (!= etat "ZZZ")
   (loop a actions
      (setq etat (@ plan etat a))
      (+= nb 1)
      (if (== etat "ZZZ")
         (break)
      )
   )
)

(println nb)

(setq res ())
(setq limits (integers))
; The first value is the one with which the circuit repeats...
(loop etat apath
   (setq nb 0)
   (while (!= (last etat) "Z")
      (loop a actions
         (setq etat (@ plan etat a))
         (+= nb 1)
         (check  (== (last etat) "Z")
            (push limits nb)
            (break)
         )
      )
   )
)


;We then extract the common dividers of all these values
(setq 
   ml
   (maplist 
      (\(v)
         (filterlist (\(x) (zerop . % v x)) (iota . integer (/ v 2)))
      )
      limits
   )
)

; We then multiply these values together: (1 47 293 71 79 67 59 61)
(* . unique . flatten ml)


