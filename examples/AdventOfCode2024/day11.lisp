;Date: 07/01/2025
;Author: Claude Roux
;Description: Advent of code 2024 day 11


(setq r (integers (split (fread (+ _current "data/day11.txt")) " ")))
      
(defmacro even(x) (zerop (% (size (string x)) 2)))

(println (size r))

(defun blink(l)
   (setq res (integers))
   (loop b l
      (cond
         ((eq b 0)
            (push res 1)
         )
         ((even b)
            (setq s (string b))
            (setq half (/ (size s) 2))
            (push res (integer (@@ s 0 half)))
            (push res (integer (@@ s half -)))
         )
         (true
            (push res (* b 2024))
         )
      )
      
   )
   res
)

(setq d (dictionaryi))

