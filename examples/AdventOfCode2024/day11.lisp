;Date: 07/01/2025
;Author: Claude Roux
;Description: Advent of code 2024 day 11

(setq d (dictionaryi))
(maplist (\(x) (set@ d x 1)) (integers (split (fread (+ _current "data/day11.txt")) " ")))

(defmacro even(x) (zerop (% (size (string x)) 2)))

(defun blink(l)
   (setq res (dictionaryi))
   (loop b l
      (setq nb (@ l b))
      (cond
         ((zerop b)
            (+= (@ res 1) nb)
         )
         ((even b)
            (setq s (string b))
            (setq half (/ (size s) 2))
            (setq left (integer (@@ s 0 half)))
            (setq right (integer (@@ s half -)))
            
            (+= (@ res left) nb)
            (+= (@ res right) nb)
         )
         (true
            (+= (@ res (* b 2024)) nb)
         )
      )
   )
   res
)

(loopcount 25
   (setq d (blink d))
)
(println (+ (values@ d)))

(loopcount 50
   (setq d (blink d))
)

(println (+ (values@ d)))

