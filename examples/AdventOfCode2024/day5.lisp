;Date: 18/12/2024
;Author: Claude Roux
;Description: Advent of code day 5


(setq enigma (fread (+ _current "data/day5.txt")))
(setq v (split enigma "\n"))

(setq règles (dictionaryi))
(setq interdits (dictionaryi)) 
(setq pages ())
(loop x v
   (ncheck (in x "|")
      (push pages (integers (split x ",")))
      (setq sub (integers (split x "|")))
      (maybe
         (push (@ règles (@ sub 0)) (@ sub 1))
         (set@ règles (@ sub 0) (list (@ sub 1)))
      )
      (maybe
         (push (@ interdits (@ sub 1)) (@ sub 0))
         (set@ interdits (@ sub 1) (integers (@ sub 0)))
      )
   )
)

(setq r (integers))
(setq désordre ())
(loop p pages
   (setq couples (zipwith (\(x y) (integers x y)) (@@ p 0 -1) (cdr p)))
   (setq error false)
   (loop c couples
      (check
         (and
            (key@ interdits (@ c 0))
            (in (@ interdits (@ c 0)) (@ c 1))
         )
         (setq error true)
         (push désordre p)
         (break)
      )
      (if error (break))
   )
   (check (nullp error)
      (push r (@ p (integer (/ (size p) 2))))
   )
)

(println (+ r))

(setq r (integers))
(loop p désordre
   (setq md true)
   (while md
      (setq couples (zipwith (\(x y) (integers x y)) (@@ p 0 -1) (cdr p)))
      (setq md false)
      (loop cl (enum couples)
         (setq c (@ cl 1))
         (check
            (and
               (key@ interdits (@ c 0))
               (in (@ interdits (@ c 0)) (@ c 1))
            )
            (swap p (@ cl 0))
            (setq md true)            
         )
      )
   )
   (push r (@ p (integer (/ (size p) 2))))
)

(println (+ r))





