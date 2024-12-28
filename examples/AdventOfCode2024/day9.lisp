;Date: 27/12/2024
;Author: Claude Roux
;Description: Advent of Code 2024 day 9


(setq codes (integers (split (trim (fread (+ _current "data/day9_example.txt"))) "")))

(setq base ())

(setq blocs (slice codes 2))

(loop b (enum blocs)
   (push base (replicate (@ b 1 0) (@ b 0)))
   (maybe
      (push base (replicate (@ b 1 1) "."))
      (setq déplie (flatten base))
   )
)

(setq nv (find déplie "."))
(setq dernier (- (size déplie) 1))

(while (> dernier nv)
   (swap déplie nv dernier)
   (setq nv (find déplie "." nv))
   (-= dernier 1)
)

(setq déplie (filterlist 'digitp déplie))

(setq somme 0)
(loop c (enum déplie)
   (+= somme (* (@ c 0) (@ c 1)))
)

(println somme)

(defun cherche (b nv)
   (while 
      (and 
         (< nv (size b)) 
         (nullp (in (@ b nv) "."))
      )
      (+= nv 1)
   )
)

(defmacro à(i) (size (@ base i)))

(setq base (filterlist '(neq _ ()) base))
(println base)
(setq sz (size base))
(setq nv -1)

(setq rb (reverse (clone base) true))
(loop dder (enum rb)
   (setq der (@ dder 1))
   (println 0 dder base)
   (loop dd (enum base)
      (setq d (@ dd 1))
      (check 
         (and 
            (in d ".")
            (<= (size der) (size d))
         )
         (pop base (- sz (@ dder 0)))
         (ncheck (!= (size der) (size d))
            (set@ base (@ dd 0) der)
            (set@ base (@ dd 0) (@@ d 0 (- (size d) (size der))))
            (insert base der (@ dd 0))
         )
         (setq bb (list (car base)))
         (setq base bb)
         (println 1 der base)
         (break)
      )
   )
   (println 2 der base)
)







