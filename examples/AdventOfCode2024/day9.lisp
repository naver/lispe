;Date: 27/12/2024
;Author: Claude Roux
;Description: Advent of Code 2024 day 9


(setq codes (integers (split (trim (fread (+ _current "data/day9.txt"))) "")))

(setq base ())

(setq blocs (slice codes 2))

(setq lst (last blocs))
(pop blocs)
(push blocs (list (car lst) 0))

(loop b (enum blocs)
   (push base (replicate (@ b 1 0) (@ b 0)))
   (push base (replicate (@ b 1 1) "."))
)

(setq déplie (flatten base))

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

(setq i 0)
(loop b blocs
   (push (@ blocs i) (replicate (@ b 0) i))
   (+= i 1)
)

(setq sz (size blocs))
(loop i (irange (- sz 1) -1 -1)
   (setq val (@ blocs i 2))
   (setq szval (size val))
   (loop b (enum blocs)
      (check 
         (and
            (< (@ b 0) i)
            (>= (@ b 1 1) szval)
         )
         (-= (@ blocs (@ b 0) 1) szval)
         (push (@ blocs (@ b 0)) val)
         (set@ blocs i 2 ())
         (if (> (size (@ blocs i)) 2)
            (+= (@ blocs (- i 1) 1) szval)
            (+= (@ blocs i 1) szval)
         )
         (break)
      )
   )
)

; 00...111...2...333.44.5555.6666.777.888899
; 00992111777.44.333....5555.6666.....8888..

(defun fusionne(blocs)
   (setq l ())
   (loop b blocs
      (nconc l (cddr b))
      (nconc l (replicate (@ b 1) "."))
   )
   (flatten l)
)


(setq somme 0)
(loop c (enum (fusionne blocs))
   (+= somme (* (@ c 0) (@ c 1)))
)

