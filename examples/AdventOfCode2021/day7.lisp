
;Date: 07/01/2022
;Author: Claude Roux
;Description: crabes



(setq r (fread (+ _current "data/codes_day7.txt")))

(setq crabes (integers (split r ",")))

(setq m (minmax crabes))

(setq consos (dictionaryn))
(loop p (irange (car m) (+ 1 (cadr m)) 1)
   (set@ consos p 0)
)

(setq res1 (integers))
(loop c (irange 0 (size crabes) 1)
   (loop cc (irange 0 (size crabes) 1)
      (check (neq c cc)
         (+= (@ consos cc) (fabs (- (@ crabes c) (@ crabes cc))))
      )
   )
)

(println (min (filter '(neq 0) (values@ consos))))

(setq consos2 (dictionaryn))
(loop p (irange (car m) (+ 1 (cadr m)) 1)
   (set@ consos2 p (sum (iota p)))
)


(setq res2 (integers))
(loop p (irange (car m) (+ 1 (cadr m)) 1)
   (push res2 (sum (maplist (\(x) (@ consos2 (fabs (- x p)))) crabes)))
)

(setq mini (min res2))
(println mini " position: " (find res2 mini))




