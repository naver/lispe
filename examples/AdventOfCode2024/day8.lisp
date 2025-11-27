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
         (if (key@ positions c)
            (insert (@ positions c) e)
            (set@ positions c (heap (\(x y) (>=< (@ x 0) (@ y 0))) e))
         )
      )
   )
)

(defun valide (x y)
   (and 
      (>= x 0)
      (< x sz)
      (>= y 0)
      (< y sz)
      (set@ réponses x y "#")
   )
)

(loop c positions
   (setq l (to_list (@ positions c)))
   (loop p (irange 0 (- (size l) 1) 1)
      (setq e (@ l p))
      (setq suite (@@ l (+ p 1) 0))
      (setq d (maplist (\(x) (- x e)) suite))
      (setq a (+ suite d))
      (nconc a (maplist (\(x) (- e x)) d))
      (maplist (\(x) (valide (@ x 0) (@ x 1))) a)      
   )
)

(println (+ (maplist (\(x) (count x "#")) réponses)))

(loop l (enum carte)
   (loop c (enum (@ l 1))
      (set@ réponses (@ l 0) (@ c 0) (@ c 1))
   )
)


(loop c positions
   (setq l (to_list (@ positions c)))
   (loop p (irange 0 (- (size l) 1) 1)
      (setq e (@ l p))
      (setq suite (@@ l (+ p 1) 0))
      (setq dt (maplist (\(x) (- x e)) suite))
      (mloop (s d) suite dt
         (setq bas (+ s d))
         (while (valide (@ bas 0) (@ bas 1))
            (+= bas d)
         )
         (setq haut (- e d))
         (while (valide (@ haut 0) (@ haut 1))
            (-= haut d)
         )
      )
   )
)

(push codes "#")
(println 
   (+ 
      (maplist (\(x) 
            (+ (maplist (\(u) (count x u)) codes)))
         réponses
      )
   )
)



