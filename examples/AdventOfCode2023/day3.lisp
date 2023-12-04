;Date: 03/12/2023
;Author: 
;Description: 


(setq lesdonnées (fread (+ _current "data/day3.txt")))

(setq symbols (filterlist (\(x) (and (!= x ".") (not (digitp x)) (!= x "\n"))) (unique (split lesdonnées ""))))

(setq données (replace lesdonnées "." " "))
(loop c symbols
   (setq données (replace données c "_"))
)

(setq lines (split données "\n"))

(setq m (rho 141 141 (strings " ")))

(loop l (enum lines)
   (setq ids (findall (@ l 1) "_"))
   (loop id ids
      (loop ln '(-1 0 1)
         (loop cl '(-1 0 1)
            (setq x (+ (@ l 0) ln))
            (setq y (+ id cl))
            (check (digitp (@ lines x y))
               (set@ m x y (@ lines x y))
               (setq dr -1)
               (while (digitp (@ lines x (+ dr y)))
                  (set@ m x (+ dr y) (@ lines x (+ dr y)))
                  (-= dr 1)
               )
               (set@ m x (+ dr y) " ")
               (setq dr 1)
               (while 
                  (and
                     (< (+dr y) 140)
                     (digitp (@ lines x (+ dr y)))
                  )
                  (set@ m x (+ dr y) (@ lines x (+ dr y)))
                  (+= dr 1)
               )
               (set@ m x (+ dr y) " ")
            )
         )
      )
   )
)

(+ (, (maplist (\(x) (integers (split (join x "") " "))) m)))

(pop symbols "*")
(setq données (replace lesdonnées "." " "))
(loop c symbols
   (setq données (replace données c " "))
)

(setq lines (split données "\n"))

(setq r 0)
(loop l (enum lines)
   (setq ids (findall (@ l 1) "*"))
   (loop id ids
      (setq m (rho 3 141 (strings " ")))
      (loop ln '(-1 0 1)
         (loop cl '(-1 0 1)
            (setq x (+ (@ l 0) ln))
            (setq y (+ id cl))
            (check (digitp (@ lines x y))
               (set@ m (+ 1 ln) y (@ lines x y))
               (setq dr -1)
               (while (digitp (@ lines x (+ dr y)))
                  (set@ m (+ 1 ln) (+ dr y) (@ lines x (+ dr y)))
                  (-= dr 1)
               )
               (set@ m (+ 1 ln) (+ dr y) " ")
               (setq dr 1)
               (while 
                  (and
                     (< (+dr y) 140)
                     (digitp (@ lines x (+ dr y)))
                  )
                  (set@ m (+ 1 ln) (+ dr y) (@ lines x (+ dr y)))
                  (+= dr 1)
               )
               (set@ m (+ 1 ln) (+ dr y) " ")
            )
         )
      )
      (setq b (, (maplist (\(x) (integers (split (join x "") " "))) m)))
      (if (eq (size b) 2)
         (+= r (* b))
      )
   )
)

(println r)

