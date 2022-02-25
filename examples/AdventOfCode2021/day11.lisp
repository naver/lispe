
;Date: 17/01/2022
;Author: Claude Roux
;Description: Day 11

(setq lescodes  (fread (+ _current "data/codes_day11.txt")))


(defun ventile(code x y)
   (check 
      (and 
         (>= x 0)
         (< x (@ taille 0))
         (>= y 0)
         (< y (@ taille 1))
         (@ codes x y)
      )
      (+= (@ codes x y) 1)
   )
)

(defun explose(codes x y res)
   (check 
      (and 
         (>= x 0)
         (< x (@ taille 0))
         (>= y 0)
         (< y (@ taille 1))
         (> (@ codes x y) 9)
         (push res (list x y))
         (set@ codes x y 0)
      )
   )
)

(defun affiche (étiquette codes)
   (println étiquette)
   (loop x codes
      (println (join x " "))
   )
   (println)
)



(defun compute(nb p)
   (setq tous 0)
   (setq flashes 0)
   (loop n (iota nb)
      (if tous (break))
      (loop x (iota0 (car taille))
         (loop y (iota0 (cadr taille))
            (+= (@ codes x y) 1)
         )
      )
      (setq ref true)
      (while ref
         (setq res ())   
         (loop x (iota0 (car taille))
            (loop y (iota0 (cadr taille))
               (explose codes x y res)
            )
         )
         (if (eq (+ (maplist '+ codes)) 0)
            (setq tous n)
         )
         (ncheck res
            (setq ref false)
            (+= flashes (size res))
            (loop coords res
               (loop ix '(-1 0 1)
                  (loop iy '(-1 0 1)
                     (if (or ix iy)
                        (ventile codes (+ ix (car coords)) (+ iy (cadr coords)))
                     )
                  )
               )
            )
         )
      )
   )

   (if (eq p 1)
      (println (+ "part " p ":") flashes)
      (println (+ "part " p ":") tous)
   )
)

(setq codes (maplist (\(x) (integers (split (trim x) ""))) (split (trim lescodes) "\n")))
(setq taille (rho codes))
(setq sauve (clone codes))
(affiche "Initial" codes)
(compute 100 1)

(setq codes sauve)
(setq taille (rho codes))
(compute 1000 2)


