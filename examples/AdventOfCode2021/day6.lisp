
;Date: 05/01/2022
;Author: Claude Roux
;Description: Lanternfishes

(setq thefishes  (fread (+ _current "data/codes_day6.txt")))

(defun generation(fishes gn) 
   (loop i (irange 0 gn 1)
      (-= fishes 1)
      (setq nb (replaceall fishes -1 6))
      (extend fishes (rho nb (integers 8)))
   )
)

(defun taille_generation(fishes gn) 
   (loop i (irange 0 gn 1)
      (-= fishes 1)
      (setq nb (replaceall fishes -1 6))
      (extend fishes (rho nb (integers 8)))
   )
   (size fishes)
)

(compute 40) 
(compute 128)

(defun compute(gen)
   (println 'extraction)
   (setq fishes (integers (split thefishes ",")))
   (generation fishes gen)
   (println 'calcul)

   (setq nb (integer 0))
   (loop i (irange 0 9 1)
      (+= nb (* (taille_generation (integers i) gen) (count fishes i)))
   )
   (println 'Taille nb)
)


