;Date: 21/02/2024
;Author: Claude Roux
;Description: Distance Cosinus

(setq v1 (floats 1 2 3))
(setq v2 (floats 4 5 6))

(defun dot(v1 v2)
   (sum (* v1 v2))
)

(defun norme(v)
   (sqrt (sum (* v v)))
)

(defun cosine(v1 v2)
   (maybe
      (/ (dot v1 v2) (* (norme v1) (norme v2)))
      0
   )
)

(cosine v1 v2)






