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
   (let ( (d (dot v1 v2))
         (n1 (norme v1))
         (n2 (norme v2)))
      (maybe
         (/ d (* n1 n2))
         0)))

(cosine v1 v2)


