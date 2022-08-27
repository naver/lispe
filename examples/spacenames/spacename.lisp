;Date: 27/08/2022
;Author: Claude Roux
;Description: Space Creation



(defspace test
   (defun truc(x u)
      (+ x u)
   )
)

(defun rappel(x y)
   (* x y)
)

(space test    
   (setq r (truc 10 20))
   (setq j (truc 10 30))
   (setq k (rappel r j))
)

(println r j k)



