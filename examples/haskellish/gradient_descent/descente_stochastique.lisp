
;Date: 24/05/2021
;Author: Claude Roux
;Description: Descente de gradient stochastique

;(setq X (rho 10 3 '(1 0.72 0.32 1 0.75 0.12 1 0.53 0.65 1 0.27 0.82 1 0.49 0.15 1 0.02 0.19 1 0.35 0.87 1 0.99 0.71 1 0.98 0.92 1 0.73 0.19)))
;(setq Y (floats 6.93 5.99 1.46 1.44 4.51 1.25 2.53 6.88 6.25 6.36))

(setq X (rho 20 3 (numbers 1 0.72 0.32 1 0.75 0.12 1 0.53 0.65 1 0.27 0.82 1 0.49 0.15 1 0.02 0.19 1 0.35 0.87 1 0.99 0.71 1 0.98 0.92 1 0.73 0.19 1 0.72 0.32 1 0.75 0.12 1 0.53 0.65 1 0.27 0.82 1 0.49 0.15 1 0.02 0.19 1 0.35 0.87 1 0.99 0.71 1 0.98 0.92 1 0.73 0.19)))
(setq Y (numbers 6.93 5.99 1.46 1.44 4.51 1.25 2.53 6.88 6.25 6.36 6.93 5.99 1.46 1.44 4.51 1.25 2.53 6.88 6.25 6.36))


(setq η 0.5)

(defun descent(a loss)
   (lloop (x y) X Y
      (setq v (• y - (sum (• a * x))))
      (setq deriv (maplist (λ(xx) (• (sign xx) * v)) x))
      (setq a (- a (* deriv η)))
      (setq Loss (sum (zipwith (λ(x y) (• (• y - (sum (•  a * x))) ^^ 2)) X Y)))
      (if (< Loss loss)
         (return (list a Loss))
      )
   )
)

(setq a (numbers 0.1 0.1 0.1))
(setq loss (sum (zipwith (λ(x y) (• (• y - (sum (• a * x))) ^^ 2)) X Y)))
(setq r (list a loss))
(setq tm
   (elapse
      (loop iter (range 0 1000 1)
         (setq v (descent (@ r 0) (@ r 1)))
         (if (nullp v)
            (break)
            (setq r v)
         )
      )
   )
)

(println "Weight Loss Iterations" (@ r 0) (@ r 1) iter)
(println "Time:" tm)






