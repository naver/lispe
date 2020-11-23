(setq c1 (chrono))
((lambda (x y) (if (eq x 0) (* 2 y) (self (- x 1) (+ y 2)))) 200000 1)
(setq c2 (chrono))
(println (- c2 c1))













