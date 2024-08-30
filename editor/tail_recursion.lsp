; This is a demonstation of how tail recursion works


(defun fact(x y)
    (if (eq x 1)
        y
        (block
           (print x)
           (fact (- x 1) (* x y))
        )))

(setq y 1)
(fact 10 y)

