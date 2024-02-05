(setq n 10000)

(loop n
    (setq n (- n 1))
    (+ n (* 2 n) 3 n)
    (print n)
)

(setq r (cons 'a '(b c)))
;(setq l '(u v x y z))
;(setq r ())
;(setq l ())

;(setq v (list 1 2 3 'a "b"))

;(defun tst(x y)
;    (* x y)
;)

;(print (tst 10 20))
;(print (tst (car v) 13))
