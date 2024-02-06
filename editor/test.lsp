(equal '(a b) '(a b))
(setq n 10000)

(loop n
    (setq n (- n 1))
    (+ n (* 2 n) 3 n)
    (print n)
)

(setq r (cons 'a '(b c)))
(setq l '(u v x y z))
(setq r ())
(setq l ())

(setq v (list 1 2 3 'a "b"))
(print "Lambda:" ((lambda (x y) (- x y)) 12 -21.4))

(setq r 4)

(print "COND:"
    (cond
        ((eq r 10) (+ 200 100))
        ((eq r 3) (+ 12 r))
        ((> r 5) (* 2 r))
        (true 'ok)
    )
)

(defun tst(x y)
    (* x y)
)

(print (tst 10 20))
(print (tst (car v) 13))

