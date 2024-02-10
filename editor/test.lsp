(setq r "abcdef")
(setq r (put r 1 "BB"))

(defun addmap(x)
    (insert {} "t" x)
)

(addmap 20)
(addmap 30)
(addmap 40)

(defun fct(x)
    (setq l '(a b c))
    (push l x)
    (print l)
)

(fct 10)
(fct 11)
(fct 12)
(fct 13)

(setq m {"a":"b" 12:14})
(= '(a b) '(a b))
(setq n 10000)

(while n
    (setq n (- n 1))
    (+ n (* 2 n) 3 n)
    (print n)
)

(setq r (cons 'a '(b c)))
(setq l '(u v x y z))
(setq r ())
(setq l ())

(setq v (list 1.1 2.2 3.3 'a "xwz"))
(setq b v)

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
(print (apply '+ '(1 2 3 4 5)))
(print (mapcar '(lambda (x) (+ x 20)) '(1 2 3 4 5)))

(print (mapcar 'chr (range 97 120 1)))
(print _args _current)

