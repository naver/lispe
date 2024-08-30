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
    (println l)
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
    (println n)
)

(setq r (cons 'a '(b c)))
(setq l '(u v x y z))
(setq r ())
(setq l ())

(setq v (list 1.1 2.2 3.3 'a "xwz"))
(setq b v)

(println "Lambda:" ((lambda (x y) (- x y)) 12 -21.4))

(setq r 4)

(println "COND:"
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

(println (tst 10 20))
(println (tst (car v) 13))
(println (apply '+ '(1 2 3 4 5)))
(println (mapcar '(lambda (x) (+ x 20)) '(1 2 3 4 5)))

(println (mapcar 'chr (range 97 120 1)))
(println _args _current)

(defun sous(x)
    (setq r ( 
        (lambda (e)
                (
                    (lambda (v) (+ v x e)) 
                    300
                )
            )
        200
        )
    )
    (println r x)
)

(println (sous 100))