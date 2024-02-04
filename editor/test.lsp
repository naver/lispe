(setq n 10000)

(loop n
    (setq n (- n 1))
    (+ n (* 2 n) 3 n)
    (print n)
)

