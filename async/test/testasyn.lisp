(use 'lispe_async)

(setq l ())
(setq u 100)
(defun appel(l)
    (push l 1000)
)

(push l (async "premier" '(+ 10 u 1)))
(push l (async "second" '(+ 10 u 2)))
(push l (async "troisième" '(+ 10 u 3)))
(push l (async "quatrième" '(+ 10 u 4)))
(push l (async "quinte" '(appel (integers u 11 12 25))))

(println l)

(elapse
    (loopcount 1000 o
        (setq u (* o 2))
        (async_run l)
    )
)

