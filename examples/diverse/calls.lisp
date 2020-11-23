
(defun appel(x)
    (+ x 10)
)

(defun test(x)
    (+ 20 (appel x))
)


(test 20)


