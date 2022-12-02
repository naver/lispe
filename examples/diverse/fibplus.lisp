;Date: 02/12/2022
;Author: Claude Roux
;Description: Fibonacci


(defun fibo(x)
   (fib x)
)

(defun fib(x)
   (if (eq x 1)
      1
      (+ x (fib (- x 1)))
   )
)


(fibo 100)






