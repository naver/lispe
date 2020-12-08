(defun fibo (n (d {1:1}) )
   (if (at d n)
      (at d n)
      (ncheck (> n 1)
         1
         (key d n (+ (fibo (- n 1) d) (fibo (- n 2) d)))
         (key d n)
      )
   )
)

(println (fibo 10))





