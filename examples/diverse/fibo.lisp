; fibonacci with memoization
; note that: (key d n v) returns d...

(defun fibo (n (d {1:1}) )
   (select 
      (keyi d n)
      (if (<= n 1)
         1
         (keyi
            (keyi d n
               (+
                  (fibo (- n 1) d)
                  (fibo (- n 2) d)
               )
            )
            n
         )
      )
   )
)

(println (fibo 10))








