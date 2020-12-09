; fibonacci with memoization
; note that: (key d n v) returns d...

(defun fibo (n (d {1:1}) )
   (select 
      (at d n)
      (if (<= n 1)
         1
         (key
            (key d n
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

(println (fibo 20))



