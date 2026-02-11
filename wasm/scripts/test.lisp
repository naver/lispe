(setq nb 0)
(setq c (complex 10 -2))
(println (sqrt c))
(defun tst(x)
   (setg nb (+ nb 1))
   (if (eq x 1) 
      1
      (* x (tst (- x 1))
      )
   )
)

(println (tst 2000) nb)
