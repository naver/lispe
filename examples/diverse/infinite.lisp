(setq a '(1 2 3))
(setq b '(10 20 30))
(push a b)
(push b a)

(println a)
(println b)

(defun traverse (l)
   (check (consp l)
      (ife (mark l)
         (println "Warning: infinite loop")
         (println (at l 0))
         (mark l true)
         (traverse (car l))
         (traverse (cdr l))
         (mark l false)
      )
   )
)

(traverse a)



