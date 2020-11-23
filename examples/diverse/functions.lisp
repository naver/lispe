

(defun other(x y (i -1) (j -1))
   (+ x y i j)
)

(defun calling (x y (() l))
   (println x y l)
)



(other 19 20)

(calling 10 20 40 60 80 900)

(calling 1 0)




