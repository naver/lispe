
; First we define some data structures
; nil or _ this is the same value

(data@ (Point integer_ integer_) (Pixel _ _) (Circle (Point _ _)  nil) (Rectangle (Point _ _)  nil nil))


; Then some pattern methods


(defpat Surface ((Circle (Point x y) r)) (* _pi r r))
(defpat Surface ((Rectangle _ h w)) (* h w))
(defpat Surface (true) 'wrong)


(println "Circle:" (Surface (Circle (Point 12 32) 10)))
(println "Circle:" (Surface ((atom "Circle") (Point 20 30) 3)))

(println "Rectangle:" (Surface (Rectangle (Point 21 20) 10 20)))

(setq x 10)
(setq y 20)
(setq r 12)

(setq cercle (Circle (Point x y) r))

(println "Circle:" (Surface cercle))
(println "Point:" (Surface (Point 10 20)))



(data@ Shape (Triangle _) (Square _))

(defpat dimension ( (Shape x))
   (println 'Dimension x)
)

(dimension (Triangle 10))
(dimension (Square 20))




