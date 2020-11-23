
(defpat tst ( (x y $ z))
   (println 'list x y z)
   (tst z)
)

(defpat tst ((x))
   (println 'unique x)
   (tst ())
)

(defpat tst ( ())
   (println 'empty)
)

(tst ())
(tst '(1 2 3 4 5))
(tst '(1 2))
(tst '(1))



(defpat parcours ((string_ (flip (in 'restaurant s))))
   (println 'ok s)
)

(parcours "la ville ne contient aucun bon restaurant italien")






