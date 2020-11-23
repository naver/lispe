

(defpat addnumbers ( (integer_ x) (numberp y))
   (+ x y)
)

(maybe
   (println 'First (addnumbers 10 20))
   (println 'Second (addnumbers 40 'x))
   (println 'Aie)
)



