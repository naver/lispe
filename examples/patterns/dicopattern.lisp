

(defpat tst({_:y _:y $:z})
   (println y z)
)

(setq d {"12":"machine" "truc":"machin" "bidule":"machine"})

(tst d)


(setq m {"12":"machine"})

(defpat unique({x:y $:z})
   (println x y z)
   (unique z)
)

(defpat unique( {} )
   (println 'vide)
)

(unique m)


(defpat un({x:y})
   (println 'un x y)
)

(un m)
(println)
(println "We should have an error now!!!")
(un d)


















