;Date: 25/11/2024
;Author: Claude Roux
;Description: Exemple de prédicats


(defpred teste ([]) 
   true
)

(defpred teste ([a $ b])
   (< a 10)
   (println a)
   (teste b)
)

(defpred teste (l)
   (println "We stop" l)
)

(teste '(1 2 11 12 13))







