;Date: 25/11/2024
;Author: Claude Roux
;Description: Exemple de prédicats


(defpred teste ([]) 
   (println 'vide)
   true
)

(defpred teste ([a $ b])
   (setq e (+ a "_a"))
   (println e)
   (teste b)
)


(teste '("a" "b"))


