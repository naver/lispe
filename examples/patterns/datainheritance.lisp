
(data@ (Truc _) (Machin _))
(data@ test (truc _) (machin _))


(defpat action ( (test x) )
   (println x)
)

(defpat action ( (Truc x))
   (println 'Test x)
)

(defpat action ( (Machin x))
   (println 'Machin x)
)

(action (truc 'truc))
(action (machin 'machin))
(action (Truc 'Truc))
(action (Machin 'Machin))








