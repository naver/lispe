
(defmacro ch (x l) (nullp (find l x)))
(defmacro within (x l) (not (ch x l)))

(setq l '(10 20 30 40 50))

; (println (not (nullp (find l 20))))
(println (within 10 l))
(println (within 20 l))
(println (within 100 l))
(println (within 1 '(1 2 3 4)))






