(data@ (Point integer_ integer_))

(println (Point 10 20))

(defun checkless (x) (< (car x) (cadr x)))

(defpat action ( (prgx `\d\d` x) u)
   (println 'ok u)
)

(defpat action ( (rgx `%a+` x) u)
   (println 'Encore u)
)

(defpat action ( (checkless x) u)
   (println 'Yooo u)
)

(action '(200 220) 'first)
(action "12" 'finally)
(action "Test" 'thing)


(maybe
   (action '(280 220) 'first)
   (println 'Oups)
)


(defpat addnumbers ( (integer_ x) (integer_ y))
   (+ x y)
)

(maybe
   (println 'First (addnumbers 10 20))
   (println 'Second (addnumbers 10 'x))
   (println 'Aie)
)






