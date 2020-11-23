# Some examples on how to use map, filter, zip or zipwith

(println 'map (map '(+ 1) '(1 2 3)))
(println 'map (map '(lambda (x) (* x 3 (- x 1))) '(3 4 5)))
(println 'filter (filter '(< 3) (range 1 20 1)))
(println 'filter (filter '(lambda (x) (< (- x 1) 10)) (range 1 20 1)))
(println 'zip (zip '(1 2 3) '(4 5 6) '(7 8 0)))
(println 'zipwith (zipwith '+ '(1 2 3) '(4 5 6) '(7 8 0)))
(println 'zipwith (zipwith '(lambda (x y z) (+ x (- y z))) '(1 2 3) '(4 5 6) '(7 8 0)))
(println 'takewhile (takewhile '(< 10) (range 1 20 1)))
(println 'fold (foldl '+ 10 '(1 2 3 4)))
(println 'foldr1 (foldr1 '+ '(1 2 3 4)))
(println 'scanr (scanr '+ 10 '(1 2 3 4)))
(println 'scanl1 (scanl1 '+ '(1 2 3 4)))
(println 'scanr (scanr '+ 0 '(3 5 2 1)))

(println 'apply ': (apply '+ (takewhile '(< 10000) (map '+ (irange 1 1)))))

(setq a '+)
(println (map a '(1 2 3)))

(setq a 'log)
(println (map a '(1 2 3)))


(defun tst (accu x) (+ accu (* 2 x)))

(defun call_scan(l)
   (scanl1 'tst l)
)

(defun call_fold(l)
   (foldl1 'tst l)
)

(defun call_scanr (l)
   (scanr1 (lambda (x accu) (+ 1 accu x)) l)
)

(println 'foldl1_tst (call_fold '(1 2 3 4 5)))
(println 'scanl1_tst (call_scan '(1 2 3 4 5)))
(println 'scanr1_lb (call_scanr '(1 2 3 4 5)))


; We let LispE compose the following expression.
; At each step it processes both the scanl1 and the map
(println 'map_composed_with_scan (map '* (scanl1 '+ '(10 20 30))))

; We prevent LispE from composing in the following expression:
(println 'non_composition_map (!map '* (scanl1 '+ '(10 20 30))))















