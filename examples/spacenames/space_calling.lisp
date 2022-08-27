;Date: 27/08/2022
;Author: Claude Roux
;Description: loading with space name



(load (+ _current "space_called.lisp") truc)


(defun calculus (x y)
   (- x y)
)

; We call the version in load 
(println (space truc (calculus 20 40)))

; We call the local version
(println (calculus 20 40))

 
(space mainspace_ (calculus 100 20))



 






