;Date: 18/01/2022
;Author: Claude Roux
;Description: Day 13
; This code should be executed in a terminal for better display


(setq values (fread (+ _current "codes_day13.txt")))

(setq positions (trim (@@ values 0 "fold")))
(setq instructions (split (trim (@@ values +"fold" 0)) "\n"))

(setq coords (maplist (\(x) (integers (split x ","))) (split positions "\n")))
(setq mmx (+ 1 (max (maplist (\(x) (car x)) coords))))
(setq mmy (+ 1 (max (maplist (\(x) (cadr x)) coords))))

(setq the_map (rho mmy mmx (integers 0)))

(loop c coords
   (set@ the_map (@ c 1) (@ c 0) 1)
)

(defun folding_y(v)
   (loop y (irange (+ v 1) mmy 1)
      (setq yy (% (- v (% y v)) v))   
      (+= (@ the_map yy) (@ the_map y))
      (set@ the_map y (rho mmx (integers 0)))
   )
)

(defun folding_x(v)
   (loop x (irange (+ 1 v) mmx 1)
      (setq xx (% (- v (% x v)) v))
      (loop y (irange 0 mmy 1)
         (+= (@ the_map y xx) (@ the_map y x))
         (set@ the_map y xx (min 1 (@ the_map y xx)))
         (set@ the_map y x 0)
      )
   )
)

(defun compute(instructions)
   (maplist 
      (\(x) 
         (setq e (split x "=")) 
         (if (in (@ e 0) "x") 
            (list 'folding_x (integer (@ e 1)))
            (list 'folding_y (integer (@ e 1)))
         )
      )
      instructions
   )
)

(eval (car (compute (list (car instructions)))))
(println 'part1 (+ (flatten the_map)))

; We reset our the_map 
(setq the_map (rho mmy mmx (integers 0)))
(loop c coords
   (set@ the_map (@ c 1) (@ c 0) 1)
)

(loop i (compute instructions)
   (eval i)
)

; displaying the final code, better in a terminal with non proportional fonts.
(loop l the_map
   (setq ligne "")
   (loop e l
      (if (eq e 0)
         (+= ligne " ")
         (+= ligne "#")
      )
   )
   (if (eq (trim ligne) "")
      (break)
   )
   (println (trimright ligne))
)


