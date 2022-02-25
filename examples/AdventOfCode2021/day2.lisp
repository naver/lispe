
;Date: 30/12/2021
;Author: Claude Roux
;Description: Sous-marin

(setq instructions (fread (+ _current "data/codes_day2.txt")))

(setq depth 0)
(setq x 0)
(setq aim 0)


(defun forward (i)
   (+= x i)
)

(defun down(i)
   (+= depth i)
)

(defun up(i)
   (-= depth i)
)

(maplist 
   (\(v)  
      (setq e (split (trim v) " "))
      (check e
         (eval (list (atom (car e)) (integer (cadr e))))
      )
   )
   (split instructions "\n")
)

(println 'part1 (* x depth))

(setq depth 0)
(setq x 0)
(setq aim 0)

(defun forward2 (i)
   (+= x i)
   (+= depth (* aim i))
)

(defun up2(i)
   (-= aim i)
)

(defun down2(i)
   (+= aim i)
)


(maplist 
   (\(v)  
      (setq e (split (trim v) " "))
      (check e
         (eval (list (atom (+ (car e) "2")) (integer (cadr e))))
      )
   )
   (split instructions "\n")
)

(println 'part1 (* x depth))


