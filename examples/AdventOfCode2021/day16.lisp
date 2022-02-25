
;Date  28/01/2022
;Author  Claude Roux
;Description  Jour16


(setq conv (key "0" "0000" "1" "0001" "2" "0010" "3" "0011" "4" "0100" "5" "0101" "6" "0110" "7" "0111" "8" "1000" "9" "1001" "A" "1010" "B" "1011" "C" "1100" "D" "1101" "E" "1110" "F" "1111"))

(defmacro conversion(v) (join (maplist (\(x) (key conv x)) v) ""))
(defmacro dec(lg) (convert_in_base lg 2 true))

(setq partie1 (integers))

(defun prendvaleurs(c résultats d)
   (setq inters "")
   (loop i (range d (size c) 5)
      (setq v (@@ c i (+ i 5)))
      (+= inters (@@ v 1 0))
      (check (eq (@ v 0) "0")
         (break)
      )
   )
   (push résultats (dec inters))
   (+ i 5)   
)

(defun prend15(c résultats d)
   (setq lg (@@ c (+ d 7) (+ d 22)))
   (setq lg (dec lg))
   (+= d 22)
   (setq bits (@@ c d (+ d lg)))
   (setq e 0)
   (while (< e (size bits))
      (setq e (entête bits résultats e))
   )
   (+ d lg)
)

(defun prend11(c résultats d)
   ; nb packets 11 bits
   (setq lg (@@ c (+ d 7) (+ d 18)))
   (setq lg (dec lg))
   (+= d 18)
   (while lg
      (setq d (entête c résultats d))
      (-= lg 1)
   )
   d
)

(defun entête(c résultats d)
   (setq version (@@ c d (+ d 3)))
   (push partie1 (dec version))
   (setq typeid (@@ c (+ d 3) (+ d 6)))
   (cond
      ( (eq typeid "100")
         (prendvaleurs c résultats (+ d 6))
      )
      ( (eq (@ c (+ d 6)) "0")
         (setq r (list (dec typeid)))
         (setq d (prend15 c r d))
         (push résultats r)         
         d
      )
      (true
         (setq r (list (dec typeid)))
         (setq d (prend11 c r d))
         (push résultats r)         
         d
      )
   )
)         

(defun évalue (r) 
   (ncheck (consp r)
      r
      (switch (car r) 
         (0
            (sum (maplist 'évalue (cdr r)))
         )
         (1
            (product (maplist 'évalue (cdr r)))
         )
         (2
            (min (maplist 'évalue (cdr r)))
         )
         (3
            (max (maplist 'évalue (cdr r)))
         )
         (5
            (integer (> (évalue (cadr r)) (évalue (caddr r))))
         )
         (6
            (integer (< (évalue (cadr r)) (évalue (caddr r))))
         )
         (7
            (== (évalue (cadr r)) (évalue (caddr r)))
         )
      )
   )
)

(setq v (conversion (fread (+ _current "codes_day16.txt"))))

(setq r ())
(entête v r 0)

(println "part 1:" (sum partie1))
(println "part 2:" (évalue (car r)))


