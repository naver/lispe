
;Date: 27/01/2022
;Author: Claude Roux
;Description: Jour 1

; Résolution du jour 1

(setq codes (fread (+ _current "data/codes_day1.txt")))
(setq codes (integers (split codes "\n")))

(defun p2(liste nb)
   (if (eq (size liste) 1)
      nb
      (if (<
            (car liste)
            (cadr liste)
         )
         (p2 (cdr liste) (+ nb 1))
         (p2 (cdr liste) nb)
      )
   )
)

(println (p2 codes 0))


(defun p3(liste nb)
   (if (eq (size liste) 3)
      nb
      (if (<
            (+ (car liste) (cadr liste) (caddr liste))
            (+ (cadr liste) (caddr liste) (cadddr liste))
         )
         (p3 (cdr liste) (+ nb 1))
         (p3 (cdr liste) nb)
      )
   )
)

(p3 codes 0)





