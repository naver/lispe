
;Date: 27/01/2022
;Author: Claude Roux
;Description: Jour 1

; Résolution du jour 1

(setq DATA (fread (+ _current "data/codes_day1.txt")))

(setq codes (integers (split DATA "\n")))

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

(println 'P2 (p2 codes 0))

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

(println 'P3 (p3 codes 0))


