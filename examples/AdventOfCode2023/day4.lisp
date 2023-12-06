;Date: 05/12/2023
;Author: Claude Roux
;Description: Day 4

(setq données (fread (+ _current "data/day4.txt")))


(setq d (split données "\n"))

(setq dd 
   (maplist (\(x) (maplist (\(e) (integers . split e " ")) (split (@@ x ":" -) "|"))) d)
)

(setq sz (integers))
(setq v 0)
(loop l dd
   (setq i (&&& (@ l 1) (@ l 0)))
   (push sz (size i))
   (check (size i)
      (+= v (<< 1 (- (size i) 1)))
   )
)

(println v)

(setq res (rho (size sz) '(1)))

(setq z (integers 0))
(loop i (enum sz)
   (setq k (@ i 1))
   (setq r (nconcn z (rho k (integers . @ res (@i 0)))))
   (push z 0)
   (+= res r)
)

(+ res)


