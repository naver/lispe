;Date: 25/10/2023
;Author: Claude Roux
;Description: Game of life

(defmacro ⊖(l n) (rotate l n true))

(defun lifeinit(x y) (rho x y . random_choice (* x y) (integers 0 1) 17))

;(setq r (lifeinit 20 40))
;(setq r . rho 4 8 . integers 0 1 1 0 0 0 1 0 1 0 1 1 1 0 1 0 1 0 0 0 1 1 1 0 1 0 0 0 0 1 1 1)

;(setq r . rho  20 20 (integers 1 1 0 0 0 0 1 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 1 1 0 0 1 1 1 0 0 0 0 0 1 1 0 1 1 1 0 1 0 0 1 0 1 0 0 0 1 1 0 0 0 1 0 1 0 0 0 0 0 1 1 1 0 0 1 0 0 1 0 0 0 0 0 0 0 1 1 0 0 1 1 1 1 0 0 1 1 1 0 1 1 1 0 0 1 0 1 0 0 1 1 1 0 0 1 1 0 0 1 0 1 1 0 0 0 1 1 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 0 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 0 0 1 1 0 1 0 0 0 1 1 0 0 1 0 0 1 1 1 1 0 1 0 1 0 1 0 1 0 0 0 1 1 0 1 0 0 1 0 0 0 1 1 0 0 0 1 0 0 0 0 1 0 1 1 1 0 1 0 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 1 1 1 0 0 1 1 0 1 1 1 0 1 1 0 1 1 0 1 0 0 1 1 0 0 0 0 0 1 1 0 1 0 1 0 0 0 1 1 1 0 1 1 0 0 1 0 0 1 1 0 0 1 1 1 0 1 1 0 0 1 0 0 1 0 1 1 0 0 0 0 0 0 1 1 1 0 0 1 1 0 0 1 0 1 1 1 0 0 0 0 1 1 0 0 1 1 0 1 0 1 1 0 1 1 0 0 0 0 1 0 1 1 0 0 0 1 0 0 0 1 1 1 1 0 1 1 1 0 0 0 0 1 0 0 0 0 1 0 1 1 1 0 0 0 0 1 1 1 1 0 0 1 1 0 1 1 0 0 1 1 1 0 0 0 0 1 0 1 1 0 0 0 0))

(setq r (rho 5 5 (integers 0 0 0 0 0 0 0 0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 )))

(defun gol1(⍵) (maplist (\(x) . ⌽ ⍵ x) '(1 0 -1)))
(defun gol2(⍵) (outer (\(x ⍺) . ⊖ ⍺ x) '(1 0 -1)  . maplist (\(x) . ⌽ ⍵ x) '(1 0 -1)))
(defun gol3(⍵) (-// '+ . outer (\(x ⍺) . ⊖ ⍺ x) '(1 0 -1)  . maplist (\(x) . ⌽ ⍵ x) '(1 0 -1)))
(defun gol4(⍵) (-// '+ . -// '+ . outer (\(x ⍺) . ⊖ ⍺ x) '(1 0 -1)  . maplist (\(x) . ⌽ ⍵ x) '(1 0 -1)))
(defun gol5(⍵) ((\(⍺) (& (== ⍺ 4) ⍵)) (-// '+ . -// '+ . outer (\(x ⍺) . ⊖ ⍺ x) '(1 0 -1)  . maplist (\(x) . ⌽ ⍵ x) '(1 0 -1))))
(defun gol6(⍵) ((\(⍺) ( == ⍺ 3)) (-// '+ . -// '+ . outer (\(x ⍺) . ⊖ ⍺ x) '(1 0 -1)  . maplist (\(x) . ⌽ ⍵ x) '(1 0 -1))))
(defun gol7(⍵) ((\(⍺) (| (& (== ⍺ 4) r) ( == ⍺ 3))) . -// '+ . -// '+ . outer (\(x ⍺) . ⊖ ⍺ x) '(1 0 -1)  . maplist (\(x) . ⌽ ⍵ x) '(1 0 -1)))
(defun gol8(⍵) ((λ(⍺) (| (& (== ⍺ 4) r) (== ⍺ 3))) (⌿ '+ (⌿  '+ (° (λ (x ⍺) (⊖ ⍺ x)) '(1 0 -1) (↑ (λ (x) (⌽ ⍵ x)) '(1 0 -1)))))))



(setq nb 20)
(println . prettify r nb)
;(println . prettify (gol1 r) nb)
;(println . prettify (gol2 r) nb)
;(println . prettify (gol3 r) nb)
(println . prettify (gol4 r) nb)
;(println . prettify (gol5 r) nb)
;(println . prettify (gol6 r) nb)
(println . prettify (gol7 r) nb)








