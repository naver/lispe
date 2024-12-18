;Date: 17/12/2024
;Author: Claude Roux
;Description: Advent of code 2024 day 4


(setq enigma (fread (+ _current "data/day4.txt")))

(setq message (split enigma "\n"))

(setq nb (+ (maplist (\(x) (+ (count x "XMAS") (count x "SAMX"))) message)))
(setq m (transpose (maplist (\(x) (split x "")) message)))

(setq m (maplist (\(x) (join x "")) m))
(+= nb (+ (maplist (\(x) (+ (count x "XMAS") (count x "SAMX"))) m)))

(setq X (size message))
(setq Y (size (@ message 0)))

(defun calcul(FD)
   (setq l 0)
   (loop ms message
      (loop x (findall ms (car FD))
         (setq ch FD)
         (setq ex x)
         (setq ey l)
         (while (and ch (< ex X) (< ey Y) (= (@ message ey ex) (car ch)))
            (+= ex 1)
            (+= ey 1)
            (setq ch (cdr ch))
         )         
         (if (nullp ch) (+= nb 1))

         (setq ch FD)
         (setq ex x)
         (setq ey l)
         (while (and ch (>= ex 0) (< ey Y) (= (@ message ey ex) (car ch)))
            (-= ex 1)
            (+= ey 1)
            (setq ch (cdr ch))
         )         
         (if (nullp ch) (+= nb 1))
      )
      (+= l 1)
   )
   nb
)

(calcul (strings "X" "M" "A" "S"))
(calcul (strings "S" "A" "M" "X"))
(println nb)

(defmacro cherche(s C1 C2)
   (filterlist (\(p) (and (< p (- X 2)) (= (@ s (+ p 2)) C2))) (findall s C1))
)

(setq dico {"M":"S" "S":"M"})

(defun ajoute(msg ms l C1 C2)
   (loop x (cherche ms C1 C2)
      (if 
         (and 
            (< l (- Y 2)) 
            (= (@ msg (+ l 1) (+ x 1)) "A")
            (= (@ msg (+ l 2) x) (@ dico C2))
            (= (@ msg (+ l 2) (+ x 2)) (@ dico C1))
         )
         (+= nb 1)
      )
   )
)

(defun croix(msg)
   (setq l 0)
   (loop ms msg
      (ajoute msg ms l "M" "S")
      (ajoute msg ms l "M" "M")
      (ajoute msg ms l "S" "S")
      (ajoute msg ms l "S" "M")
      (+= l 1)
   )
)


(setq nb 0)
(croix message)
(println nb)

