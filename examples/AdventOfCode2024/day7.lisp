;Date: 22/12/2024
;Author: Claude Roux
;Description: Advent of code 2024 day 7

(setq données (fread (+ _current "data/day7.txt")))

(setq données (split données "\n"))

(setq données (maplist (\(x) (split x ":")) données))


(setq score 0)

(defun calcul(accu lst valeur)
   (if (nullp lst)
      (== accu valeur)
      (or
         (calcul (+ accu (car lst)) (cdr lst) valeur)
         (calcul (* accu (car lst)) (cdr lst) valeur)
         (calcul (integer (+ "" accu (car lst))) (cdr lst) valeur)
      )
   )
)

(loop equation données
   (setq valeur (integer (car equation)))
   (setq arguments (integers (split (trim (cadr equation)) " ")))
   (+= score (* valeur (calcul (car arguments) (cdr arguments) valeur)))
)
(println score)


