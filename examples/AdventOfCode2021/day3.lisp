
;Date: 30/12/2021
;Author: Claude Roux
;Description: sous-marin gamma/epsilon


(setq code (fread (+ _current "data/codes_day3.txt")))


(defmacro nb0(x) (- (size x) (sum x)))

(defmacro extracts(code)
   (matrix
      (maplist
         (\(x)
            (integers
               (split (trim x) "")
            )
         )
         (split code "\n")
      )
   )
)

(setq codes (extracts code))
(setq nb (size (@ codes 0)))

(setq l "")
(setq gamma (integer (+ "0b" (join (maplist (\(x) (integer (>= (sum x) (nb0 x)))) (irank codes -1)) ""))))

(setq epsilon (& (~ gamma) 4095))

(println 'part1 (* epsilon gamma))

(defun calcul(op s)
   (loop i (iota0 nb)
      (if (eq (size s) 1) (break))
      (setq keep (rank s -1 i))
      (if (apply op (list (sum keep) (nb0 keep))) (setq keep (maplist (\(x) (- 1 x)) keep)))
      (setq s (matrix (// keep s)))
   )
   (integer (+ "0b" (join (@ s 0) "")))
)

(setq r (calcul '< codes))
(setq v (calcul '>= codes))
(println 'part2 (* r v))




