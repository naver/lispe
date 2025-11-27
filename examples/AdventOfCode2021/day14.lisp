
;Date: 18/01/2022
;Author: Claude Roux
;Description: Jour14



(setq codes (fread (+ _current "data/codes_day14.txt")))

(setq règles (key))
(maplist 
   (\(x) 
      (setq r (split x "->"))
      (key@ règles (trim (car r)) (trim (cadr r)))
   )
   (split (trim codes) "\n")
)

(setq reference "CPSSSFCFOFVFNVPKBFVN")
(setq code reference)
(setq couples (set))

(defun build_polymer(nb code)
   ; d'abord on construit tous les couples 
   (setq couples (key))
   (loop i (irange 0 (- (size code) 1) 1)
      (setq sub (extract code i (+ i 2)))
      (+= (@ couples sub) 1)
   )
   (setq comptes (key))
   (maplist (\(x) (+= (@ comptes x) 1)) code)
   (loopcount nb
      (setq nouveau (key))
      (loop c couples
         ; r est le nouveau caractère que l'on rajoute          
         (setq r (@ règles c))
         (setq n (@ couples c))
         ; on incrémente le nombre de caractère du nombre de couple présent en mémoire 
         (+= (@ comptes r) n)
         (+= (@ nouveau (+ (@ c 0) r))  n)
         (+= (@ nouveau (+  r (@ c 1)))  n)
      )      
      (setq couples nouveau)
   )
   comptes
)

(println 'part1 (- (flip (minmax (values@ (build_polymer 10 code))))))
(println 'part2 (- (flip (minmax (values@ (build_polymer 40 code))))))







