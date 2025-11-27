;Date: 04/01/2022
;Author: Claude Roux
;Description: Bingo


(setq codes (fread (+ _current "data/codes_day4.txt")))

(setq codes (split codes "\n"))
(setq letirage (car codes))
(setq lesgrilles (join (cdr codes) " "))

(setq tirage (integers (split letirage ",")))
(setq numeros (integers (split lesgrilles)))
(setq choix (rho (/ (size numeros) 25) 5 5 (floats 0)))
(setq valeurs (dictionaryn))

(setq x 0)
(setq y 0)
(loop i (irange 0 (size numeros) 1)
   ; g est la grille
   (setq g (integer (/ i 25)))
   ; x et y sont les positions dans la grille
   (setq x (integer (/ (- i (* g 25)) 5)))
   (setq y (% i 5))
   ; v est une valeur dans la grille g à la position x,y
   (setq v (@ numeros i))
   ; Si v est déjà dans valeurs, on concatène avec la liste existante
   ; (grille x y i)
   (if (keyn@ valeurs v)
      (set@ valeurs v (cons (list g x y i) (@ valeurs v)))
      (set@ valeurs v (list (list g x y i)))
   )
)

(defun victory(choix)
   (setq g -1)
   (loop grille choix
      (+= g 1)
      (loop ligne grille
         (check (eq (sum ligne) 5)
            (return g)
         )
      )
      ; on prend les colonnes
      (loop colonne (irank@ grille -1)
         (check (eq (sum colonne) 5)
            (return g)
         )
      )
   )
)

; La victoire, c'est lorsqu'une ligne ou une colonne vaut 5
(defun victoire(choix grd)
   (setq g -1)

   ; On vérfie toutes les grilles
   (loop grille choix
      (+= g 1)
      ; Si la grille n'est pas dans grd, c'est qu'elle n'a pas encore été correctement
      ; remplie, des lignes et des colonnes sont vides
      ; On vérifie si la somme des lignes fait 5
      (check (not (in grd g))
         (loop ligne grille
            (check (eq (sum ligne) 5)
               (push grd g)
               (break)
            )
         )
      )

      ; on prend les colonnes même chose
      (check (not (in grd g))
         (loop colonne (irank@ grille -1)
            (check (eq (sum colonne) 5)
               (push grd g)
               (break)
            )
         )
      )
   )
)


(defun compute(nb)
   (setq derniers (integers))
   (loop num tirage
      ; d'abord on marque la valeur trouvée
      (loop nums (@ valeurs num)
         ;nums est donc une position
         ;d'abord on retire cette valeur dans numero
         (set@ numeros (@ nums 3) 0)
         ; puis on rajoute dans les choix: (grille x y)
         (set@ choix (@ nums 0) (@ nums 1) (@ nums 2) 1)
      )

      (victoire choix derniers)
      (check (eq (size derniers) nb)
         (setq g (@ derniers -1))
         (break)
      )
   )
   (setq nums (@@ numeros (* g 25) (+ (* g 25) 25)))
   (println (* num (sum nums)))
)

;35711 / 5586
(compute 1)
; we have already identified the maximum step...
(compute 100)




