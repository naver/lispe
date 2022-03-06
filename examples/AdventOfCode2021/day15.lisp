
;Date: 21/01/2022
;Author: Claude Roux
;Description: Jour15 bis


(setq lacarte (fread (+ _current "data/codes_day15.txt")))
(setq carte (maplist (\(x) (integers (split (trim x) ""))) (split (trim lacarte) "\n")))
(setq dm (rho carte))
(setq dmx (- (@ dm 0) 1))
(setq dmy (- (@ dm 1) 1))

(setq maxval (+ (+ carte)))

(defun metdanstas(tas x)
   (loop i (irange (- (size tas) 1) -1 -1)
      (check (< (car x) (car (@ tas i)))
         (insert tas x i)
         (return)
      )
   )
   (push tas x)
)

(defmacro clef(x y) (+ (string x) "_" y))
(defmacro bornées(x y)
   (and
      (<= 0 x dmx)
      (<= 0 y dmy)
   )
)

(defun voisins(x y)
   (setq cellules ())

   (loop xy '((-1 0) (0 -1) (0 1) (1 0))
      (setq ix (+ x (car xy)))
      (setq iy (+ y (cadr xy)))
      (if (bornées ix iy)
         (push cellules (integers ix iy))
      )
   )
   cellules
)

(defmacro laclef(voisin) (+ (* 1000 (car voisin)) (cadr voisin)))

(defun calcul_risque(carte)
   (setq destination (integers dmx dmy))
   (setq arbre (heap (\(x y) (>=< (car x) (car y)))))
   (insert arbre (list 0 (integers 0 0)))

   (setq risque_cumulé (dictionaryi))
   (setq cellules_non_visitées (seti))
   (loop ix (irange 0 (car dm) 1)
      (loop iy (irange 0 (car dm) 1)
         (push cellules_non_visitées (laclef (integers ix iy)))
         (set@ risque_cumulé (laclef (integers ix iy)) maxval)
      )
   )

   (set@ risque_cumulé (laclef '(0 0)) 0)

   (while (in cellules_non_visitées (laclef destination))
      (setq courant (car arbre))
      (setq courant (cadr courant))
      (popfirst arbre)
      (check (in cellules_non_visitées (laclef courant))
         (setq lesvoisins (voisins (car courant) (cadr courant)))
         (loop voisin lesvoisins
            (check (in cellules_non_visitées (laclef voisin))
               (setq risque
                  (min
                     (@ risque_cumulé (laclef voisin))
                     (+
                        (@ risque_cumulé (laclef courant))
                        (@ carte (cadr voisin) (car voisin))
                     )
                  )
               )
               (set@ risque_cumulé (laclef voisin) risque)
               (setq e (list risque voisin))
               (if (not (in arbre e))
                  (insert arbre e)
               )
            )
         )
         (pop cellules_non_visitées (laclef courant))
      )
   )

   (setg finalement (@ risque_cumulé (laclef (integers dmx dmy))))
   finalement
)

(setq finalement 0)
(println 'part1 (calcul_risque carte))

(defmacro nouveau(c)
   (maplist 
      (\(v) 
         (maplist 
            (\(x) (if (eq x 10) 1 x)) v)) 
      (+ c 1)
   )
)

(defun étend(c cc)
   (lloop (l ll) c cc
      (extend l ll)
   )
)

(setq extensions ())
(setq c1 (nouveau carte))
(setq c2 (nouveau c1))
(setq c3 (nouveau c2))
(setq c4 (nouveau c3))
(setq c5 (nouveau c4))
(setq c6 (nouveau c5))
(setq c7 (nouveau c6))
(setq c8 (nouveau c7))

(push extensions c1 c2 c3 c4 c5 c6 c7 c8)
(loop e (list c1 c2 c3 c4)
   (étend carte e)
)

(loop i (range 0 4 1)
   (setq ext (clone (@ extensions i)))
   (loop n (range (+ i 1) (+ i 5) 1)
      (étend ext (@ extensions n))
   )
   (extend carte ext)
)

(setq dm (rho carte))
(setq dmx (- (@ dm 0) 1))
(setq dmy (- (@ dm 1) 1))

(println 'part2 (elapse (calcul_risque carte)))



