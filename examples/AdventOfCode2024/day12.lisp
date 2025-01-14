;Date: 10/01/2025
;Author: Claude Roux
;Description: Advent of Code 2024 day 12


(setq m . maplist '(split _ "") . split (fread (+ _current "data/day12.txt")) "\n")

(setq sz . size m)

(setq veg . unique . flatten m)

(defun compte(a c)
   (setq nb 0)
   (setq x (@ a 0))
   (setq y (@ a 1))
   (setq xm (- x 1))
   (setq ym (- y 1))
   (setq xp (+ x 1))
   (setq yp (+ y 1))

   (if (zerop x)
      (+= nb 1)
      (if (!= (@ m xm y)  c)
         (+= nb 1)
      )
   )
   (if (zerop y)
      (+= nb 1)
      (if (!= (@ m x ym)  c)
         (+= nb 1)
      )
   )

   (if (= xp sz)
      (+= nb 1)
      (if (!= (@ m xp y)  c)
         (+= nb 1)
      )
   )
   (if (= yp sz)
      (+= nb 1)
      (if (!= (@ m x yp)  c)
         (+= nb 1)
      )
   )

   nb
)

(defun vérif(a b)
   (loop e (- a b)
      (if (or 
            (< e -1)
            (> e 1)
         )
         (return false)
      )
   )
   true
)

(defun fusionne(interm)
   (loop i (irange 0 (size interm) 1)
      (loop j (irange 0 (size interm) 1)
         (check (neq i j)
            (setq found false)
            (loop e1 (@ interm i)
               (loop e2 (@ interm j)
                  (check (vérif e1 e2)
                     (setq found true)
                     (break)
                  )
               )
               (if found (break))
            )
            (check found
               (nconc (@ interm i) (@ interm j))
               (set@ interm j ())
            )
         )
      )
   )
   (filterlist (\(x) (not . nullp x)) interm)
)

(setq dico (dictionary))

(loop e veg
   (setq l ())
   (loop c (enum m)
      (setq r (findall (@ c 1) e))
      (check r
         (setq o (zipwith 'list (replicate (size r) (@c 0)) r))
         (nconc l o)
      )
   )

   (setq interm ())
   (setq courant (car l))
   (setq ligne (list courant))
   (loop n (cdr l)
      (ife 
         (and
            (= (@ courant 0) (@ n 0))
            (= (@ courant 1) (- (@ n 1) 1))
         )
         (push ligne n)
         (push interm ligne)
         (setq ligne (list n))
      )
      (setq courant n)
   )

   (if ligne
      (push interm ligne)
   )

   (setq interm (fusionne interm))
   (setq interm (fusionne interm))
   (set@ dico e interm)
)


(setq total 0)
(loop c veg
   (loop i (@ dico c)
      (setq nb 0)
      (loop e i
         (+= nb (compte e c))
      )
      (+= total (* (size i) nb))
   )
)

(println total)

