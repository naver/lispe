;Date: 07/02/2023
;Author: Claude Roux
;Description: Complex

(use 'gui)

(setq wnd (fltk_create 100 100 1200 1200 "Fractal" 'appel (numbers 40 20 1)))
(fltk_end wnd 1)

(setq max_iteration 100)
(setq coords {})
(setq points ())


(defun appel(w o)
   (fltk_drawtext w o 20 20)
   (fltk_drawtext w (size points) 20 40)
   (setq width (car o))
   (setq height (cadr o))
   (setq clr (caddr o))

   (loop y (irange (sign height) height 0.5)
      (loop x (irange (sign width)  width 0.5)
         (setq k (+ "" x ":" y) )
         (check (not (key coords k))
            (setq c (+ (/ (* x 2) (- width 1.5))  (* (/ (* y 2) (- height 1)) 0,i)))
            (setq z c)
            (setq iterations 0)
            (while
               (and
                  (< (fabs (real z)) 2)
                  (< iterations max_iteration)
               )
               (setq z (+ (* z z) c))
               (+= iterations 1)
            )
            (check (eq iterations max_iteration)
               (setq e (numbers x y clr))
               (set@ coords k e)
               (push points e)
            )
         )
      )
   )

   (set@ o 0 (+ width 10))
   (set@ o 1 (+ height 10))
   (set@ o 2 (+ clr 1))
   (fltk_plot w points 1)
   1
)

(fltk_run wnd)

