;Date: 09/02/2023
;Author: Claude Roux
;Description: Mandel 2


(use 'gui)

(setq wnd (fltk_create 100 100 1200 1200 "Fractal" 'appel (numbers 40 20 1)))
(fltk_end wnd)

(setq max_iteration 100)
(setq coords {})
(setq points ())


(defun appel(w o)
   (setq width 100)
   (setq height 100)
   (setq clr 1)
   (setq iter 0)

   (loop inc (irange 0.1 2 0.1)
      (loop y (irange  -100 100 inc)
         (loop x (irange -100  100 inc)
            (+= iter 1)
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
                  (set@ coords k true)
                  (push points (numbers x y clr))
               )
            )
         )
      )
      (+= clr 1)
   )

   (setq repère (fltk_plot w points 1))

   (fltk_drawcolor w FL_BLACK)
   (fltk_drawtext w clr 20 20)
   (fltk_drawtext w (size points) 20 40)
   (fltk_drawtext w iter 20 60)
   (fltk_drawtext w repère 20 80)
)

(fltk_run wnd)


