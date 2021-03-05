(defmacro ++(x i inc) (at x i (+ (at x i) inc)))
(defmacro --(x i inc) (at x i (- (at x i) inc)))

(use 'gui)

(setq maxx 100)
(setq maxy 900)
(setq sense true)

(setq incb 0)

(defun slide(s o)
   (setg incb (fltk_value s))
)

(setq wnd (fltk_create 100 100 1000 1000 "Polar" 'appel '(0.0 0.0)))
(setq incb_slider (fltk_slider wnd 30 900 200 20 "incb" FL_HOR_SLIDER true 'slide))
(fltk_boundaries incb_slider -100 100)
(fltk_end wnd 0.01)

(setq valeurs (range 0 (* 2 _pi) 0.005))

(defun equation(o a)
   (+  (cos (* o (at a 0))) (at a 1))
)

(defun appel(w o)
   (at o 1 incb)
   (if sense
      (block
         (++ o 0 2)
         (if (> (at o 0) maxx)
            (setg sense nil)
         )
      )
      (block
         (-- o 0 2)
         (if (< (at o 0) 3)
            (setg sense true)
         )
      )
   )

   (setq coords ())

   (loop x valeurs
      (setq v (equation x o))
      (push coords
         (list
            (* (cos x) v)
            (* (sin x) v)
         )
      )
   )
   (fltk_drawcolor w FL_RED)
   (fltk_plot w coords 0)
   (return 0.01)
)

(fltk_run wnd)


















