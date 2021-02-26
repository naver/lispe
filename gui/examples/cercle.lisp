(use 'gui)

(defmacro ++(x i inc) (at x i (+ (at x i) inc)))
(defmacro --(x i inc) (at x i (- (at x i) inc)))

(setq wnd (fltk_create 100 100 1000 1000 "Cercle" 'appel '(30.0 40.0 20.0)))

(setq sense true)

(defun appel(w o)
   (fltk_circle w (at o 0) (at o 1) (at o 2) FL_BLUE)
   (fltk_circle w (at o 1) (at o 0) (at o 2) FL_RED)
   (check sense
      (++ o 0 1)
      (++ o 1 1)
      (++ o 2 1)
      (if (> (at o 2) 500)
         (setg sense nil)
      )
      (return 0.01)
   )
   (-- o 0 1)
   (-- o 1 1)
   (-- o 2 1)
   (if (< (at o 2) 10)
      (setg sense true)
   )
   (return 0.01)
)

(fltk_end wnd 0.01)
(fltk_run wnd)











