(use 'gui)


(setq wnd (fltk_create 100 100 1000 1000 "Polar" 'affiche))
(fltk_end wnd)

(setq image (fltk_create_gif (+ _current "pouces.gif")))

(defun affiche(w o)
   (fltk_gif w image 40 40 500 500)
)

(fltk_run wnd)






















