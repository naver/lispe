
(use 'gui)

(defun pushed(b o)
   (printerrln "poussé" (fltk_label b))
)

(setq wnd (fltk_create 100 100 1000 1000 "Test"))
(setq button (fltk_button wnd 50 50 100 100 "ok" 'pushed))
(fltk_end wnd)

(fltk_run wnd)









