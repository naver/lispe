
(use 'gui)

(defun pushed(b o)
   (printerrln "poussé" (fltk_label b))
)

(setq gomme '(0 62 0 128 127 0 192 255 0 224 255 1 240 255 3 248 255 7 252 255 12
      252 127 24 248 15 24 248 7 112 240 3 96 224 0 192 192 0 192 128 1 192 0 3
      224 0 3 240 0 6 120 0 12 60 0 24 30 0 240 7 0 224 3 0 192 0 0 0 0 0 0 0))

(setq image (fltk_create_bitmap gomme 24 24))

(setq wnd (fltk_create 100 100 1000 1000 "Test"))
(setq button (fltk_button wnd 50 50 40 40 "ok" 'pushed FL_IMAGE_BUTTON_TYPE))
(fltk_bitmap button image FL_BLUE)
(fltk_end wnd)

(fltk_run wnd)













