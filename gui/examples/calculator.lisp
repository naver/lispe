(use 'gui)

(setq wnd (fltk_create 100 100 300 300 "Calculator"))

(setq n1 (fltk_input wnd 50 60 150 20 "N1:" false))
(setq n2 (fltk_input wnd 50 100 150 20 "N2:" false))
(setq n3 (fltk_input wnd 50 140 150 20 "N3:" false))
(setq n4 (fltk_input wnd 50 180 150 20 "N4:" false))
(setq mem (fltk_input wnd 50 20 150 20 "Mem:" false))

(setq result (fltk_input wnd 60 230 200 20 "Result:" false))
(setq bplus (fltk_button wnd 230 20 30 30 "+" 'plus))
(setq bminus (fltk_button wnd 230 60 30 30 "-" 'moins))
(setq bmultiply (fltk_button wnd 230 100 30 30 "*" 'multiplie))
(setq bdivide (fltk_button wnd 230 140 30 30 "/" 'divise))
(setq bgarde (fltk_button wnd 230 180 50 30 "mem" 'memory))
(fltk_end wnd)

(defun memory(b o)
   (fltk_value mem (fltk_value result))
)

(defun plus(b o)
   (fltk_value result
      (trim0
         (+
            (number (fltk_value n1))
            (number (fltk_value n2))
            (number (fltk_value n3))
            (number (fltk_value n4))
            (number (fltk_value mem))
         )
      )
   )
)

(defun moins(b o)
   (fltk_value result
      (trim0
         (-
            (number (fltk_value n1))
            (number (fltk_value n2))
            (number (fltk_value n3))
            (number (fltk_value n4))
            (number (fltk_value mem))
         )
      )
   )
)

(defun multiplie(b o)
   (setq v1 1)
   (setq v2 1)
   (setq v3 1)
   (setq v4 1)
   (setq v5 1)
   (setq v (fltk_value n1))
   (if (neq v "") (setq v1 (number v)))
   (setq v (fltk_value n2))
   (if (neq v "") (setq v2 (number v)))
   (setq v (fltk_value n3))
   (if (neq v "") (setq v3 (number v)))
   (setq v (fltk_value n4))
   (if (neq v "") (setq v4 (number v)))
   (setq v (fltk_value mem))
   (if (neq v "") (setq v5 (number v)))
   (fltk_value result
      (trim0 (* v1 v2 v3 v4))
   )
)

(defun divise(b o)
   (setq v1 1)
   (setq v2 1)
   (setq v3 1)
   (setq v4 1)
   (setq v5 1)
   (setq v (fltk_value n1))
   (if (neq v "") (setq v1 (number v)))
   (setq v (fltk_value n2))
   (if (neq v "") (setq v2 (number v)))
   (setq v (fltk_value n3))
   (if (neq v "") (setq v3 (number v)))
   (setq v (fltk_value n4))
   (if (neq v "") (setq v4 (number v)))
   (setq v (fltk_value mem))
   (if (neq v "") (setq v5 (number v)))
   (fltk_value result
     (trim0 (/ v1 v2 v3 v4))
   )
)

(fltk_focus wnd)
(fltk_run wnd)




