;Date: 18/12/2024
;Author: Claude Roux
;Description: Advent of code 2024 day 6

(setq m_clear (+ (chr 27) "[2J"))
(setq m_clear_scrolling (+ (chr 27) "[3J"))
(setq m_home  (+ (chr 27) "[H"))

(defun clear_screen()
   (print m_clear)
   (print m_clear_scrolling)
   (print m_home)
)



(setq enigma (fread (+ _current "data/day6.txt")))

(setq carte (split enigma "\n"))

(loop i (enum carte)
   (setq y (find (@ i 1) "^"))
   (if y
      (break)
   )
)

(setq lx (size carte))
(setq ly (size (@ carte 0)))

(setq original (maplist (\(x) (split x "")) carte))
(setq carte (maplist (\(x) (split x "")) carte))

(setq x (@ i 0))

(setq moves {"^": '(-1 0) "v":'(1 0) "<":'(0 -1) ">":'(0 1)})
(setq tourne {"^": ">"  ">":"v" "v":"<" "<":"^"})

(defmacro valide(x y)
   (and 
      (>= x 0)
      (>= y 0)
      (< x lx)
      (< y ly)
   )
)

(defmacro vérifie(x y)
   (and 
      (valide x y)
      (!= (@ carte x y) "#")
   )
)

(defun affiche(c)
   (clear_screen)
   (sleep 40)
   (print (join (maplist (\(x) (join x "")) c) "\n"))
)

(setq dedans true)
(setq initx x)
(setq inity y)

(while dedans
   (setq caret (@ carte x y))
   (if (key moves caret)
      (setq actions (@ moves caret))
   )
   (setq xx (+ x (@ actions 0)))
   (setq yy (+ y (@ actions 1)))
   (ncheck (vérifie xx yy)
      (ncheck (valide xx yy)
         (setq dedans false)
         (set@ carte x y (@ tourne caret))
      )
      (set@ carte x y "X")
      (set@ carte xx yy caret)
      (setq x xx)
      (setq y yy)
   )
)

(println (+ 1 (+ (maplist (\(x) (count x "X")) carte))))

(setq nbloop 0)
(setq carte original)
(setq
   temps
   (elapse
      (loop o_r (irange 0 lx 1)
         (loop o_c (irange 0 ly 1)
            (ncheck (!= (@ carte o_r o_c) "#")
               (setq r initx)
               (setq c inity)
               (setq vue (seti))
               (setq d 0)
               (while true
                  (setq clef (+ r (* 130 c) (* 16900 (+ d 1))))
                  (check (find vue clef)
                     (+= nbloop 1)
                     (break)
                  )
                  (insert vue clef)
                  (setq action (@ '((-1 0) (0 1) (1 0) (0 -1)) d))
                  (setq rr (+ r (@ action 0)))
                  (setq cc (+ c (@ action 1)))
                  (if (nullp (valide rr cc))
                     (break)
                  )
                  (if (or 
                        (= (@ carte rr cc) "#")
                        (and
                           (eq rr o_r)
                           (eq cc o_c)
                        )
                     )
                     (setq d (% (+ d 1) 4))
                     (block
                        (setq r rr)
                        (setq c cc)
                     )
                  )
               )
            )
         )
      )      
   )
)
(println nbloop temps)

