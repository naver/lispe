;Date: 17/01/2022
;Author: Claude Roux
;Description: Jour 12

(setq initiale (fread (+ _current "codes_day12.txt")))

(setq codes (maplist 'trim (split (trim initiale) "\n")))

(setg paths (dictionary))
(maplist
   (\(x) 
      (setq ch (split x "-")) 
      (setq x (car ch))
      (setq y (cadr ch))
      (if (neq y "start")
         (if (key paths x)
            (push (@ paths x) y)
            (set@ paths x (strings y))
         )
      )
      (if (neq x "start")
         (if (key paths y)
            (push (@ paths y) x)
            (set@ paths y (strings x))
         )
      )
   )
   codes
)

(defun traverse(clef visite alls utilise)
   (if (eq clef "end") 
      (block
         (+= alls 1)
      )
      (loop c (key paths clef)
         (if (lowerp c) 
            (if (in visite c) 
               (if utilise
                  (traverse c visite  alls false)
               )
               (traverse c (insert (clone visite) c)  alls utilise)
            )
            (traverse c visite alls utilise)
         )
      )
   )
)


(setq alls 0)
(traverse "start" (set) alls false)
(println 'part1 alls)

(setq alls 0)
(traverse "start" (set) alls true)
(println 'part2 alls)

