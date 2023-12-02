;Date: 02/12/2023
;Author: Claude Roux
;Description: Day2 2023 Advent of Code


(setq données (fread (+ _current "data/day2.txt")))

(setq données (split données "\n"))

(setq parties
   (mapcar
      (\(line)
         (setq v (split (@@ line ":" -) ";"))
         (mapcar  (\(x) (split (trim x) ", ")) v)
      )
      données
   )
)

(setq definition {"red":12 "green":13 "blue":14})

(setq resultat 0)
(loop i (enum parties)
   (setq res 
      (maplist 
         (\(e) 
            (maplist 
               (\(x) 
                  (setq r (split x " "))
                  (< (@ definition (@ r 1)) (integer (@ r 0)))
               )
               e
            )
         )
         (@ i 1)
      )
   )
   (println res)
   (setq tst (sum (, res)))
   (if (not tst)
      (+= resultat (+ 1 (@ i 0)))
   )
)

(println resultat)

(setq resultat 0)
(loop i (enum parties)
   (setq res {"blue":[] "red":[] "green":[]})
   (maplist 
      (\(e) 
         (maplist 
            (\(x) 
               (setq r (split x " "))
               (push (@ res (@ r 1)) (integer (@ r 0)))
            )
            e
         )
      )
      (@ i 1)
   )
   (set@ res "blue" (max (@ res "blue")))
   (set@ res "red" (max (@ res "red")))
   (set@ res "green" (max (@ res "green")))
   (+= resultat (* (values@ res)))
)

