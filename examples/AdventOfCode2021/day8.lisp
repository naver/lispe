
;Date: 07/01/2022
;Author: Claude Roux
;Description: Segments



(setq codes (fread (+ _current "data/codes_day8.txt")))

(setq chiffres {"abcefg":0  "cf": 1 "acdeg":2 "acdfg":3 "bcdf":4 "abdfg":5 "abdefg":6 "acf":7 "abcdefg":8 "abcdfg":9})
(setq par_taille {7:'("abcdefg") 6:'("abcdfg" "abcefg" "abdefg") 4:'("bcdf") 3:'("acf") 5:'("abdfg" "acdeg" "acdfg") 2:'("cf")})

(setq faciles (keyn))
(setq nums (keyn))

(loop c chiffres
   (check (in  '(1 4 7 8) (@ chiffres c))
      (set@ faciles (size c) c)
      (set@ nums (size c) (@ chiffres c))
   )
)

(setq clefs (strings "a" "b" "c" "d" "e" "f" "g"))
(setq lescodes (maplist 'trim (split (trim codes) "\n")))
(setq signals ())
(setq codes ())
(loop m lescodes
   (setq res (split m "|"))
   (push signals (split (@ res 0)))
   (push codes (split (@ res 1)))
)

(setq compte 0)
(loop c codes
   (+= compte (sum (maplist (\(x) (if (keyn@ faciles (size x)) 1 0)) c)))
)

(println 'Facile compte)

(defmacro dans(valeur ref) (eq (&&& ref valeur) ref))

(defmacro srt(x) (join (sort '< (split x "")) ""))

(defun traverse (signaux lecode)
   (setq un (car (filterlist (\(x) (eq (size x) 2))  signaux)))
   (setq sept (car (filterlist (\(x) (eq (size x) 3))  signaux)))
   (setq huit (car (filterlist (\(x) (eq (size x) 7))  signaux)))
   (setq quatre (car (filterlist (\(x) (eq (size x) 4))  signaux)))

   (setq combinaisons (key))

   (set@ combinaisons (srt un) "1")
   (set@ combinaisons (srt sept) "7")
   (set@ combinaisons (srt huit) "8")
   (set@ combinaisons (srt quatre) "4")

   (setq cinqs (filterlist (\(x) (eq (size x) 5))  signaux))
   (setq sixs (filterlist (\(x) (eq (size x) 6))  signaux))

   (loop c sixs
      (if (not (dans c sept))
         (block
            (set@ combinaisons (srt c) "6")
            (setq six c)
         )
         (if (dans c quatre)
            (set@ combinaisons (srt c) "9")
            (set@ combinaisons (srt c) "0")
         )
      )      
   )

   (loop c cinqs
      (if (dans c sept)
         (set@ combinaisons (srt c) "3")
         (if (dans six c)
            (set@ combinaisons (srt c) "5")
            (set@ combinaisons (srt c) "2")
         )
      )      
   )
   (setq res "")
   (loop c lecode
      (+= res (@ combinaisons (srt c)))
   )
   (integer res)
)

(setq resultat (integers))
(lloop (ccc code) signals codes
   (push resultat (traverse ccc code))
)

(println 'Compliqué (sum resultat))







