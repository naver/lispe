;Date: 01/12/2023
;Author: Claude Roux
;Description: Day 1

(setq données (split (fread (+ _current "data/" "day1.txt")) "\n"))

(println (+ (maplist (\(x) ( (\(l) (integer . join (cons (car l) (last l)) "")) (rgx_findall (rgx "%d") x))) données)))


(setq words "(one|two|three|four|five|six|seven|eight|nine)")


(setq equivalence {
      "one" : "1"
      "two" : "2"
      "three": "3"
      "four": "4"
      "five": "5"
      "six": "6"
      "seven": "7"
      "eight": "8"
      "nine": "9"
   }
)

(defun remplace(line)
   (setq l line)
   (setq pos 0)
   (loop w (prgx_findall (prgx words) line)
      (setq d (find line w pos))
      (println 1 (@@ line pos d) pos d)
      (println 2 (@ equivalence w))
      (println 3 (@@ line (+ 1 d (size w)) 0))
      (setq line (+ (@@ line pos d) (@ equivalence w) (@@ line (+ 1 d (size w)) 0)))
      (setq pos d)
   )
   (println l line ((\(x) (list (car x) (last x))) (rgx_findall (rgx "%d") line)))
   line
)


(+ (maplist (\(x) ( (\(l) (integer . join (cons (car l) (last l)) "")) (rgx_findall (rgx "%d") (remplace x)))) données))

