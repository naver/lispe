;Date: 01/12/2023
;Author: Claude Roux
;Description: Day 1

(setq données (split (fread (+ _current "data/" "day1.txt")) "\n"))

(println (+ (maplist (\(x) ( (\(l) (integer . join (cons (car l) (last l)) "")) (rgx_findall (rgx "%d") x))) données)))

;;
(setq données `two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
`
)

(setq données (split données "\n"))
;;

(setq nombres  (strings "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defmacro startwith (s i w)
   (if (>= (- (size s) i) (size w))
      (= (@@ s i (+ i (size w))) w)
      false
   )
)

(defun remplace(line)
   (setq v (strings))
   (loop i (enum (split line ""))
      (if (digitp (@ line (@ i 0)))
         (push v (@ i 1))
         (loop u (enum nombres)
            (check (startwith line (@ i 0)  (@ u 1))
               (push v (+ 1 (@ u 0)))
               (break)
            )
         )
      )     
   )
  (integer (+ (car v) (last v)))
)

(+ (maplist 'remplace données))



