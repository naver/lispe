;Date: 01/12/2023
;Author: Claude Roux
;Description: Day 1

(setq données (split (fread (+ _current "data/" "day1.txt")) "\n"))

(println (+ (maplist (\(x) ( (\(l) (integer . join (cons (car l) (last l)) "")) (rgx_findall (rgx "%d") x))) données)))


(setq données `two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
`)

(setq données (split données "\n"))

(setq words "(one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety")


(setq equivalence {
      "one" : 1
      "two" : 2
      "three":3
      "four":4
      "five":5
      "six":6
      "seven":7
      "eight":8
      "nine":9
      "ten":10
      "eleven":11
      "twelve":12
      "thirteen":13
      "fourteen":14
      "fifteen":15
      "sixteen":16
      "seventeen":17
      "eighteen":18
      "nineteen":19
      "twenty":20
      "thirty":30
      "forty":40
      "fifty":50
      "sixty":60
      "seventy":70
      "eighty":80
      "ninety":90
      "hundred":100
   }
)

(defun remplace(line)
   (loop w (prgx_findall (prgx words) line)
      (setq line (replace line w (@ equivalence w)))
   )
   line
)


(+ (maplist (\(x) ( (\(l) (integer . join (cons (car l) (last l)) "")) (rgx_findall (rgx "%d") (remplace x)))) données))



