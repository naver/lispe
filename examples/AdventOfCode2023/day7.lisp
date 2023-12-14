;Date: 12/12/2023
;Author: Claude Roux
;Description: Day 7 of Advent of Code 2023



(setq entrées `32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
`
)

(setq letters '(2 3 4 5 6 7 8 9 T J Q K A))

(setq force {})
(loop i (enum  letters 2)
   (set@ force (@ i 1) (convert_in_base (@ i 0) 16))
)

(setq entrées (fread (+ _current "data/day7.txt")))
(setq données (maplist '(split _ " ") (split entrées "\n")))

(defun modif(x)
   (setq v (maplist (\(x) (@ force x)) (split x "")))
   (setq nb (sort '> (filterlist (\(x) x) (maplist '(count x) letters))))
   (setq id
      (switch (size nb)
         (5 0)
         (4 1)
         (3 (car nb))
         (2 (+ 1 (car nb)))
         (1 6)
      )
   )
   (list (join (cons id v) "") x)
)

(setq données (maplist (\(x) (list (modif (car x)) (integer (cadr x)))) données))
(sort (\(x y)  (< (caar x) (caar y))) données)
(setq r 0)
(loop u (enum données 1)
   (+= r (* (car u) (cadadr u)))
)
(println r)


; letters are in a new order
(setq letters '(J 2 3 4 5 6 7 8 9 T Q K A))

; Their force are then different
(setq force {})
(loop i (enum  letters 1)
   (set@ force (@ i 1) (convert_in_base (@ i 0) 16))
)

(defun modif2(x)
   (setq v (maplist (\(x) (@ force x)) (split x "")))
   (setq nb (sort '> (filterlist (\(x) x) (maplist '(count x) letters))))
   (setq nb (integer (join nb "")))
   (setq nbj (count x "J"))
   (setq id
      (if nbj
         (switch nb
            (11111 "1_") ; <=> 2111
            (2111   "3_") ; <=> 311
            (221    (if (== nbj 1) "4_" "5_")); <=>  32 or 41
            (311     "5_") ; <=> 41
            (32      "6_" ) ; <=> 5
            (41     "6_") ; <=> 5
            (5 "6_")
         )
         (switch nb
            (11111 "0_")
            (2111   "1_")
            (221    "2_")
            (311    "3_")
            (32      "4_")
            (41     "5_")
            (5 "6_")
         )
      )      
   )

   (list (join (cons id v) "") x)
)

(setq données (maplist '(split _ " ") (split entrées "\n")))
(setq données (maplist (\(x) (list (modif2 (car x)) (integer (cadr x)))) données))
(sort (\(x y)  (< (caar x) (caar y))) données)
(setq r 0)
(loop u (enum données 1)
   (+= r (* (car u) (cadadr u)))
)
(println r) 


