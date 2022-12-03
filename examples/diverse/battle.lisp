(setq colors '(SPADE CLUB HEART DIAMOND))
(setq cardvalues '(2 3 4 5 6 7 8 9 10 Jack Queen King Ace))

(setq stackofcards ())
; Notre stackofcards contient toutes les cards combinant couleur et valeur
(loop c colors
   (loop v cardvalues
      (push stackofcards (list v c)
      )
   )
)

; On compare deux cards entre elles
(defun compare(card1 card2)
   (setq v1 (find cardvalues (car card1)))
   (setq v2 (find cardvalues (car card2)))
   (cond
      (
         (< v1 v2)
         -1
      )
      (
         (> v1 v2)
         1
      )
      (true 0)
   )
)

; On mélange le stackofcards de cards
(shuffle stackofcards)

; On partage le stackofcards mélangé en deux
(setq player1 (extract stackofcards 0 26))
(setq player2 (extract stackofcards 26 (size stackofcards)))
(setq tour 1)

; On code: -1 player 2 gagne, 1 player 1 gagne, sinon Bataille
(setq dico {-1:'Player2 1:'Player1 0:'Battle})

; On joue tant que les deux players ont des cards
(while (and (size player1) (size player2))
   (setq card1 (last player1))
   (setq card2 (last player2))
   (pop player1)
   (pop player2)
   (setq comp (compare card1 card2))
   (println "Round:" tour
      "Card Player1:"
      card1
      "Card Player2:"
      card2
      "Winner:"
      (@ dico comp)
      (size player1)
      (size player2)
      (+ (size player1) (size player2) 2)
   )
   (+= tour 1)
   (cond
      ((eq comp -1)
         (insert player2 card1 0)
         (insert player2 card2 0)
      )
      ((eq comp 1)
         (insert player1 card2 0)
         (insert player1 card1 0)
      )
      (true (println "Battle")
         (setq the_heap (list card1 card2))
         (while (and (size player1) (size player2))
            (setq card1 (last player1))
            (setq card2 (last player2))
            (pop player1)
            (pop player2)
            (setq comp (compare card1 card2))
            (println "--> Card Player1:"
               card1
               "Card Player2:"
               card2
               "Winner:"
               (@ dico comp)
               (size player1)
               (size player2)
               (+ (size player1) (size player2) (size the_heap) 2)
            )
            (cond
               ((eq comp -1)
                  (push the_heap card1)
                  (push the_heap card2)
                  (shuffle the_heap)
                  (loop c the_heap (insert player2 c 0))
                  (println "--> Stack:" the_heap)
                  (break)
               )
               ((eq comp 1)
                  (push the_heap card1)
                  (push the_heap card2)
                  (shuffle the_heap)
                  (loop c the_heap (insert player1 c 0))
                  (println "--> Stack:" the_heap)
                  (break)
               )
               (true
                  (push the_heap card1)
                  (push the_heap card2)
                  (cond
                     ((eq (size player1) 0)
                        (println "Player 1 stops")
                        (loop c the_heap (insert player2 c 0))
                        (break)
                     )
                     ((eq (size player2) 0)
                        (println "Player 2 stops")
                        (loop c the_heap (insert player1 c 0))
                        (break)
                     )
                  )
               )
            )
         )
      )
   )
)

(println)
(println)
(println "Player 1" player1)
(println)
(println "Player 2" player2)









