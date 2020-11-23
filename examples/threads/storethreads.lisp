


(dethread call(i)
   (setq l (range 1 i 1))
   (threadstore "here" l)
)


(call 50)
(call 40)
(call 30)
(call 20)
(call 10)
(call 9)
(call 8)
(call 7)
(call 6)
(call 5)

(wait)

(println (threadretrieve "here"))










