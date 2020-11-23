

(dethread call(s i)
   (loop e (range 0 i 1)
      (+= s (string e))
   )
   (println s)
)



(call "l:" 10)
(call "x:" 20)
(call "y:" 30)

(wait)




