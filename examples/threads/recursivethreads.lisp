(dethread call(n s i l)
   (if (zerop i)
      (threadstore "here" s)
      (block
         (loop e (range 0 i 1)
            (+= s (string e))
         )
         (push l s)
         (println n s i l)
         (call n s (- i 1) l)
      )
   )
)


(setq l '("First"))
(call 'T1 "x:" 10 l)
(call 'T2 "y:" 10 l)
(call 'T3 "z:" 10 l)

(wait)

(println (threadretrieve "here"))
(threadclear "here")

















