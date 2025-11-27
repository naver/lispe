;Date: 08/03/2024
;Author: Claude Roux
;Description: Permutations

(setq m {})
(setq A ())
(loop i (range 1 10 1)
   (loop j (range 1 10 1)
      (loop k (range 1 10 1)
         (setq ak (+ "A" i k))
         (setq bk (+ "B" k j))
         (if (key@ m ak)
            (push (@ m ak) bk)
            (set@ m ak (list bk))
         )
      )
   )
)

(println m)



