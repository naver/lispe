;Date: 20/10/2022
;Author: Claude Roux
;Description: Haskell functions

(setq r ())
(loop x (irange 1 19 1)
   (pushtrue r
      (andvalue
         (< x 10)
         (+ x x)
      )
   )
)

(println r)


(setq l ())
(loop r (irange 0 10 1)
   (pushtrue l 
       (andvalue
          (eq (% r 2) 0)
          (* 2 r)
       )
    )
)

(println l)
