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

