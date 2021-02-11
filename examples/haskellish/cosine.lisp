

; dot product: sum of the product of two vectors
; note that zipwith will apply * to each element of v1 with v2
(defun dot(v1 v2)
   (sum (zipwith '* v1 v2))
)

; norm: √∑(x²)
(defun norm(v)
   (sqrt (sum (map '* v)))
)

; cosine distance: (v1 . v2) / ∏(norm(v1) norm(v2))
(defun cosine(v1 v2)
   (maybe
      (/
         (dot v1 v2)
         (* (norm v1) (norm v2))
      )
      0
   )
)



