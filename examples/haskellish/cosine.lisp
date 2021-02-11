(link "∑" 'sum)
(link "√" 'sqrt)
(defmacro ∏(v vv) (zipwith '* v vv))

; dot product: sum of the product of two vectors
; note that zipwith will apply * to each element of v1 with v2
(defun dot(v1 v2)
   (∑ (∏ v1 v2))
)

; norm: √∑(x²)
(defun norm(v)
   (√ (∑ (map '* v)))
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


(cosine '(1 2 3) '(4 5 6))








