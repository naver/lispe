;Date: 27/12/2024
;Author: Claude Roux
;Description: Advent of Code 2024 day 9


(setq codes (integers (split (trim (fread (+ _current "data/day9_example.txt"))) "")))

(setq déplie ())

(setq blocs (slice codes 2))

(loop b (enum blocs)
   (push déplie (replicate (@ b 1 0) (@ b 0)))
   (maybe
      (push déplie (replicate (@ b 1 1) "."))
      (setq déplie (flatten déplie))
   )
)

(setq p (find déplie "."))
(loop c (enum (reverse déplie))
   (println c)
   (set@ déplie p (@ c 1))
   (set@ déplie (@ c 0) ".")
   (println déplie)
   (+= p 1)
)



