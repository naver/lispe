;Date: 06/12/2023
;Author: Claude Roux
;Description: Day 5

(setq données `seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4

`
)

(setq données (fread (+ _current "data/day5.txt")))

(setq seeds (integers (split (@@ données ":" "\n") " ")))
(setq pos (popfirst (findall données ":")))

(setq r (maplist (\(x) (maplist (\(u) (integers (split u " "))) (split (@@ données (+ x 1) "\n\n") "\n"))) pos))

(defun calcul(seeds)
   (setq v (integers))
   (loop s seeds
      (loop mp r
         (loop b mp
            (check (<= (@ b 1) s (+ (@ b 1) (@ b 2)))
               (setq s (+ (@ b 0) (- s (@ b 1))))
               (break)
            )
         )
      )
      (push v s)
   )
   (min v)
)

(println (calcul seeds))

(setq s_eeds (slice seeds 2))
(setq seeds (maplist (\(x) (list (@ x 0) (+ x))) s_eeds))

(defun extraire(db mp)
   (setq res ())
   (setq s (@ db 0))
   (setq f (@ db 1))
   (setq sz (- f s))
   (loop b mp
      ; The trick is to split the list whenever the testing value is in the middle
      (check (< s (@ b 1) f)
         (nconc res (extraire (integers s (- (@ b 1) 1)) mp))
         (nconc res (extraire (integers (@ b 1) f) mp))
         (return res)
      )
      (setq sm (+ (@ b 1) (@ b 2)))         
      ; otherwise it is as the previous solution
      (check (<= (@ b 1) s sm)
         (setq s (+ (@ b 0) (- s (@ b 1))))
         (check (<= f sm)
            (setq f (+ s sz)) 
            (push res (integers s f))
            (return res)
         )         
         ; with a twist when f is too large. We then split again
         ; The new limit is the maximum value to which s can belong
         (push res (integers s (+ (@ b 0) (@ b 2))))
         (setq s (+ sm 1))
         ; f stays the same
         (check (<= s f)
            (nconc res (extraire (integers s f) mp))
            (return res)
         )
      )
   )
   (push res (integers s f))
   res
)

(loop mp r
   (setq res ())
   (loop db seeds
      (nconc res (extraire db mp))
   )
   (setq seeds (unique res))
)

(println (min (flatten seeds)))

