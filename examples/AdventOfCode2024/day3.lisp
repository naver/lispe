;Date: 17/12/2024
;Author: Claude Roux
;Description: Advent of Code 2024 day 3




(setq code (fread (+ _current "data/day3.txt")))

(setq instructions (rgx_findall (rgx "mul%(%d+,%d+%)") code))

(println (+ (maplist (\(x) (* (integers (rgx_findall (rgx "%d+") x)))) instructions)))

(setq instructions (rgx_findall (rgx "{[don't%(%)][mul%(%d+,%d+%)][do%(%)]}") code))
(setq result 0)
(setq ajoute true)
(loop s instructions
   (cond
      ((= s "don't()")
         (setq ajoute false)
      )
      ((= s "do()")
         (setq ajoute true)
      )
      (ajoute
         (+= result (* (integers (rgx_findall (rgx "%d+") s))))
      )
   )
)

(println result)