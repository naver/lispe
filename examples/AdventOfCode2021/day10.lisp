
;Date: 14/01/2022
;Author: Claude Roux
;Description: Syntax Problems


; codes_complets_syntaxe.txt 
; codes_syntaxe_simples.txt

(setq codes (fread (+ _current "data/codes_day10.txt")))
(setq codes (maplist (\(x) (split x "")) (maplist 'trim (split (trim codes) "\n"))))

(setq equivalence {"(":")" "{":"}" "[":"]" "<":">"})
(setq score {")": 3 "]":57 "}":1197 ">":25137})
(setq score_comp  {")": 1 "]":2 "}":3 ">":4})


(setq erreurs (strings))
(setq ajouts ())
(loop c codes
   (setq ok true)
   (setq chaine c)
   (setq consomme (strings))
   (maplist 
      (\(ref)
         (if (key equivalence ref)
            (push consomme ref)
            (ncheck (neq (@ equivalence (last consomme)) ref)
               (pop consomme)
               (push erreurs ref)
               (setq ok false)
               (break)
            )
         )
      )
      chaine
   )

   (check ok
      (setq ajout (maplist (\(x) (@ equivalence x)) (reverse consomme)))
      (push ajouts ajout)
   )
)

(println 'part1 (+ (maplist (\(x) (@ score x)) erreurs)))

(defun compte (comp)
   (setq sc 0)
   (loop c comp
      (*= sc 5)
      (+= sc (@ score_comp c))
   )
   sc
)

(setq res (sort '< (maplist 'compte ajouts)))
(println 'part2 (@ res (/ (size res) 2)))



