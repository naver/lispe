;Date: 20/02/2023
;Author: Claude Roux
;Description: Simple Copy

(setq rep (ls (+ _current "../../src")))

(defun traite (fichier)
   (println fichier)
   (setq corps (fread fichier))
   (setq corps (replace corps "::eval(" "::_eval("))
   (setq corps (replace corps "::EVAL(" "::eval("))
   (setq fichier (@@ fichier -"/" 0))
   (fwrite (+ _current "../src/" fichier) corps)
)

(defun boucle (fichiers)
   (loop f fichiers
      (check 
         (and
            (not (in f "old"))
            (not (in f "DS"))
            (not (in f "editor"))
         )         
         (traite f)         
      )
   )
)


(boucle rep)







