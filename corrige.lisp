;Date: 02/10/2026
;Author: Claude Roux
;Description: Existence @@ avec 0


(setq fichiers (split (fread (+ _current "lisp_files.txt")) "\n"))


(loop f fichiers
   (setq contenu (fread (+ _current f)))
   (setq pos (findall contenu "(@@ "))
   (setq lst ())
   (loop p pos
      (setq e (@ (getstruct contenu "(" ")" p) 0))
      (check (eq (@@ e -3) " 0)")
         (setq rp e)
         (set@@ rp -3 ")")
         (push lst (strings e rp))
      )
   )
   (check lst
      (loop k lst
         (println f k)
         (setq contenu (replace contenu (@ k 0) (@ k 1)))
      )
      (fwrite (+ _current f) contenu)
   )
)

