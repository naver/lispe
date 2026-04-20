;Date: 26/03/2026
;Author: Claude Roux
;Description: Find closing curl brackets



(setq r (string . fread (+ _current "../templates/app-agents.js")))



(defmacro inc(i) (+= (@ i 0) 1))
(defmacro dec(i) (-= (@ i 0) 1))


(defun check_bracket(code i structure)
   (while (< (@ i 0) (size code))
      (setq ligne (@ code (@ i 0)))
      (setq clr (segment (trim ligne)))
      (inc i)
      (setq nb (count clr "{"))
      (-= nb (count clr "}"))
      (ife (eq nb 1)
         (block
            (setq sub (list ligne))
            (push structure sub)
            (check_bracket code i sub))
         (push structure ligne)
         (if (eq nb -1)
            (return)))
   )
   structure
)


(defun extract_brackets(code)
   (setq code (split (trim . string code) "\n"))
   (setq labels ())
   (check_bracket code '(0) labels)
   (setq result ())
   (setq chaine "")
   (loop e labels
      (if (consp e)
         (block
            (check chaine
               (pushfirst e (trim chaine))
               (setq chaine ""))
            (push result e))      
         (+= chaine e "\n"))
   )
   (if chaine
      (push result chaine))
   result)

