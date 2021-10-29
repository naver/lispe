
;Date: 05/10/2021
;Author: Claude Roux
;Description: Implementing a rudimentary Python interpreter

(setq tok (tokenizer_rules))
(setq txt (fread (+ _current "code.py")))

(setq code (split (trim txt) "\n"))

(defpat modify_method(["def" $ form])
   (list 'quote (list 'defun (atom (@ form 0)) (filterlist (\(x) (neq x ',)) (maplist 'atom (extract form 2 -1)))))
)

(defpat modify_method(["for" "(" $ form])
   (if (> (size form) 4) 
      (+ "(eval '(list " "'loop '" (@ form 0)  " (• '(" (join (cddr form) " ") ")))")
      (list 'quote (list 'loop (atom (@ form 0)) (atom (@ form 2))))
   )
)

(defpat modify_method(["for" $ form])
   (+ "(eval '(list " "'loop '" (@ form 0)  " (• '(" (join (cddr form) " ") "))))")
)

(defpat modify_method(form)
   (+ "(• '("
      (join 
         form
         " "
      )
      "))"
   )
)

(defun formule(form)
   (check (eq (@ form -1) ":")
      (setq form (extract form 0 -1))
      (check 
         (and
            (neq (@ form -1) ")")
            (find form " ")
         )            
         (setq form 
            (insert form "(" (+ 1 (find form " ")))
         )
         (+= form ")")
      )
   )

   (setq dicos (rgx_findall (rgx "%{?*%}") form))
   (loop d dicos
      (setq form (replace form d (replace d "," " ")))
   )

   (setq interval (rgx_findall (rgx "%a+%[?*:?*%]") form))
   (loop i interval
      (setq nom (extract i 0 -"["))
      (setq a1 (extract i -"[" -":"))
      (setq a2 (extract i -":" -"]"))
      (if (neq 1 (size (tokenize_rules tok a1)))
         (setq a1 (+ "(" a1 ")"))
      )
      (if (neq 1 (size (tokenize_rules tok a2)))
         (setq a2 (+ "(" a2 ")"))
      )
      (setq form (replace form i (+ "(extract " nom " " a1 " " a2 ")")))
   )

   (setq interval (rgx_findall (rgx "%a+%[?*%]") form))
   (loop i interval
      (setq nom (extract i 0 -"["))
      (setq a (extract i -"[" -"]"))
      (if (neq 1 (size (tokenize_rules tok a)))
         (setq a (+ "(" a ")"))
      )
      (setq form (replace form i (+  nom " @ " a)))
   )

   (setq lists (rgx_findall (rgx "%[?*%]") form))
   (loop i lists
      (setq r
         (+ 
            "'"
            (join 
               (filterlist 
                  (\(x) (neq x ","))
                  (tokenize_rules tok i)
               )
               ""
            )
         )
      )
      (setq form (replace form i r))
   )

   (setq form
      (filterlist 
         (\(x) (size (trim x))) 
         (tokenize_rules tok form)
      )
   )

   (modify_method form)
)

(defun build(code result ref)
   (while code
      (setq e (@ code 0))
      (setq diff  (- (size e) (size (trimleft e))))
      (setq e (trim e))
      (cond
         ( (eq ref diff)
            (ncheck (= e "else:")
               (block
                  (push result e)
                  (setq code (cdr code))
               )
               (setq sub '(block))
               (push result sub)
               (setq e (@ code 1))
               (setq diff  (- (size e) (size (trimleft e))))
               (setq code (build (cdr code) sub diff))
            )
         )
         ( (< ref diff)
            (if (eq (extract (@ result -1) 0 3) "if ")
               (setq sub (list 'block e))
               (setq sub (list e))
            )
            (push result sub)
            (setq code (build (cdr code) sub diff))
         )
         (true
            (return code)
         )
      )
   )
   code
)

(defpat update( ['= u $ x] )
   (cond
      ( (and (consp u) (eq (car u) '@))
         (set@ u 0 'set@)
         (extend u x)
      )
      ( (and (consp u) (eq (car u) 'extract))
         (set@ u 0 'setrange)
         (extend u x)
      )
      (true
         (extend (list 'setq u) x)
      )
   )
)

(defpat update ( ['print $ x])
   (cons 'println x)
)

(defpat update(f) f)

(defun traverse (root instructions)   
   (check (consp root)
      (if (eq (car root) 'block)
         (block
            (push instructions '(block))
            (traverse (cdr root) (@ instructions -1))
         )
         (loop r root
            (ncheck (consp r)
               (check (size (trim r))
                  (println (formule r))
                  (push instructions (update (eval (formule r))))
               )
               (traverse r (@ instructions -1))
            )
         )
      )
   )
)

(setq root ())
(build code root 0)
(println (prettify root))
(setq ins '(block))
(traverse root ins)
;(println (prettify ins))
;(println (eval ins))










