# Exemple de code Lisp
(trace true)

(println
   (eval
      (list 'cons ''a  ''(e b c))
   )
)

# Une fonction pour faire la somme de deux éléments
(defun somme (x y)
   (setg globaltoto 1000)
   (+ x y)
)

(println (somme 10 20))
(println (somme "a" "b"))

(println "Global:" globaltoto)

# Une fonction pour multiplier deux éléments
(defun multi (x y)
   (* x  y)
)

(println (multi 10 20))

(defun cconc (x)
   (cons
      'R
      (cons x '(b c))
   )
)

(println (cconc '(e f)))

(println (cconc 'a))

(setq val 20)

(println
   (while (> val 10)
      (if (eq val 12)
         (return 'test)
         (setq val
            (- val 1)
         )
      )
   )
)

(println
   (while (> val 10)
      (if (eq val 12)
         (return 'hélas)
         (setq val (- val 1))
      )
   )
)


(defun boucle(x)
   (if (eq x ())
      "fin"
      (if (eq (car x) 10)
         'erreur_de_valeur
         (boucle (cdr x))
      )
   )
)

(println (boucle '(a b c d e)))
(println (boucle '(a b 10 d e)))


