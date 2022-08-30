;Date: 25/07/2022
;Author: Claude Roux
;Description: Execute Basic

(load (+ _current "basic.lisp"))

(defpat parsing ( ['assignment $ d] )
   (setq v (caar d))
   (switch (string v)
      ("setdimvariablestring" (list 'set@ (atom (+ (cadar d) "$")) (parsing (caddar d)) (list 'string (parsing (cdr d)))))
      ("setdimvariable" (list 'set@ (atom (cadar d)) (parsing (caddar d)) (list 'number (parsing (cdr d)))))
      ("stringvariable" (list 'setq (parsing (car d)) (list 'string (parsing  (cdr d)))))
      ("variable" (list 'setq (parsing (car d)) (parsing  (cdr d))))
   )
)

(defpat parsing (['dim  n $ d] )
   (list 'setq (atom n) (list 'numbers (list 'to_list 0 (parsing d))))
)

(defpat parsing (['dimstring  n $ d] )
   (list 'setq (atom (+ n "$")) (list 'strings (list 'to_list "" (parsing d))))
)

(defpat parsing ( ['dimvariablestring n  $ d] )
   (list 'string (list 'at (atom (+ n "$")) (parsing d)))
)

(defpat parsing ( ['dimvariable n  $ d] )
   (list 'at (atom n) (parsing d))
)

(defpat parsing ( ['variable d] )
   (atom d)
)

(defpat parsing ( ['stringvariable $ d] )
   (atom (+ (car d) "$"))
)


(defpat parsing ( ['minus d] )
   (* -1 (parsing d))
)

(defpat parsing ( ['anumber d] )
   (number d)
)

(defpat parsing ( ['string d] )
   d
)

(defpat parsing ( ['comparator $ d] )
   (setq a (join (maplist 'parsing d false) ""))
   (switch a
      ("<>" 'neq)
      ("==" 'eq)
      (true (atom a))
   )
)

(defpat parsing ( ['method n m $ arguments] )
   (setq v (list (atom m) (parsing n)))
   (ncheck arguments
      v
      (nconcn v (maplist 'parsing arguments false))
   )
)

(defpat parsing ( ['call n $ arguments] )
   (if arguments
      (consb (atom n) (maplist 'parsing arguments false))
      (list (atom n))
   )
)

(defpat parsing ( ['function  name parameters  $ code] )
   (setq code (maplist 'parsing code false))   
   (nconcn (list 'defun (atom name) (map 'parsing (cdr parameters))) code)
)


(defpat parsing ( ['forin init rg $ code] )
   (setq code (maplist 'parsing code))
   (nconcn (list 'loop (parsing init) (parsing rg)) code)
)

(defpat parsing ( ['for init test inc $ code] )
   (setq code (maplist 'parsing code))
   (list 
      'block
      (parsing init)
      (nconc
         (list 
            'while
            (parsing test)
         )
         code
         (list (parsing inc))
      )
   )
)

(defpat parsing ( ['while test $ code] )
   (setq code (maplist 'parsing code))
   (nconc
      (list 
         'while
         (parsing test)
      )
      code
   )
)

(defpat parsing ( ['then $ code])
   (if (eq 1 (size code))
      (parsing code)
      (consb 'block (maplist 'parsing code false))
   )
)

(defpat parsing ( ['else $ code])
   (if (eq 1 (size code))
      (parsing code)
      (consb 'block (maplist 'parsing code false))
   )
)

(defpat parsing ( ['if test then $ else] )
   (setq test (parsing test))
   (setq then (parsing then))
   (ife else
      (block
         (setq else (parsing (car else)))
         (if
            (and
               (consp else)
               (eq (car else) 'block)
            )
            (nconcn (list 'ife test then) (cdr else))
            (list 'if test then else)
         )
      )
      (if
         (and
            (consp then)
            (eq (car then) 'block)
         )
         (nconcn (list 'check test) (cdr then))
         (list 'if test then)
      )
   )
)

(defpat parsing ( ['computing $ reste] )
   (setq ope (maplist 'parsing (filterlist (\(x) (eq (car x) 'operator)) reste) false))
   (setq args (maplist 'parsing (filterlist (\(x) (neq (car x) 'operator)) reste) false))
   (ncheck ope
      (car args)
      (setq r (list (atom (car ope)) (car args) (cadr args)))
      (setq args (cddr args))
      (loop o (cdr ope)
         (if (eq (atom o) (car r))
            (nconc r (car args))
            (setq r (list (atom o) r (car args)))
         )
         (setq args (cdr args))
      )
      r
   )
)

; trois éléments à la fois
(defpat parsing ( ['comparison a1 op a2 $ d] )
   (setq res (list (parsing op) (parsing a1) (parsing a2)))
   (check d
      (setq orand (atom (parsing (car d))))
      (setq reste (parsing (cadr d)))
      (if (eq (car reste) orand)
         (setq res (nconcn (list orand res) (cdr reste)))
         (setq res (list orand res reste))
      )
   )
   res
)

(defpat parsing ( ['multiop x $ arguments] )
   (consb (atom x) (maplist 'parsing arguments false))
)

(defpat parsing ( ['parenthetic $ code] )
   (parsing code)
)

(defpat parsing ( [ [atom_ x] $ d] )
   (parsing d)
)

(defpat parsing ( [ x $ d] )
   (setq n (parsing x))
   (setq nn (parsing d))
   (if (neq n ())
      (if (neq nn ())
         (cons n nn)
         n
      )
      nn
   )
)

(defpat parsing ( x )
   x
)

(defpat parsing ( () )
   ()
)

(defun transpile (code)
   ; Transpiling into LispE
   (setq abstract_tree (basic_abstract_tree code))

   ; __root__ is a special function that defines the top block of a LispE program
   (setq code '(__root__))

   (ife (in (car abstract_tree) "Error")
      (setq code (list 'println (join abstract_tree " ")))
      (loop line (cdr abstract_tree)
         (setq c (parsing line))
         (push code c)
      )
   )
   code
)


