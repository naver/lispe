;Date: 25/07/2022
;Author: Claude Roux
;Description: Execute Basic

; shapes records the dimensions of a given variables
(setq shapes (dictionary))

(defun checkshape (n idx values)
   (if (key shapes n)
      (nconcn (list 'set@@@ (atom n) (list 'quote (@ shapes n))) idx (list values))
      (nconcn '(set@) (atom n) idx (list values))
   )
)

; We use pattern matching to parse the abstract syntax tree

; For an assignment, we can have different types of variables as input
(defpat parsing ( ['assignment $ d] )
   (setq v (caar d))
   (switch (string v)
      ("setdimvariablestring" (checkshape (+ (cadar d) "$") (parsing (caddar d)) (list 'string (parsing (cdr d)))))
      ("setdimvariable" (checkshape (cadar d) (parsing (caddar d)) (list 'number (parsing (cdr d)))))
      ("stringvariable" (list 'setq (parsing (car d)) (list 'string (parsing  (cdr d)))))
      ("variable" (list 'setq (parsing (car d)) (parsing  (cdr d))))
   )
)

; Indexes  10, 20, 30
(defpat parsing (['indexes $ d] )
   (maplist 'parsing d)
)

; a DIM variable definition: DIM TST[10, 20]
(defpat parsing (['dim  n $ d] )
   (setq r (parsing d))
   (ife (eq (size r) 1)
      (list 'setq (atom n) (list 'numbers (nconcn '(to_list) 0 r)))
      ;else we have a list of values
      (set@ shapes n r)
      (list 'setq (atom n) (list 'numbers (list 'to_list 0 (nconcn  '(*) r))))
   )
)

; a DIM string variable definition: DIM TST$[10]
(defpat parsing (['dimstring  n $ d] )
   (setq r (parsing d))
   (ife (eq (size r) 1)
      (list 'setq (atom (+ n "$")) (list 'strings (nconcn '(to_list) "" r)))
      (set@ shapes (+ n "$") r)
      (list 'setq (atom (+ n "$")) (list 'strings (list 'to_list "" (nconcn '(*) r))))
   )
)

; a dim string variable with indexes: v$[10]
(defpat parsing ( ['dimvariablestring n  $ d] )
   (setq r (parsing d))
   (if (eq (size r) 1)
      (list 'string (nconcn '(at) (atom (+ n "$")) r))
      (ncheck (key shapes n)
         (throw (+ "Wrong dimension for: " n))
         (if (eq (size n) (size (key shapes n)))
            (list 'string (nconcn (list 'atshape (atom (+ n "$")) (list 'quote (@ shapes n))) r))
            (nconcn (list 'atshape (atom (+ n "$")) (list 'quote (@ shapes n))) r)
         )
      )
   )
)

; a dim variable with indexes: v$[10]
(defpat parsing ( ['dimvariable n  $ d] )
   (setq r (parsing d))
   (if (eq (size r) 1)
      (nconcn '(at) (atom n) r)
      (ncheck (key shapes n)
         (throw (+ "Wrong dimension for: " n))
         (nconcn (list 'atshape (atom n) (list 'quote (@ shapes n))) r)
      )
   )
)

; A list of data: data 10, 20, 30 enddata
(defpat parsing ( ['data $ d] )
   (list 'quote (maplist 'parsing d false))   
)

; a simple variable
(defpat parsing ( ['variable d] )
   (atom d)
)

; a string variable
(defpat parsing ( ['stringvariable $ d] )
   (atom (+ (car d) "$"))
)

; a negative numerical value
(defpat parsing ( ['minus d] )
   (* -1 (parsing d))
)

; a numerical value
(defpat parsing ( ['anumber d] )
   (number d)
)

; a string value: "value"
(defpat parsing ( ['string d] )
   d
)

; the comparison operator
; It can be made out of two pieces: == or <> or <= or >=
(defpat parsing ( ['comparator $ d] )
   (setq a (join (maplist 'parsing d false) ""))
   (switch a
      ("<>" 'neq)
      ("==" 'eq)
      (true (atom a))
   )
)

; a method implementation: toto.method(...)
(defpat parsing ( ['method n m $ arguments] )
   (setq v (list (atom m) (parsing n)))
   (ncheck arguments
      v
      (nconcn v (maplist 'parsing arguments false))
   )
)

; a function call: call(x,...)
(defpat parsing ( ['call n $ arguments] )
   (if (consp n)
      (cons (parsing n) (maplist 'parsing arguments false))
      (if arguments
         (consb (atom n) (maplist 'parsing arguments false))
         (list (atom n))
      )
   )
)

; a function definition
(defpat parsing ( ['function  name parameters  $ code] )
   (setq code (maplist 'parsing code false))   
   (nconcn (list 'defun (atom name) (map 'parsing (cdr parameters))) code)
)

; lambda
(defpat parsing (['lambda parameters code] )
   (setq code (maplist 'parsing (cdr code) false))   
   (nconcn (list 'lambda (map 'parsing (cdr parameters))) code)
)

; forin: For A in range(1,10,1)... EndFor
(defpat parsing ( ['forin init rg $ code] )
   (setq code (maplist 'parsing code))
   (nconcn (list 'loop (parsing init) (parsing rg)) code)
)

;for: For A = 0, A < 10 , A = A + 1... EndFor
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
; While x < 10 ... EndWhile
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

; Then...
(defpat parsing ( ['then $ code])
   (if (eq 1 (size code))
      (parsing code)
      (consb 'block (maplist 'parsing code false))
   )
)

; Else...
(defpat parsing ( ['else $ code])
   (if (eq 1 (size code))
      (parsing code)
      (consb 'block (maplist 'parsing code false))
   )
)

; if A < 10 Then ... Else ... EndIf
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

; calculus expression: A + 10 - 30
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

; a comparison: A < 10 or B > 10 and C <> 8
(defpat parsing ( ['comparison a1 op a2 $ d] )
   (setq op (parsing op))
   (if (eq op 'in)
      (setq res (list op (parsing a2) (parsing a1)))
      (setq res (list op (parsing a1) (parsing a2)))
   )
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

; print "Toto", 10, A
(defpat parsing ( ['multiop x $ arguments] )
   (consb (atom x) (maplist 'parsing arguments false))
)

; (A + 20)
(defpat parsing ( ['parenthetic $ code] )
   (parsing code)
)

; a simple atom, might be a variable
(defpat parsing ( [ [atom_ x] $ d] )
   (parsing d)
)

; argument is a list, we analyse x and d one after the other
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
   (setq tree (abstract_tree code))

   ; __root__ is a special function that defines the top block of a LispE program
   (setq code '(__root__))

   (ife (in (car tree) "Error")
      (setq code (list 'println (join tree " ")))
      (loop line (cdr tree)
         (setq c (parsing line))
         (push code c)
      )
   )
   code
)


