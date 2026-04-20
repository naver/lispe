;Date: 25/07/2022
;Author: Claude Roux
;Description: Execute Basic

; shapes records the dimensions of a given variables

(defpat parsing( ['lispexp lispe_code])
   (car . to_list lispe_code)
)

(defpat parsing( ['returning x $ v])
   (if v
      (list (atom x) (parsing v))
      (list (atom x)))
)

(defpat parsing ( ['tojoin sep cnt])
   (list 'join (parsing cnt) (parsing sep))
)

(defpat parsing ( ['walrus nm $ d] )
   (list 'setqv (atom nm) (parsing d)))

(defpat parsing ( ['assignmenttopself $ d] )
   (setq letype (cadar d))
   (setq op (atom (join (cdr (@ d 1)))))
   (setq value (@ d 2))
   (if (consp letype)
      (nconcn (list op) (atom (@ letype 1)) (parsing (@ letype 2)) (parsing value))
      (list op (atom letype) (parsing value))))

(defpat parsing ( ['assignmentop $ d] )
   (setq letype (cadar d))
   (setq op (atom (join (cdr (@ d 1)))))
   (setq value (@ d 2))
   (if (consp letype)
      (nconcn (list op) (atom (@ letype 1)) (parsing (@ letype 2)) (parsing value))
      (list op (atom letype) (parsing value))))


(defpat container_assignment([ ['variable $ vari] value] asgn)
   (setq dimvar (car vari))
   (setq nm (@ dimvar 1))
   (setq indexes ())
   (setq letype nil)
   (loop i (cddr dimvar)
      (if (consp . car i)
         (setq letype (caar i))
         (setq letype 'indexes)
      )
      (nconc indexes (parsing i)))
   (if (eq letype 'indexes)
      (nconcn '(set@) (atom nm) indexes (list . parsing value))
      (nconcn '(set@@) (atom nm) indexes (list . parsing value))))

(defpat container_assignment([['multiassignment ['variables $ vari]] value] asgn)
   (list asgn (mapcar 'atom vari) (parsing value))
)

(defpat parsing ( ['parameters])
   ()
)

(defpat parsing ( ['parameters ['variables $ v]] )
   (mapcar 'atom v)
)


(defpat parsing ( ['assignmentself $ d] )
   (setq letype (cadar d))
   (setq value (cadr d))
   (if (consp letype)
      (container_assignment d 'setq) 
      (list 'setqi (atom letype) (parsing value))))

(defpat parsing ( ['assignment $ d] )
   (setq letype (cadar d))
   (setq value (cadr d))
   (if (consp letype)
      (container_assignment d 'setq) 
      (list 'setq (atom letype) (parsing value))))

(defpat parsing ( ['assignmentg $ d] )
   (setq letype (cadar d))
   (setq value (cadr d))
   (if (consp letype)
      (container_assignment d 'setg)
      (list 'setg (atom letype) (parsing value))))

; Indexes  10, 20, 30
(defpat parsing( ['interval "[" ":" "]"])
   '(0 0))

(defpat parsing( ['interval "[" n ":" "]"])
   (list (parsing n) 0))

(defpat parsing( ['interval "[" ":" n "]"])
   (list 0 (parsing n)))

(defpat parsing( ['interval "[" d1 ":" d2 "]"])
   (list (parsing d1) (parsing d2)))

(defpat parsing (['indexes $ d] )
   (mapcar 'parsing d)
)

(defpat parsing (['valuelist $ d] )
   (nconcn '(list) (mapcar 'parsing d))
)
(defpat parsing (['valuedictionary $ d] )
   (setq args '(dictionary))
   (loop a d
      (push args (parsing (@ a 1)) (parsing (@ a 2))))
   args
)

(defpat get_action(l)
   (setq d {"interval":'extract "indexes":'at})
   (if (atomp . car l)
      (@ d (car l))
      (@ d (caar l))))

(defpat container_at( ['dimvariable varname $ lst])
   (setq r ())
   (setq last nil)
   (loop l lst
      (setq idx (parsing l))
      (setq action (get_action l))
      (ncheck (not r)
         (setq r (nconcn (list action (atom varname)) idx))
         (if (and (= action 'at) (= action (car r)))
            (nconc r idx)
            (setq r (nconcn (list action r) idx))))
   )
   r
)

; a simple variable
(defpat parsing ( ['variable d] )
   (if (consp d)
      (container_at d)
      (atom d)))

; a negative numerical value
(defpat parsing ( ['minus d] )
   (* -1 (parsing d))
)

; a numerical value
(defpat parsing ( ['anumber d] )
   (number d)
)

(defpat parsing ( ['f_ $ d] )
   (list 'f_ (parsing d))
)

; a string value: "value"
(defpat parsing ( ['string d] )
   d
)

; a string value: "value"
(defpat parsing ( ['longstring d] )
   (aslongstring d)
)

; the comparison operator
; It can be made out of two pieces: == or <> or <= or >=
(defpat parsing ( ['comparator $ d] )
   (setq a (join (maplist 'parsing d false) ""))
   (switch a
      ("<>" '!=)
      ("==" '=)
      (true (atom a))
   )
)


(defpat parsing ( ['catching kw "(" nm ")" $ inst])
   (list (list 'lambda (list (atom nm)) (parsing inst)) )
)

(defpat parsing ( ['catching kw $ inst])
   (list (list 'lambda (list 'e_) (parsing inst)) )
)

(defpat parsing(['amaybe $ ct])
   (nconcn '(maybe) (mapcar 'parsing (extract ct 0 -1)) (parsing (last@ ct)))
)

; Indexes  10, 20, 30
(defpat parsing( ['callinterval "[" ":" "]"])
   '(extract 0 0))

(defpat parsing( ['callinterval "[" n ":" "]"])
   (list 'extract (parsing n) 0))

(defpat parsing( ['callinterval "[" ":" n "]"])
   (list 'extract 0 (parsing n)))

(defpat parsing( ['callinterval "[" d1 ":" d2 "]"])
   (list 'extract (parsing d1) (parsing d2)))

(defpat parsing( ['callindexes $ args])
   (if args
      (nconcn (list 'at) (mapcar 'parsing args))
      (list 'at))
)

(defpat parsing( ['callmethod nm $ args])
   (if args
      (nconcn (list (atom nm)) (mapcar 'parsing args))
      (list (atom nm)))
)

; a method implementation: toto.method(...)
(defpat parsing ( ['method n $ mts] )
   (setq v (parsing n))
   (setq calls (mapcar 'parsing mts))
   (loop a calls
      (ife (eq (size a) 1)
         (if (not . consp v)
            (setq v (nconcn a (list v)))
            (setq v (nconcn a '. v)))
         (ife (and (consp v)  (eq (car a) (car v)) (eq (car a) 'at))
            (nconc v (cdr a))
            (insert a v 1)
            (setq v a))))
   v
)

; a function call: call(x,...)
(defpat parsing ( ['call n "(" ")" $ mts] )
   (if (consp n)
      (setq v (cons (parsing n) (maplist 'parsing arguments false)))
      (setq v (list (atom n)))
   )
   (setq calls (mapcar 'parsing mts))
   (loop a calls
      (ife (eq (size a) 1)
         (if (not . consp v)
            (setq v (nconcn a (list v)))
            (setq v (nconcn a '. v)))
         (ife (and (consp v)  (eq (car a) (car v)) (eq (car a) 'at))
            (nconc v (cdr a))
            (insert a v 1)
            (setq v a))))
   v
)

(defpat parsing ( ['call n "(" arguments ")" $ mts] )
   (if (consp n)
      (setq v (cons (parsing n) (maplist 'parsing arguments false)))
      (setq v (consb (atom n) (maplist 'parsing (cdr arguments) false)))
   )
   (setq calls (mapcar 'parsing mts))
   (loop a calls
      (ife (eq (size a) 1)
         (if (not . consp v)
            (setq v (nconcn a (list v)))
            (setq v (nconcn a '. v)))
         (ife (and (consp v)  (eq (car a) (car v)) (eq (car a) 'at))
            (nconc v (cdr a))
            (insert a v 1)
            (setq v a))))
   v
)


(defpat parsing ( ['acase chk exp])
   (list (parsing chk) (parsing exp))
)

(defpat parsing ( ['switch vari $ cs])
   (nconcn '(switch) (parsing vari) (mapcar 'parsing cs))
)

; a function definition
(defpat parsing ( ['classedef nm $ code] )
   (setq code (maplist 'parsing code false))   
   (setq fnd false)
   (setq methods ())
   (loop c code
      (if (eq (@ c 1) '__init__)
         (setq fnd (@ c 2))
         (push methods (list 'defpat (atom (@@ (string . @ c 1) 0 -1)) (cons (list (atom (+ nm "_")) 'r) (@ c 2)) (list 'r (cons (@ c 1) (@ c 2)))))))
   (println methods)   
   (ncheck fnd
      (setq r (nconcn (list 'class@ (atom (+ nm "_")) '()) code))
      (setq r (nconcn (list 'class@ (atom (+ nm "_")) '(init)) code))
      (setq initclass (list 'defun (atom nm) fnd (list (atom (+ nm "_")) (cons '__init__ fnd))))
      (setq r (list r initclass)))
   (nconcn r methods)
)

; a function definition
(defpat parsing ( ['classfunc nm parameters $ code] )
   (setq code (maplist 'parsing code false))   
   (if (neq nm "__init__")
      (+= nm "_"))
   (nconcn (list 'defun (atom nm) (mapcar 'atom (cdr parameters))) code)
)

; a function definition
(defpat parsing ( ['jsfunc nm parameters $ code] )
   (setq code (maplist 'parsing code false))   
   (setq p (mapcar 'atom (cdr parameters)))
   (setq frst (car p))
   (setq initialis (list 'setq frst (list 'json_parse '. 'atob frst)))
   (nconcn (list 'defun (atom nm) p) (nconcn (list initialis) code))
)

; a function definition
(defpat parsing ( ['function nm parameters $ code] )
   (setq code (maplist 'parsing code false))   
   (nconcn (list 'defun (atom nm) (parsing parameters)) code)
)

; a thread definition
(defpat parsing ( ['athread nm parameters $ code] )
   (setq code (maplist 'parsing code false))   
   (nconcn (list 'dethread (atom nm) (parsing parameters)) code)
)

; lambda
(defpat parsing (['lambda parameters code] )
   (setq code (maplist 'parsing (cdr code) false))   
   (nconcn (list 'lambda (mapcar 'atom (cdr parameters))) code)
)

(defpat parsing(['tail p])
   (list '$ (parsing p))
)

(defpat parsing (['patterndictionary $ d] )
   (setq args ())
   (push args '{)
   (loop a d
      (if (eq (car a) 'tail)
         (nconc args '$ (parsing (@ a 1)))
         (push args (parsing (@ a 1)) ': (parsing (@ a 2)))))
   (push args '})
   args
)

(defpat parsing(['patternlist $ pats])
   (setq pr ())
   (loop p pats
      (setq v (parsing p))
      (if (and (consp v) (eq (car v) '$))
         (nconc pr v)
         (push pr v)
      )
   )
   pr
)

(defpat parsing ( ['patterns $ pats])
   (setq pr ())
   (loop p pats
      (setq v (parsing p))
      (if (and (consp v) (eq (car v) '{))
         (nconc pr v)
         (push pr v)
      )
   )
   pr
)

(defpat parsing ( ['patternsjs wrd $ pats])
   (setq pr ())
   (push pr (atom wrd))
   (loop p pats
      (setq v (parsing p))
      (if (and (consp v) (eq (car v) '{))
         (nconc pr v)
         (push pr v)
      )
   )
   pr
)

(defpat parsing ( ['jspatternrule nm parameters $ code] )
   (setq code (maplist 'parsing code false))   
   (setq p (parsing parameters))
   (setq frst (car p))
   (setq initialis (list 'setq frst (list 'json_parse '. 'atob frst)))
   (nconcn (list 'defpat (atom nm) p) (nconcn (list initialis) code))
)

; a function definition
(defpat parsing ( ['patternrule  nm parameters  $ code] )
   (setq code (maplist 'parsing code false))   
   (nconcn (list 'defpat (atom nm) (parsing parameters)) code)
)

(defpat parsing ( ['jsrule nm parameters $ code] )
   (setq code (maplist 'parsing code false))   
   (setq p (parsing parameters))
   (setq frst (car p))
   (setq initialis (list 'setq frst (list 'json_parse '. 'atob frst)))
   (nconcn (list 'defpred (atom nm) p) (nconcn (list initialis) code))
)

; a function definition
(defpat parsing ( ['rule  nm parameters  $ code] )
   (setq code (maplist 'parsing code false))   
   (nconcn (list 'defpred (atom nm) (parsing parameters)) code)
)


(defpat parsing (['comprehension $ d])
   (setq vari (atom (@ d 1)))
   (ife (= (size d) 3)
      (nconcn (list 'mapcar (list 'lambda (list vari) (parsing (@ d 0))) (parsing (@ d 2))))
      (nconcn 
         (list 'mapcar 
            (list 'lambda (list vari) (parsing (@ d 0))) 
            (list 'filtercar (list 'lambda (list vari) (parsing (@ d 3))) (parsing (@ d 2))))))
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

(defpat parsing ( ['elif test then $ else] )
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
   (check (consp (car ope))
      (setq ope (list (join (car ope) ""))))
   (ncheck ope
      (car args)
      (setq r (list (atom (car ope)) (car args) (cadr args)))
      (setq args (cddr args))
      (loop o (cdr ope)
         (setq o (if (consp o) (atom . join o "") (atom o)))
         (if (eq o (car r))
            (push r (car args))
            ; Operator priority
            (ife (in '(+ - & |) o)
               (setq r (list o r (car args)))
               (setq las (last@ r))
               (if (and (consp las) (eq o (car las)))
                  (set@ r -1 (nconcn las (car args)))
                  (set@ r -1 (list o las (car args))))
            )
         )
         (setq args (cdr args))
      )
      r
   )
)

; a comparison: A < 10 or B > 10 and C <> 8
(defpat parsing ( ['comparing "not" a1 ['comparator $ op] a2 $ d] )
   (setq op (parsing (nconcn '(comparator) op)))
   (if (eq op 'in)
      (setq res (list 'not (list 'in_it (parsing a2) (parsing a1))))
      (setq res (list 'not (list op (parsing a1) (parsing a2))))
   )
   (check d
      (setq orand (atom (parsing (car d))))
      (setq reste (parsing (cadr d)))
      (if (and (consp reste) (eq (car reste) orand))
         (setq res (nconcn (list orand res) (cdr reste)))
         (setq res (list orand res reste))
      )
   )
   res
)

(defpat parsing ( ['comparing a1 ['comparator $ op] a2 $ d] )
   (setq op (parsing (nconcn '(comparator) op)))
   (if (eq op 'in)
      (setq res (list 'in_it (parsing a2) (parsing a1)))
      (setq res (list op (parsing a1) (parsing a2)))
   )
   (check d
      (setq orand (atom (parsing (car d))))
      (setq reste (parsing (cadr d)))
      (if (and (consp reste) (eq (car reste) orand))
         (setq res (nconcn (list orand res) (cdr reste)))
         (setq res (list orand res reste))
      )
   )
   res
)

(defpat parsing ( ['comparing "not" a $ d] )
   (setq res (list 'not (parsing a)))
   (check d
      (setq orand (atom (parsing (car d))))
      (setq reste (parsing (cadr d)))
      (if (and (consp reste) (eq (car reste) orand))
         (setq res (nconcn (list orand res) (cdr reste)))
         (setq res (list orand res reste))
      )
   )
   res
)

(defpat parsing ( ['comparing a $ d] )
   (setq res (parsing a))
   (check d
      (setq orand (atom (parsing (car d))))
      (setq reste (parsing (cadr d)))
      (if (and (consp reste) (eq (car reste) orand))
         (setq res (nconcn (list orand res) (cdr reste)))
         (setq res (list orand res reste))
      )
   )
   res
)

; a comparison: A < 10 or B > 10 and C <> 8
(defpat parsing ( ['comparingmax "not" a1 ['comparator $ op] a2 $ d] )
   (setq op (parsing (nconcn '(comparator) op)))
   (if (eq op 'in)
      (setq res (list 'not (list 'in_it (parsing a2) (parsing a1))))
      (setq res (list 'not (list op (parsing a1) (parsing a2))))
   )
   (check d
      (setq orand (atom (parsing (car d))))
      (setq reste (parsing (cadr d)))
      (if (and (consp reste) (eq (car reste) orand))
         (setq res (nconcn (list orand res) (cdr reste)))
         (setq res (list orand res reste))
      )
   )
   res
)

(defpat parsing ( ['comparingmax a1 ['comparator $ op] a2 $ d] )
   (setq op (parsing (nconcn '(comparator) op)))
   (if (eq op 'in)
      (setq res (list 'in_it (parsing a2) (parsing a1)))
      (setq res (list op (parsing a1) (parsing a2)))
   )
   (check d
      (setq orand (atom (parsing (car d))))
      (setq reste (parsing (cadr d)))
      (if (and (consp reste) (eq (car reste) orand))
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
         (if (and (consp . car c) (eq (caar c) 'class@))
            (nconc code c)
	      (push code c))
      )
   )
   code
)

(defun compile(code)
   (setg pythonmode false)
   (setq lsp (transpile code))
   (ncheck (= (@ lsp 0) '__root__)
      (+ (replace (@ lsp 1) "> > >" ":\n>> ") " <<")
      (setq r (@@ (trim . prettify (cdr lsp) 100) 1 -1))
      (if (= (@ r 0) "\n")
         (join (maplist (\(x) (@@ x 3 0)) (split r "\n")) "\n")
         r)))

; ------------------------------------------------------------------------
; This is the part where BasAIc can be a kind of Python
; ------------------------------------------------------------------------

(defmacro espace(x) (size (takelist (\(c) (or (= c "\t") (= c " "))) x)))
(defmacro inc(i) (+= (@ i 0) 1))
(defmacro dec(i) (-= (@ i 0) 1))
(defmacro labl(ligne) (trim (@@ (trim  ligne) 0 " "))) 

; We skip unimportant lines
; comments and empty lines
(defun skip(code i)
   (setq ligne (@ code (car i)))
   (setq stripped (trim ligne))
   (while 
      (or  
         (= stripped "")
         (= (upper . @@ stripped 0 3) "REM")
         (= (@ stripped 0) "#")
      )
      (inc i)
      (ncheck (< (@ i 0) (size code))
         (block
            (setq ligne "")
            (break)
         )
         (setq ligne (@ code (car i)))
         (setq stripped (trim ligne))
      )
   )
   (if (not . in ligne "#")
      ligne
      (if (in (tokenize_rules parser_tok ligne) "#")
         (@@ ligne 0 "#")
         ligne)))

; We inject the closing tags
(defun insert_label (code i root labels ref)
   (while (< (car i) (size code))
      (setq ligne (skip code i))
      (setq clr (trim ligne))
      (setq cpt (espace ligne))
      (inc i)
      (ife (and 
               (eq cpt ref) 
               (eq (@ clr -1) ":")
               (or (eq (@@ clr 0 4) "else")
                   (eq (@@ clr 0 6) "except")
                   (eq (@@ clr 0 5) "catch")
                   (eq (@@ clr 0 4) "elif")))
         (push labels (@@ (trimright ligne) 0 -1))

         (check (and ref (<= cpt ref))
            (dec i)
            (return)
         )
         (ncheck (eq (@ clr -1) ":")
            (if cpt
               (push labels ligne)
               (push root ligne)
            )
            (setq r (list (@@ (trimright ligne) 0 -1)))
            (if cpt
               (push labels r)
               (push root r)
            )
            (setq v ())
            (insert_label code i root v cpt)
            (if (in clr " ")
               (setq closing_tag (+ "end" (@@ clr 0 " ")))
               (setq closing_tag (+ "end" (@@ clr 0 ":"))))

            (if (eq closing_tag "end")
               (throw (+ "Error: check indentation at line: " (trim ligne) "(" (@ i 0) ")")))
            (push r v (+ (fill " " cpt)  closing_tag))
         )
      )
   )
)

; The main injection function
(defun injecte_labels(code)
   (setq code (split (trim code) "\n"))
   (setq labels ())
   (insert_label code '(0) labels labels 0)
   (join (flatten labels) "\n"))

; We call this specific function to inject closing tags
; We then transform our Python into Lisp
(defun compilepython(code)
   (setg pythonmode true)
   (setq code (injecte_labels code))
   (setq lsp (transpile code))
   (ncheck (= (@ lsp 0) '__root__)
      (+ (replace (@ lsp 1) "> > >" ":\n>> ") " <<")
      (setq r (@@ (trim . prettify (cdr lsp) 100) 1 -1))
      (if (= (@ r 0) "\n")
         (join (maplist (\(x) (@@ x 3 0)) (split r "\n")) "\n")
         r)))


