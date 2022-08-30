;Date: 22/07/2022
;Author: Claude Roux
;Description: Rule Compiler


(load (+ _current "rules.lisp"))
(setq grammar "basic")

(setq grammaire (fread (+ _current grammar)))

(setq lst (split grammaire "\n"))
(setq lst (filterlist (\(x) (neq (@ x 0) "#")) lst))
(setq errors (filterlist (\(x) (eq (@ x 0) "*")) lst))
(setq règles (filterlist (\(x) (neq (@ x 0) "*")) lst))

(setq keep true)
(setq keywords ())
(setq fonctions ())
(setq courant "")

(defpat disjunction ( ['^ x $ reste] idx)
   (cons (traversing x idx) (disjunction reste idx))
)

(defpat disjunction ( () idx)
   ()
)

(defpat disjunction (x idx)
   (traversing x idx)
)

(defmacro variable(i) (atom (+ "i" i)))
(defmacro valeurs(i) (atom (+ "v" i)))


(defpat traversing ( [e '^ $ reste] idx)
   (nconc (list 'or (traversing e idx)) (disjunction (consb '^ reste) idx))
)

(defpat traversing ( [e $ reste] idx)
   (setq idx1 (+ idx 1))
   (setq init
      (list 'and
         (list 'setq (variable idx1)
            (list 'clone (variable idx))
         )
         (list 'setq (valeurs idx1) ())
      )
   )

   (setq elements ())
   (loop x (cons e reste)
      (ncheck (in '(? + *) x)
         (push elements (traversing x idx1))
         (switch (string x)
            ("?" (setq nom (atom (+ "O_" courant "_" (size fonctions)))))
            ("*" (setq nom (atom (+ "S_" courant "_" (size fonctions)))))
            ("+" (setq nom (atom (+ "P_" courant "_" (size fonctions)))))
         )
         (push fonctions (list nom (list (variable idx1) (valeurs idx1) (last elements))))
         (pop elements)
         (push elements (list nom 'tokens (variable idx1) (valeurs idx1)))
      )
   )
   (nconc init elements)
   (nconc init  (list (list 'set@ (variable idx) 0 (list 'car (variable idx1)))))
   (nconc init  (list (list 'setq (valeurs idx) (valeurs idx1))))
)

(defpat traversing (x idx)
   (setq var (variable idx))
   (setq val (string x))
   (setq val
      (switch val
         ("%40" "%(")
         ("%41" "%)")
         ("%91" "%[")
         ("%92" "%]")
         (true val)
      )
   )

   (if
      (or
         (eq (@ val 0) "%")
         (eq (@ val 0) "$")
      )
      (block 
         (check (eq (@ val 0) "$")
            (push keywords (@@ val 1 0))
         )
         (list 'compare 'tokens (@@ val 1 0) var (valeurs idx) keep)
      )
      (list (atom (+ "C_" val)) 'tokens var (valeurs idx))
   )
)

(defun protecting (r)
   (map
      (\(x)
         (switch x
            ("%(" "%40")
            ("%)" "%41")
            ("%[" "%91")
            ("%]" "%92")
            (true x)
         )
      )
      r
   )
)


(setq lecode `;Date: %2
;Description: Parser for %1 description
;Generated with LispE Transpiler

(defun compare (tokens value i v keep)
   (check 
      (and
         (< (car i) (size tokens))      
         (eq (@ tokens (car i)) value)
      )
      (+= i 1)
      (if keep
         (push v value)
      )
      (return true)
   )
)

(defun C_Word (tokens i v)
   (check (< (car i) (size tokens))
      (setq w (@ tokens (car i)))
      (check 
         (and
            (rgx "%a{%a%d_}*" w)
            (nokeywords w)
         )
         (+= i 1)
         (push v w)
         (return true)
      )
   )
)

(defun C_astring(tokens i v)
   (check (< (car i) (size tokens))
      (setq m (@ tokens (car i)))
      (check (and
            (eq (@ m 0) "\"")
            (eq (@ m -1) "\"")
         )
         (+= i 1)
         (push v (list 'string (@@ m 1 -1)))
         (return true)
      )
   )
)


(defun C_anumber (tokens i v)
   (check (< (car i) (size tokens))
      (setq w (@ tokens (car i)))
      (check (rgx "({-%+})%d+(.%d+({eE}(-)%d+))" w)
         (+= i 1)
         (push v (list 'anumber (float w)))
         (return true)
      )
   )
)

`
)

(setq star_func `(defun %1 (tokens %4 vp)
   (setq v ())
   (while %2
      (nconc v %3)
   )
   (check v
      (nconc vp v)
   )
   true
)

`
)

(setq plus_func `(defun %1 (tokens %4 vp)
   (setq v ())
   (setq %3 ())
   (check %2
      (push v %3)
      (setq %3 ())
      (while %2
         (nconc v %3)
         (setq %3 ())
      )
   )
   (if v
      (nconc vp v)       
   )
)

   `
)

(setq opt_func `(defun %1 (tokens %4 vp)
   (setq %3 ())
   (check %2
      (nconc vp %3)
   )
   true
)

`
)

(setq lecode (format lecode grammar (date)))
(loop r règles
   (setq règle (to_list (join  (protecting (tokenize_rules tok r)) " ")))

   (setq entrypoint false)
   (setq build true)
   (setg keep true)
   (setq dontskip true)

   (check (eq (car règle) '°)
      (setq entrypoint true)
      (setq règle (cdr règle))
   )

   (println règle)
   (check (eq (car règle) '&)
      (setq dontskip false)
      (setq règle (cdr règle))
   )

   (check (eq (car règle) '^)
      (setq build false)
      (setq règle (cdr règle))
   )

   (check (eq (car règle) '!)
      (setg keep nil)
      (setq règle (cdr règle))
   )

   (setq courant (car règle))
   (setq corps (traversing (cdddr règle) 0))
   (setq corps
      (list 'defun (atom (+ "C_" courant)) '(tokens i0 v)
         (list 'check '(< (car i0) (size tokens))
            (list 'setq 'v0 ())
            (list 'if corps
               (if entrypoint
                  (list 'cons (list 'quote courant) 'v0)
                  (if build
                     (ife dontskip
                        (list 'push 'v (list 'cons (list 'quote courant) 'v0))
                        (list 'if
                           (list 'eq (list 'size 'v0) 1)
                           (list 'nconc 'v 'v0)
                           (list 'push 'v (list 'cons (list 'quote courant) 'v0))
                        )
                     )
                     (list 'nconc 'v 'v0)
                  )
               )
            )
         )
      )
   )
   (if entrypoint
      (+= lecode (+ ";" r " (entry point)\n"))
      (+= lecode (+ ";" r "\n"))
   )
   (+= lecode (prettify corps 40) "\n")
   (check fonctions
      (loop f fonctions
         (setq idx (caadr f))
         (setq val (cadadr f))
         (setq tst (caddadr f))
         (setq func "")
         (switch (@ (string (car f)) 0)
            ("O" (setq func (format opt_func (car f) tst val idx)))
            ("P" (setq func (format plus_func (car f) tst val idx)))
            ("S" (setq func (format star_func (car f) tst val idx)))
         )         
         (+= lecode (prettify (car (to_list func))) "\n")
      )
      (setq fonctions ())
   )
)

(setq nokeyword `
(defun nokeywords(w)
      (not (in  '%1 w))
)
   `
)

(+= lecode (format nokeyword (unique keywords)))

(setq func   `
(setq parser_tok (tokenizer_rules))

(defun retire (rg base)
    (loop i (range 0 (size rg) 1)
       (setq x (@ rg i))
       (check (in x base)
          (pop rg i)
          (break)
      )
   )
)

(setq rg (get_tokenizer_rules parser_tok))

(set@ rg 0 " +=#")
(set@ rg 1 "\t+=#")
(set@ rg 2 "\n+=#")

(retire rg "===0")
(retire rg "<==0")
(retire rg ">==0")
(retire rg "<>=0")

(set_tokenizer_rules parser_tok rg)

(defun %1_abstract_tree (code)
   (setq tokens (tokenize_rules parser_tok code))
   (setq i '(0))
   (setq res (C_analyse tokens i ()))
   (if (< (car i) (size tokens))
      (cons "Error from: " (@@ tokens (car i) (+ 20 (car i))))
      res
   )
)

`
)

(+= lecode (format func grammar))
(fwrite (+ _current (+ grammar ".lisp")) (prettify lecode))

(eval lecode)



