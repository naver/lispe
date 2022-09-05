;Date: 22/07/2022
;Author: Claude Roux
;Description: Rule Compiler

; rules.lisp contains the tokenizer modifications to handle a grammar rule
(load (+ _current "rules.lisp"))

; Our grammar is called basic
(setq grammar "basic")

; We read our grammar file
(setq grammaire (fread (+ _current grammar)))

; We first split along cariage returns
(setq grammar_rules (split grammaire "\n"))
; We get rid of lines started with # (comments)
(setq grammar_rules (filterlist (\(x) (neq (@ x 0) "#")) grammar_rules))


; keep is false when the rule starts with a ! 
(setq keep true)

; Each keyword (a string starting with a $) is stored here
(setq keywords ())

; When an element is optional we store its constraints here to create special functions on the fly
(setq functions ())

; the current rule name (head of the rule)
(setq rule_name "")


(defmacro variable(i) (atom (+ "i" i)))
(defmacro results(i) (atom (+ "v" i)))

; The ^ is used as a disjunction operator
; disjunction populates a (or...) structure with the different elements of the rule
(defpat disjunction ( ['^ x $ remainder] idx)
   (cons (transpiling x idx) (disjunction remainder idx))
)

; The list has been consumed
(defpat disjunction ( () idx)
   ()
)

; This is a specific element within the disjunction: [..]^...
; we apply transpiling again
(defpat disjunction (x idx)
   (transpiling x idx)
)

; If the disjunction operator is present
; We populate a (or...) structure with disjunction
(defpat transpiling ( [e '^ $ remainder] idx)
   (nconc (list 'or (transpiling e idx)) (disjunction (consb '^ remainder) idx))
)

; This the regular case
; We populate a (and...) structure with variable initialisations
(defpat transpiling ( [e $ remainder] idx)
   (setq idx1 (+ idx 1))
   (setq init
      (list 'and
         (list 'setq (variable idx1)
            (list 'clone (variable idx))
         )
         (list 'setq (results idx1) ())
      )
   )

   (setq elements ())
   ; We loop among all the elements in the whole structure
   ; We re-build it for this purpose: (cons e remainder)
   (loop x (cons e remainder)
      ; We check if there is one of the Kleene operator: ? + *
      (ncheck (in '(? + *) x)
         ; If it is the case, we store the constraints (in elements)
         ; in functions for later creation
         ; The last element of elements is the one that is covered by the Kleene operator
         ; It is replaced with a call to a function whose name reflects the Kleene operator
         (push elements (transpiling x idx1))
         (switch (string x)
            ("?" (setq nom (atom (+ "O_" rule_name "_" (size functions)))))
            ("*" (setq nom (atom (+ "S_" rule_name "_" (size functions)))))
            ("+" (setq nom (atom (+ "P_" rule_name "_" (size functions)))))
         )
         (push functions (list nom (list (variable idx1) (results idx1) (last elements))))
         (pop elements)
         (push elements (list nom 'tokens (variable idx1) (results idx1)))
      )
   )
   ; We then implement the final stage where the results are pushed into the current result
   (nconc init elements)
   (nconc init  (list (list 'set@ (variable idx) 0 (list 'car (variable idx1)))))
   (nconc init  (list (list 'setq (results idx) (results idx1))))
)

; This one single element, not a list.
; We replaced the ()[] with their code to avoid potential misinterpretation
; We replace these values back with their original values
(defpat transpiling (x idx)
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

   ; % and $ introduce some specific characters, which will compared against the grammar
   ; We introduce a «compare» function, whose body is in this file
   ; This function is used to check if the current element corresponds to these values
   (if
      (or
         (eq (@ val 0) "%")
         (eq (@ val 0) "$")
      )
      (block 
         (check (eq (@ val 0) "$")
            (push keywords (lower (@@ val 1 0)))
         )
         (list 'compare 'tokens (@@ val 1 0) var (results idx) keep)
      )
      ; otherwise it is a call to a function
      (list (atom (+ "C_" val)) 'tokens var (results idx))
   )
)

; Our initial program with the out of grammar function implemenations
(setq the_code `;Date: %2
;Description: Parser for %1 description
;Generated with compiler.lisp

(defun compare (tokens value i v keep)
   (check 
      (and
         (< (car i) (size tokens))      
         (eq (lower (@ tokens (car i))) (lower value))
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

; The body of structure which was followed with *
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

; The body of structure which was followed with +
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

; The body of structure which was followed with ?
(setq opt_func `(defun %1 (tokens %4 vp)
   (setq %3 ())
   (check %2
      (nconc vp %3)
   )
   true
)

`
)

; As we use to_list for a string, we replace the escape ()[] with their code.
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

; Rules can start with a specific command character
; The ° in front of a rule defines the entry point of the grammar
; The ! in front of a rule means that any %x or $xxx will only be tested and will not appear in the final tree.
; The & in front of a rule means that the rule label will be inserted in the final tree if the number of elements is > 1
; The ^ in front of a rule means that the tree item will not contain the rule label 

(setq the_code (format the_code grammar (date)))
(loop r grammar_rules

   (println r)

   ; The most cryptic line of the whole code
   ; We first tokenize then we replace %( %) etc... with their code
   ; Then we join again then apply to_list to this string   
   ; Hence, we are sure that every token will be distinctely separated from the others
   ; In particular, the disjunction operator is isolated with this process
   ; We then apply to_list, which transforms the [...] into sublists.

   (setq a_rule (to_list (join  (protecting (tokenize_rules tok r)) " ")))

   (setq entrypoint false) ; for °
   (setq build true) ; for ^
   (setg keep true) ; for !
   (setq dontskip true) ; for &

   (check (eq (car a_rule) '°)
      (setq entrypoint true)
      (setq a_rule (cdr a_rule))
   )

   (check (eq (car a_rule) '^)
      (setq build false)
      (setq a_rule (cdr a_rule))
   )

   (check (eq (car a_rule) '!)
      (setg keep nil)
      (setq a_rule (cdr a_rule))
   )

   (check (eq (car a_rule) '&)
      (setq dontskip false)
      (setq a_rule (cdr a_rule))
   )

   ; This rule_name, which the head of rule, will be used to create a LispE function with this as a name
   (setq rule_name (car a_rule))
   (setq corps (transpiling (cdddr a_rule) 0))
   (setq corps
      (list 'defun (atom (+ "C_" rule_name)) '(tokens i0 v)
         (list 'check '(< (car i0) (size tokens))
            (list 'setq 'v0 ())
            (list 'if corps
               (if entrypoint
                  (list 'cons (list 'quote rule_name) 'v0)
                  (if build
                     (ife dontskip
                        (list 'push 'v (list 'cons (list 'quote rule_name) 'v0))
                        (list 'if
                           (list 'eq (list 'size 'v0) 1)
                           (list 'nconc 'v 'v0)
                           (list 'push 'v (list 'cons (list 'quote rule_name) 'v0))
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
      (+= the_code (+ ";" r " (entry point)\n"))
      (+= the_code (+ ";" r "\n"))
   )
   (+= the_code (prettify corps 40) "\n")
   ; If there are some functions created for ?+* then we create their body here
   ; Using the above patterns.   
   (check functions
      (loop f functions
         (setq idx (caadr f))
         (setq val (cadadr f))
         (setq tst (caddadr f))
         (setq func "")
         (switch (@ (string (car f)) 0)
            ("O" (setq func (format opt_func (car f) tst val idx)))
            ("P" (setq func (format plus_func (car f) tst val idx)))
            ("S" (setq func (format star_func (car f) tst val idx)))
         )         
         (+= the_code (prettify (car (to_list func))) "\n")
      )
      (setq functions ())
   )
)

(setq nokeyword `
(defun nokeywords(w)
      (not (in  '%1 (lower w)))
)
   `
)

(+= the_code (format nokeyword (unique keywords)))

(setq func   `
(setq parser_tok (tokenizer_rules))

(defun remove_rule (rg base)
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

; Our tokenizer for Basic instructions prevents <> == <= and >= to be one token
; We remove the rules
(remove_rule rg "===0")
(remove_rule rg "<==0")
(remove_rule rg ">==0")
(remove_rule rg "<>=0")

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

; This line creates an entry point  for our grammar transpiling
; In our case, it will implement the function: basic_abstract_tree
(+= the_code (format func grammar))
(fwrite (+ _current (+ grammar ".lisp")) (prettify the_code))

; We check if the code is well-formed LispE program.
(eval the_code)



