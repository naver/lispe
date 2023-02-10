;Date: 2023/02/03 15:14:51
;Description: Parser for basicois description
;Generated with compiler.lisp

(defun C_any(tokens i v)
   (check (< (car i) (size tokens))
      (setq w (@ tokens (car i)))
      (check (neq w ";") 
         (+= i 1)
         (push v w)
         (return true)
      )
   )
)

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
            (rgx "{%a%h}{%a%h%d_}*" w)
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

;°analyse := [function^expressions]+ (entry point)
(defun C_analyse (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (P_analyse_0 tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (cons 'analyse v0)
      )
   )
)

(defun P_analyse_0 (tokens i1 vp)
   (setq v ())
   (setq v1 ())
   (check (or
         (C_function tokens i1 v1)
         (C_expressions tokens i1 v1)
      )
      (push v v1)
      (setq v1 ())
      (while (or
            (C_function tokens i1 v1)
            (C_expressions tokens i1 v1)
         )
         (nconc v v1)
         (setq v1 ())
      )
   )
   (if v
      (nconc vp v)
   )
)

(link "affiche" 'print)
(link "afficheln" 'println)
(link "renvoie" 'return)
(link "interval" 'range)
(link "empile" 'push)
(link "dépile" 'pop)
(link "taille" 'size)
(link "liref" 'fread)
(link "écriref" 'fwrite)
(link "entiers" 'integers)
(link "réels" 'numbers)
(link "chaines" 'strings)
(link "entier" 'integer)
(link "réel" 'number)
(link "chaine" 'string)
(link "extraire" 'extract)
(link "dans" 'in)
(link "ou" 'or)
(link "et" 'and)
(link "xou" 'xor)
;operator := [%< %<]^[%> %>]^[%^ %^]^[%* %*]^%&^%|^%+^%-^%*^%/^%%^%^
(defun C_operator (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (or
            (and
               (setq i1 (clone i0))
               (setq v1 ())
               (compare tokens "<" i1 v1 true)
               (compare tokens "<" i1 v1 true)
               (set@ i0 0 (car i1))
               (setq v0 v1)
            )
            (and
               (setq i1 (clone i0))
               (setq v1 ())
               (compare tokens ">" i1 v1 true)
               (compare tokens ">" i1 v1 true)
               (set@ i0 0 (car i1))
               (setq v0 v1)
            )
            (and
               (setq i1 (clone i0))
               (setq v1 ())
               (compare tokens "^" i1 v1 true)
               (compare tokens "^" i1 v1 true)
               (set@ i0 0 (car i1))
               (setq v0 v1)
            )
            (and
               (setq i1 (clone i0))
               (setq v1 ())
               (compare tokens "*" i1 v1 true)
               (compare tokens "*" i1 v1 true)
               (set@ i0 0 (car i1))
               (setq v0 v1)
            )
            (compare tokens "&" i0 v0 true)
            (compare tokens "|" i0 v0 true)
            (compare tokens "+" i0 v0 true)
            (compare tokens "-" i0 v0 true)
            (compare tokens "*" i0 v0 true)
            (compare tokens "/" i0 v0 true)
            (compare tokens "%" i0 v0 true)
            (compare tokens "^" i0 v0 true)
         )
         (push v (cons 'operator v0))
      )
   )
)

;orand := $ou^$et^$xou
(defun C_orand (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (or
            (compare tokens "ou" i0 v0 true)
            (compare tokens "et" i0 v0 true)
            (compare tokens "xou" i0 v0 true)
         )
         (push v (cons 'orand v0))
      )
   )
)

;comparator := [%< %>]^[%< %=]^[%= %=]^[%> %=]^%<^%>^$dans
(defun C_comparator (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (or
            (and
               (setq i1 (clone i0))
               (setq v1 ())
               (compare tokens "<" i1 v1 true)
               (compare tokens ">" i1 v1 true)
               (set@ i0 0 (car i1))
               (setq v0 v1)
            )
            (and
               (setq i1 (clone i0))
               (setq v1 ())
               (compare tokens "<" i1 v1 true)
               (compare tokens "=" i1 v1 true)
               (set@ i0 0 (car i1))
               (setq v0 v1)
            )
            (and
               (setq i1 (clone i0))
               (setq v1 ())
               (compare tokens "=" i1 v1 true)
               (compare tokens "=" i1 v1 true)
               (set@ i0 0 (car i1))
               (setq v0 v1)
            )
            (and
               (setq i1 (clone i0))
               (setq v1 ())
               (compare tokens ">" i1 v1 true)
               (compare tokens "=" i1 v1 true)
               (set@ i0 0 (car i1))
               (setq v0 v1)
            )
            (compare tokens "<" i0 v0 true)
            (compare tokens ">" i0 v0 true)
            (compare tokens "dans" i0 v0 true)
         )
         (push v (cons 'comparator v0))
      )
   )
)

;comparison := computing comparator computing [orand comparison]*
(defun C_comparison (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_computing tokens i1 v1)
            (C_comparator tokens i1 v1)
            (C_computing tokens i1 v1)
            (S_comparison_0 tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'comparison v0))
      )
   )
)

(defun S_comparison_0 (tokens i1 vp)
   (setq v ())
   (while (and
         (setq i2 (clone i1))
         (setq v2 ())
         (C_orand tokens i2 v2)
         (C_comparison tokens i2 v2)
         (set@ i1 0 (car i2))
         (setq v1 v2)
      )
      (nconc v v1)
   )
   (check v
      (nconc vp v)
   )
   true)

;!minus := %- anumber
(defun C_minus (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "-" i1 v1 nil)
            (C_anumber tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'minus v0))
      )
   )
)

;!indexes := computing [%, computing]*
(defun C_indexes (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_computing tokens i1 v1)
            (S_indexes_0 tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'indexes v0))
      )
   )
)

(defun S_indexes_0 (tokens i1 vp)
   (setq v ())
   (while (and
         (setq i2 (clone i1))
         (setq v2 ())
         (compare tokens "," i2 v2 nil)
         (C_computing tokens i2 v2)
         (set@ i1 0 (car i2))
         (setq v1 v2)
      )
      (nconc v v1)
   )
   (check v
      (nconc vp v)
   )
   true)

;!setdimvariable :=  Word %[ ;1 indexes %]
(defun C_setdimvariable (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_Word tokens i1 v1)
            (compare tokens "[" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_indexes tokens i1 v1)
                  (compare tokens "]" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'setdimvariable v0))
      )
   )
)

;!setdimvariablestring :=  Word %$ %[ ;1 indexes %]
(defun C_setdimvariablestring (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_Word tokens i1 v1)
            (compare tokens "$" i1 v1 nil)
            (compare tokens "[" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_indexes tokens i1 v1)
                  (compare tokens "]" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push
            v
            (cons 'setdimvariablestring v0)
         )
      )
   )
)

;!dimvariable := Word %[ ;1 indexes %]
(defun C_dimvariable (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_Word tokens i1 v1)
            (compare tokens "[" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_indexes tokens i1 v1)
                  (compare tokens "]" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'dimvariable v0))
      )
   )
)

;!dimvariablestring := Word %$ %[ ;1 indexes %]
(defun C_dimvariablestring (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_Word tokens i1 v1)
            (compare tokens "$" i1 v1 nil)
            (compare tokens "[" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_indexes tokens i1 v1)
                  (compare tokens "]" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'dimvariablestring v0))
      )
   )
)

;!stringvariable := Word %$
(defun C_stringvariable (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_Word tokens i1 v1)
            (compare tokens "$" i1 v1 nil)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'stringvariable v0))
      )
   )
)

;variable := Word
(defun C_variable (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_Word tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'variable v0))
      )
   )
)

;!variables := [stringvariable^variable] [%, [stringvariable^variable]]*
(defun C_variables (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (or
               (C_stringvariable tokens i1 v1)
               (C_variable tokens i1 v1)
            )
            (S_variables_0 tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'variables v0))
      )
   )
)

(defun S_variables_0 (tokens i1 vp)
   (setq v ())
   (while (and
         (setq i2 (clone i1))
         (setq v2 ())
         (compare tokens "," i2 v2 nil)
         (or
            (C_stringvariable tokens i2 v2)
            (C_variable tokens i2 v2)
         )
         (set@ i1 0 (car i2))
         (setq v1 v2)
      )
      (nconc v v1)
   )
   (check v
      (nconc vp v)
   )
   true)

;!call := [lambda^Word] %( ;2 [computing [%, computing]*]? %)
(defun C_call (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (or
               (C_λ tokens i1 v1)
               (C_Word tokens i1 v1)
            )
            (compare tokens "(" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (O_call_1 tokens i1 v1)
                  (compare tokens ")" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'call v0))
      )
   )
)

(defun S_call_0 (tokens i2 vp)
   (setq v ())
   (while (and
         (setq i3 (clone i2))
         (setq v3 ())
         (compare tokens "," i3 v3 nil)
         (C_computing tokens i3 v3)
         (set@ i2 0 (car i3))
         (setq v2 v3)
      )
      (nconc v v2)
   )
   (check v
      (nconc vp v)
   )
   true)

(defun O_call_1 (tokens i1 vp)
   (setq v1 ())
   (check (and
         (setq i2 (clone i1))
         (setq v2 ())
         (C_computing tokens i2 v2)
         (S_call_0 tokens i2 v2)
         (set@ i1 0 (car i2))
         (setq v1 v2)
      )
      (nconc vp v1)
   )
   true)

;!method := [dimvariablestring^dimvariable^stringvariable^variable] %. ;9 Word %( ;2 [computing [%, computing]*]? %)
(defun C_method (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (or
               (C_dimvariablestring tokens i1 v1)
               (C_dimvariable tokens i1 v1)
               (C_stringvariable tokens i1 v1)
               (C_variable tokens i1 v1)
            )
            (compare tokens "." i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_Word tokens i1 v1)
                  (compare tokens "(" i1 v1 nil)
                  (push error_id 0)
                  (if (and
                        (O_method_1 tokens i1 v1)
                        (compare tokens ")" i1 v1 nil)
                     )
                     (pop error_id)
                     (setg do_not_stop nil)
                  )
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'method v0))
      )
   )
)

(defun S_method_0 (tokens i2 vp)
   (setq v ())
   (while (and
         (setq i3 (clone i2))
         (setq v3 ())
         (compare tokens "," i3 v3 nil)
         (C_computing tokens i3 v3)
         (set@ i2 0 (car i3))
         (setq v2 v3)
      )
      (nconc v v2)
   )
   (check v
      (nconc vp v)
   )
   true)

(defun O_method_1 (tokens i1 vp)
   (setq v1 ())
   (check (and
         (setq i2 (clone i1))
         (setq v2 ())
         (C_computing tokens i2 v2)
         (S_method_0 tokens i2 v2)
         (set@ i1 0 (car i2))
         (setq v1 v2)
      )
      (nconc vp v1)
   )
   true)

;!assignment := [setdimvariablestring^setdimvariable^stringvariable^variable] %= computing
(defun C_assignment (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (or
               (C_setdimvariablestring tokens i1 v1)
               (C_setdimvariable tokens i1 v1)
               (C_stringvariable tokens i1 v1)
               (C_variable tokens i1 v1)
            )
            (compare tokens "=" i1 v1 nil)
            (C_computing tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'assignment v0))
      )
   )
)

;!dim :=  $DIM Word %[ ;1 indexes %]
(defun C_dim (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "DIM" i1 v1 nil)
            (C_Word tokens i1 v1)
            (compare tokens "[" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_indexes tokens i1 v1)
                  (compare tokens "]" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'dim v0))
      )
   )
)

;!dimstring :=  $DIM Word %$ %[ ;1 indexes %]
(defun C_dimstring (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "DIM" i1 v1 nil)
            (C_Word tokens i1 v1)
            (compare tokens "$" i1 v1 nil)
            (compare tokens "[" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_indexes tokens i1 v1)
                  (compare tokens "]" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'dimstring v0))
      )
   )
)

;!parenthetic := %( ;2 computing %)
(defun C_parenthetic (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "(" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_computing tokens i1 v1)
                  (compare tokens ")" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'parenthetic v0))
      )
   )
)

;!data := $données ;3 computing [%, computing]* $findonnées
(defun C_data (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "données" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_computing tokens i1 v1)
                  (S_data_0 tokens i1 v1)
                  (
                     compare
                     tokens
                     "findonnées"
                     i1
                     v1
                     nil
                  )
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'data v0))
      )
   )
)

(defun S_data_0 (tokens i1 vp)
   (setq v ())
   (while (and
         (setq i2 (clone i1))
         (setq v2 ())
         (compare tokens "," i2 v2 nil)
         (C_computing tokens i2 v2)
         (set@ i1 0 (car i2))
         (setq v1 v2)
      )
      (nconc v v1)
   )
   (check v
      (nconc vp v)
   )
   true)

;^callitem := method^call^lambda^parenthetic^data^astring^minus^anumber^dimvariablestring^dimvariable^stringvariable^variable
(defun C_callitem (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (or
            (C_method tokens i0 v0)
            (C_call tokens i0 v0)
            (C_λ tokens i0 v0)
            (C_parenthetic tokens i0 v0)
            (C_data tokens i0 v0)
            (C_astring tokens i0 v0)
            (C_minus tokens i0 v0)
            (C_anumber tokens i0 v0)
            (C_dimvariablestring tokens i0 v0)
            (C_dimvariable tokens i0 v0)
            (C_stringvariable tokens i0 v0)
            (C_variable tokens i0 v0)
         )
         (nconc v v0)
      )
   )
)

;&computing := callitem  [operator callitem]*
(defun C_computing (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_callitem tokens i1 v1)
            (S_computing_0 tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (if (eq (size v0) 1)
            (nconc v v0)
            (push v (cons 'computing v0))
         )
      )
   )
)

(defun S_computing_0 (tokens i1 vp)
   (setq v ())
   (while (and
         (setq i2 (clone i1))
         (setq v2 ())
         (C_operator tokens i2 v2)
         (C_callitem tokens i2 v2)
         (set@ i1 0 (car i2))
         (setq v1 v2)
      )
      (nconc v v1)
   )
   (check v
      (nconc vp v)
   )
   true)

;body := expressions+
(defun C_body (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (P_body_0 tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'body v0))
      )
   )
)

(defun P_body_0 (tokens i1 vp)
   (setq v ())
   (setq v1 ())
   (check (C_expressions tokens i1 v1)
      (push v v1)
      (setq v1 ())
      (while (C_expressions tokens i1 v1)
         (nconc v v1)
         (setq v1 ())
      )
   )
   (if v
      (nconc vp v)
   )
)

;!arguments := computing [%, computing]*
(defun C_arguments (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_computing tokens i1 v1)
            (S_arguments_0 tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'arguments v0))
      )
   )
)

(defun S_arguments_0 (tokens i1 vp)
   (setq v ())
   (while (and
         (setq i2 (clone i1))
         (setq v2 ())
         (compare tokens "," i2 v2 nil)
         (C_computing tokens i2 v2)
         (set@ i1 0 (car i2))
         (setq v1 v2)
      )
      (nconc v v1)
   )
   (check v
      (nconc vp v)
   )
   true)

;!multiop := Word computing [%, computing]*
(defun C_multiop (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_Word tokens i1 v1)
            (C_computing tokens i1 v1)
            (S_multiop_0 tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'multiop v0))
      )
   )
)

(defun S_multiop_0 (tokens i1 vp)
   (setq v ())
   (while (and
         (setq i2 (clone i1))
         (setq v2 ())
         (compare tokens "," i2 v2 nil)
         (C_computing tokens i2 v2)
         (set@ i1 0 (car i2))
         (setq v1 v2)
      )
      (nconc v v1)
   )
   (check v
      (nconc vp v)
   )
   true)

;^expressions := method^call^lambda^dimstring^dim^assignment^forin^for^if^while^multiop^computing
(defun C_expressions (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (or
            (C_method tokens i0 v0)
            (C_call tokens i0 v0)
            (C_λ tokens i0 v0)
            (C_dimstring tokens i0 v0)
            (C_dim tokens i0 v0)
            (C_assignment tokens i0 v0)
            (C_forin tokens i0 v0)
            (C_for tokens i0 v0)
            (C_if tokens i0 v0)
            (C_while tokens i0 v0)
            (C_multiop tokens i0 v0)
            (C_computing tokens i0 v0)
         )
         (nconc v v0)
      )
   )
)

;!then := $Alors expressions+
(defun C_then (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "Alors" i1 v1 nil)
            (P_then_0 tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'then v0))
      )
   )
)

(defun P_then_0 (tokens i1 vp)
   (setq v ())
   (setq v1 ())
   (check (C_expressions tokens i1 v1)
      (push v v1)
      (setq v1 ())
      (while (C_expressions tokens i1 v1)
         (nconc v v1)
         (setq v1 ())
      )
   )
   (if v
      (nconc vp v)
   )
)

;!else := $Sinon expressions+
(defun C_else (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "Sinon" i1 v1 nil)
            (P_else_0 tokens i1 v1)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'else v0))
      )
   )
)

(defun P_else_0 (tokens i1 vp)
   (setq v ())
   (setq v1 ())
   (check (C_expressions tokens i1 v1)
      (push v v1)
      (setq v1 ())
      (while (C_expressions tokens i1 v1)
         (nconc v v1)
         (setq v1 ())
      )
   )
   (if v
      (nconc vp v)
   )
)

;!function := $Fonction ;4 Word %( ;2 variables? %) expressions+ $FinFonction
(defun C_function (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "Fonction" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_Word tokens i1 v1)
                  (compare tokens "(" i1 v1 nil)
                  (push error_id 0)
                  (if (and
                        (O_function_0 tokens i1 v1)
                        (compare tokens ")" i1 v1 nil)
                        (P_function_1 tokens i1 v1)
                        (
                           compare
                           tokens
                           "FinFonction"
                           i1
                           v1
                           nil
                        )
                     )
                     (pop error_id)
                     (setg do_not_stop nil)
                  )
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'function v0))
      )
   )
)

(defun O_function_0 (tokens i1 vp)
   (setq v1 ())
   (check (C_variables tokens i1 v1)
      (nconc vp v1)
   )
   true)

(defun P_function_1 (tokens i1 vp)
   (setq v ())
   (setq v1 ())
   (check (C_expressions tokens i1 v1)
      (push v v1)
      (setq v1 ())
      (while (C_expressions tokens i1 v1)
         (nconc v v1)
         (setq v1 ())
      )
   )
   (if v
      (nconc vp v)
   )
)

;!lambda := %[ ;1 %( ;2 variables? %) body %]
(defun C_λ (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "[" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (compare tokens "(" i1 v1 nil)
                  (push error_id 0)
                  (if (and
                        (O_λ_0 tokens i1 v1)
                        (compare tokens ")" i1 v1 nil)
                        (C_body tokens i1 v1)
                        (compare tokens "]" i1 v1 nil)
                     )
                     (pop error_id)
                     (setg do_not_stop nil)
                  )
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'λ v0))
      )
   )
)

(defun O_λ_0 (tokens i1 vp)
   (setq v1 ())
   (check (C_variables tokens i1 v1)
      (nconc vp v1)
   )
   true)

;!if := $Si ;5 comparison then else? $FinSi
(defun C_if (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "Si" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_comparison tokens i1 v1)
                  (C_then tokens i1 v1)
                  (O_if_0 tokens i1 v1)
                  (compare tokens "FinSi" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'if v0))
      )
   )
)

(defun O_if_0 (tokens i1 vp)
   (setq v1 ())
   (check (C_else tokens i1 v1)
      (nconc vp v1)
   )
   true)

;!while := [$tantque^[$tant $que]] ;6 comparison expressions+ $FinTantQue
(defun C_while (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (or
               (compare tokens "tantque" i1 v1 nil)
               (and
                  (setq i2 (clone i1))
                  (setq v2 ())
                  (compare tokens "tant" i2 v2 nil)
                  (compare tokens "que" i2 v2 nil)
                  (set@ i1 0 (car i2))
                  (setq v1 v2)
               )
            )
            (push error_id 0)
            (if (and
                  (C_comparison tokens i1 v1)
                  (P_while_0 tokens i1 v1)
                  (compare tokens "FinTantQue" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'while v0))
      )
   )
)

(defun P_while_0 (tokens i1 vp)
   (setq v ())
   (setq v1 ())
   (check (C_expressions tokens i1 v1)
      (push v v1)
      (setq v1 ())
      (while (C_expressions tokens i1 v1)
         (nconc v v1)
         (setq v1 ())
      )
   )
   (if v
      (nconc vp v)
   )
)

;!forin := $Pour [stringvariable^variable] $dans ;7 callitem expressions+ $FinPour
(defun C_forin (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "Pour" i1 v1 nil)
            (or
               (C_stringvariable tokens i1 v1)
               (C_variable tokens i1 v1)
            )
            (compare tokens "dans" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_callitem tokens i1 v1)
                  (P_forin_0 tokens i1 v1)
                  (compare tokens "FinPour" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'forin v0))
      )
   )
)

(defun P_forin_0 (tokens i1 vp)
   (setq v ())
   (setq v1 ())
   (check (C_expressions tokens i1 v1)
      (push v v1)
      (setq v1 ())
      (while (C_expressions tokens i1 v1)
         (nconc v v1)
         (setq v1 ())
      )
   )
   (if v
      (nconc vp v)
   )
)

;!for := $Pour ;8 assignment %, comparison %, assignment expressions+ $FinPour
(defun C_for (tokens i0 v)
   (check (and
         do_not_stop
         (< (car i0) (size tokens))
      )
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "Pour" i1 v1 nil)
            (push error_id 0)
            (if (and
                  (C_assignment tokens i1 v1)
                  (compare tokens "," i1 v1 nil)
                  (C_comparison tokens i1 v1)
                  (compare tokens "," i1 v1 nil)
                  (C_assignment tokens i1 v1)
                  (P_for_0 tokens i1 v1)
                  (compare tokens "FinPour" i1 v1 nil)
               )
               (pop error_id)
               (setg do_not_stop nil)
            )
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'for v0))
      )
   )
)

(defun P_for_0 (tokens i1 vp)
   (setq v ())
   (setq v1 ())
   (check (C_expressions tokens i1 v1)
      (push v v1)
      (setq v1 ())
      (while (C_expressions tokens i1 v1)
         (nconc v v1)
         (setq v1 ())
      )
   )
   (if v
      (nconc vp v)
   )
)


(defun nokeywords(w)
      (not (in  '("ou" "et" "xou" "dans" "dim" "données" "findonnées" "alors" "sinon" "fonction" "finfonction" "si" "finsi" "tantque" "tant" "que" "fintantque" "pour" "finpour") (lower w)))
)
   (setq error_messages {"1":"Attend une terminaison \"]\"" "2":"Attend une terminaison \")\"" "3":"Attend une balise de fermeture pour \"données\" ." "4":"Attend une balise de fermeture pour \"fonction\" ." "5":"Attend une balise de fermeture pour \"si\" ." "6":"Attend une balise de fermeture pour \"tantque\" ;" "7":"Attend une balise de fermeture pour \"pour...dans\" ." "8":"Attend une balise fermante pour \"pour\" ;" "9":"Erreur lors de l ' analyse d ' une méthode"})
(setq parser_tok (tokenizer_rules))

(setq rg «1:{%d #A-F #a-f}
#9=#
#10=#
#32=#
"{[\-"] ~%r}*"=34
0x%1+(.%1+({p P}({%- %+})%d+))=57
%d+(.%d+({eE}({%- %+})%d+))=57
%o=63
%p=32
%h{%h %d}*=65
%a{%a %d}*=65
»
)

(setq rg (replace rg "\r" ""))
(setq rg (split rg "\n"))

(set_tokenizer_rules parser_tok rg)

(defun abstract_tree (code)
   (setq code (replace code "\r" ""))
   (setg error_id '(0))
   (setg do_not_stop true)
   (key error_messages 0 "Syntax Error")
   ; The magic of LIsp. A one-liner to get rid of lines that starts with REM
   (setq code (join (filterlist (\(x) (neq (lower (@@ (trim x) 0 4)) "rem ")) (split code "\n")) "\n"))
   (setq tokens (tokenize_rules parser_tok (+ code "\n")))
   (setq i '(0))
   (setq res (C_analyse tokens i ()))
   (if (< (car i) (size tokens))
	(list (join (maplist (\(x) (@ error_messages x)) error_id) ", ") "in \"" (join (@@ tokens (car i) (+ 20 (car i))) " ") "\"")
      res
   )
)

