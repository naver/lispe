;Date: 2022/10/05 15:49:01
;Description: Parser for hellenica description
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
   (check (< (car i0) (size tokens))
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

(link "εμφάνισε" 'print)
(link "εμφάνισεγρ" 'println)
(link "επιστροφή" 'return)
(link "εύρος" 'range)
(link "στοιβάζω" 'push)
(link "έβγαλε" 'pop)
(link "μέγεθος" 'size)
(link "διαβάστεαρ" 'fread)
(link "γράψτεαρ" 'fwrite)
(link "ακέραιοι" 'integers)
(link "αριθμοί" 'numbers)
(link "συμβολοσειρές" 'strings)
(link "ακέραιος" 'integer)
(link "αριθμός" 'number)
(link "συμβολοσειρά" 'string)
(link "εξάγετε" 'extract)
(link "μέσα" 'in)
(link "ή" 'or)
(link "και" 'and)
(link "ήή" 'xor)
;operator := [%< %<]^[%> %>]^[%^ %^]^[%* %*]^%&^%|^%+^%-^%*^%/^%%^%^
(defun C_operator (tokens i0 v)
   (check (< (car i0) (size tokens))
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

;orand := $ή^$και^$ήή
(defun C_orand (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (or
            (compare tokens "ή" i0 v0 true)
            (compare tokens "και" i0 v0 true)
            (compare tokens "ήή" i0 v0 true)
         )
         (push v (cons 'orand v0))
      )
   )
)

;comparator := [%< %>]^[%< %=]^[%= %=]^[%> %=]^%<^%>^$μέσα
(defun C_comparator (tokens i0 v)
   (check (< (car i0) (size tokens))
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
            (compare tokens "μέσα" i0 v0 true)
         )
         (push v (cons 'comparator v0))
      )
   )
)

;comparison := computing comparator computing [orand comparison]*
(defun C_comparison (tokens i0 v)
   (check (< (car i0) (size tokens))
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
   (check (< (car i0) (size tokens))
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
   (check (< (car i0) (size tokens))
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

;!setdimvariable :=  Word %[ indexes %]
(defun C_setdimvariable (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_Word tokens i1 v1)
            (compare tokens "[" i1 v1 nil)
            (C_indexes tokens i1 v1)
            (compare tokens "]" i1 v1 nil)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'setdimvariable v0))
      )
   )
)

;!setdimvariablestring :=  Word %$ %[ indexes %]
(defun C_setdimvariablestring (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_Word tokens i1 v1)
            (compare tokens "$" i1 v1 nil)
            (compare tokens "[" i1 v1 nil)
            (C_indexes tokens i1 v1)
            (compare tokens "]" i1 v1 nil)
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

;!dimvariable := Word %[ indexes %]
(defun C_dimvariable (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_Word tokens i1 v1)
            (compare tokens "[" i1 v1 nil)
            (C_indexes tokens i1 v1)
            (compare tokens "]" i1 v1 nil)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'dimvariable v0))
      )
   )
)

;!dimvariablestring := Word %$ %[ indexes %]
(defun C_dimvariablestring (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (C_Word tokens i1 v1)
            (compare tokens "$" i1 v1 nil)
            (compare tokens "[" i1 v1 nil)
            (C_indexes tokens i1 v1)
            (compare tokens "]" i1 v1 nil)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'dimvariablestring v0))
      )
   )
)

;!stringvariable := Word %$
(defun C_stringvariable (tokens i0 v)
   (check (< (car i0) (size tokens))
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
   (check (< (car i0) (size tokens))
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
   (check (< (car i0) (size tokens))
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

;!call := [lambda^Word] %( [computing [%, computing]*]? %)
(defun C_call (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (or
               (C_λ tokens i1 v1)
               (C_Word tokens i1 v1)
            )
            (compare tokens "(" i1 v1 nil)
            (O_call_1 tokens i1 v1)
            (compare tokens ")" i1 v1 nil)
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

;!method := [dimvariablestring^dimvariable^stringvariable^variable] %. Word %( [computing [%, computing]*]? %)
(defun C_method (tokens i0 v)
   (check (< (car i0) (size tokens))
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
            (C_Word tokens i1 v1)
            (compare tokens "(" i1 v1 nil)
            (O_method_1 tokens i1 v1)
            (compare tokens ")" i1 v1 nil)
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
   (check (< (car i0) (size tokens))
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

;!dim :=  $διάσταση Word %[ indexes %]
(defun C_dim (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (
               compare
               tokens
               "διάσταση"
               i1
               v1
               nil
            )
            (C_Word tokens i1 v1)
            (compare tokens "[" i1 v1 nil)
            (C_indexes tokens i1 v1)
            (compare tokens "]" i1 v1 nil)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'dim v0))
      )
   )
)

;!dimstring :=  $διάσταση Word %$ %[ indexes %]
(defun C_dimstring (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (
               compare
               tokens
               "διάσταση"
               i1
               v1
               nil
            )
            (C_Word tokens i1 v1)
            (compare tokens "$" i1 v1 nil)
            (compare tokens "[" i1 v1 nil)
            (C_indexes tokens i1 v1)
            (compare tokens "]" i1 v1 nil)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'dimstring v0))
      )
   )
)

;!parenthetic := %( computing %)
(defun C_parenthetic (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "(" i1 v1 nil)
            (C_computing tokens i1 v1)
            (compare tokens ")" i1 v1 nil)
            (set@ i0 0 (car i1))
            (setq v0 v1)
         )
         (push v (cons 'parenthetic v0))
      )
   )
)

;!data := $δεδομένα computing [%, computing]* $τέλοςδεδομένα
(defun C_data (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (
               compare
               tokens
               "δεδομένα"
               i1
               v1
               nil
            )
            (C_computing tokens i1 v1)
            (S_data_0 tokens i1 v1)
            (
               compare
               tokens
               "τέλοςδεδομένα"
               i1
               v1
               nil
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
   (check (< (car i0) (size tokens))
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
   (check (< (car i0) (size tokens))
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
   (check (< (car i0) (size tokens))
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
   (check (< (car i0) (size tokens))
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
   (check (< (car i0) (size tokens))
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
   (check (< (car i0) (size tokens))
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

;!then := $τότε expressions+
(defun C_then (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "τότε" i1 v1 nil)
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

;!else := $αλλιώς expressions+
(defun C_else (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (
               compare
               tokens
               "αλλιώς"
               i1
               v1
               nil
            )
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

;!function := $συνάρτηση Word %( variables? %) expressions+ $τέλοςσυνάρτησης
(defun C_function (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (
               compare
               tokens
               "συνάρτηση"
               i1
               v1
               nil
            )
            (C_Word tokens i1 v1)
            (compare tokens "(" i1 v1 nil)
            (O_function_0 tokens i1 v1)
            (compare tokens ")" i1 v1 nil)
            (P_function_1 tokens i1 v1)
            (
               compare
               tokens
               "τέλοςσυνάρτησης"
               i1
               v1
               nil
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

;!lambda := %[ %( variables? %) body %]
(defun C_λ (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "[" i1 v1 nil)
            (compare tokens "(" i1 v1 nil)
            (O_λ_0 tokens i1 v1)
            (compare tokens ")" i1 v1 nil)
            (C_body tokens i1 v1)
            (compare tokens "]" i1 v1 nil)
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

;!if := $αν comparison then else? $τέλοςαν
(defun C_if (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "αν" i1 v1 nil)
            (C_comparison tokens i1 v1)
            (C_then tokens i1 v1)
            (O_if_0 tokens i1 v1)
            (
               compare
               tokens
               "τέλοςαν"
               i1
               v1
               nil
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

;!while := $ενώ comparison expressions+ $τέλοςενώ
(defun C_while (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "ενώ" i1 v1 nil)
            (C_comparison tokens i1 v1)
            (P_while_0 tokens i1 v1)
            (
               compare
               tokens
               "τέλοςενώ"
               i1
               v1
               nil
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

;!forin := $για [stringvariable^variable] $μέσα callitem expressions+ $τέλοςγια
(defun C_forin (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "για" i1 v1 nil)
            (or
               (C_stringvariable tokens i1 v1)
               (C_variable tokens i1 v1)
            )
            (compare tokens "μέσα" i1 v1 nil)
            (C_callitem tokens i1 v1)
            (P_forin_0 tokens i1 v1)
            (
               compare
               tokens
               "τέλοςγια"
               i1
               v1
               nil
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

;!for := $για assignment %, comparison %, assignment expressions+ $τέλοςγια
(defun C_for (tokens i0 v)
   (check (< (car i0) (size tokens))
      (setq v0 ())
      (if (and
            (setq i1 (clone i0))
            (setq v1 ())
            (compare tokens "για" i1 v1 nil)
            (C_assignment tokens i1 v1)
            (compare tokens "," i1 v1 nil)
            (C_comparison tokens i1 v1)
            (compare tokens "," i1 v1 nil)
            (C_assignment tokens i1 v1)
            (P_for_0 tokens i1 v1)
            (
               compare
               tokens
               "τέλοςγια"
               i1
               v1
               nil
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
      (not (in  '("ή" "και" "ήή" "μέσα" "διάσταση" "δεδομένα" "τέλοςδεδομένα" "τότε" "αλλιώς" "συνάρτηση" "τέλοςσυνάρτησης" "αν" "τέλοςαν" "ενώ" "τέλοςενώ" "για" "τέλοςγια") (lower w)))
)
   
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

(defun abstract_tree (code)
   ; The magic of LIsp. A one-liner to get rid of lines that starts with REM
   (setq code (join (filterlist (\(x) (neq (lower (@@ (trim x) 0 4)) "rem ")) (split code "\n")) "\n"))
   (setq tokens (tokenize_rules parser_tok code))
   (setq i '(0))
   (setq res (C_analyse tokens i ()))
   (if (< (car i) (size tokens))
      (cons "Error from: " (@@ tokens (car i) (+ 20 (car i))))
      res
   )
)

