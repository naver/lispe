;;
Date: 01/03/2023
Author: Claude Roux
Description: try/catch
;;

(defun trouvecatch(fonc i)
   (setq corps "")
   (while (neq (@ fonc i) "{")
      (+= corps (@ fonc i))
      (+= i 1)
   )
   (setq indent 0)
   (loop c (@@ fonc i 0)
      (+= corps c)
      (+= indent (== c "{"))
      (-= indent (== c "}"))
      (if (not indent)
         (break)
      )
   )
   corps
)

(defun transforme(fonc fichier)
   (check 
      (and 
         (not (in fonc "catch (Error*"))
         (not (in fonc "throw "))
         (not (in fonc "!= l_return"))
      )
      (return fonc)
   )

   (setq type_return "")
   (setq retvoid false)
   (cond
      ((in (@@ fonc 0 24) "Element* ") (setq type_return "e"))
      ((in (@@ fonc 0 15) "void") (setq type_return "v") (setq retvoid true))
      ((in (@@ fonc 0 20) "double") (setq type_return "n"))
      ((in (@@ fonc 0 20) "float") (setq type_return "n"))
      ((in (@@ fonc 0 20) "int16_t") (setq type_return "n"))
      ((in (@@ fonc 0 20) "long") (setq type_return "n"))
      ((in (@@ fonc 0 20) "u_ustring") (setq type_return "s"))
   )

   (check (in fonc "&& element != terminal_")
      (setq fonc (replace fonc "&& element != terminal_" "&& thrown_error == NULL && element != terminal_"))      
   )

   (check (in fonc "; e < listsz;")
      (setq fonc (replace fonc "; e < listsz;" "; e < listsz && thrown_error == NULL;"))      
   )

   (check (in fonc "< listsize &&")
      (setq fonc (replace fonc "< listsize &&" "< listsize && thrown_error == NULL &&"))      
   )
   (check (in fonc "< listsize;")
      (setq fonc (replace fonc "< listsize;" "< listsize && thrown_error == NULL;"))      
   )
   (check (in fonc "i_loop < sz")
      (setq fonc (replace fonc "i_loop < sz" "i_loop < sz && thrown_error == NULL"))      
   )

   (setq tries (rgx_findall (rgx "try %{?+catch %(Error") fonc))
   (setq catches ())
   (setq pos (findall fonc "catch "))
   (loop p pos
      (push catches (trouvecatch fonc p))
   )

   (setq disp false)
   ; We have different cases
   ; The most common cases, there is no throw in the function
   (lloop (atry acatch) tries catches
      ;first do we have a throw in it ?
      (setq ntry atry)
      (while (in ntry "throw ")
         (setq bathrow (@@ ntry +"throw " +";"))
         (setq athrow (@@ ntry "throw " ";"))
         (setq nthrow (@@ acatch +"{" -+"}"))
         (check (in acatch "throw err")
            (if retvoid
               (setq nthrow (replace nthrow "throw err;" (+ "{lisp->delegation->set_error(" athrow ");return;}" )))
               (if (in fonc "LispE::")
                  (setq nthrow (replace nthrow "throw err" (+ "return delegation->set_error(" athrow ")" )))
                  (setq nthrow (replace nthrow "throw err" (+ "return lisp->delegation->set_error(" athrow ")" )))
               )
            )
         )
         (check (in acatch "return lisp->check_error(this, err, line, fileidx);")            
            (setq nthrow (replace nthrow "(this, err" (+ "(this,  " athrow)))
         )
         (setq ntry (replace ntry bathrow nthrow))
      )      
      (setq ntry (@@ ntry "{" -"}"))
      (if (in fonc "LispE::")
         (setq nthrow (+ "if (delegation->current_error) " (@@ acatch +"{" -+"}")))
         (setq nthrow (+ "if (thrown_error) " (@@ acatch +"{" -+"}")))
      )
      (if (in acatch "throw err")
         (if retvoid
            (setq nthrow (replace nthrow "throw err;" "{\nlisp->delegation->set_error(thrown_error);\nreturn;\n}"))
            (if (in fonc "LispE::")
               (setq nthrow (replace nthrow "throw err" "return delegation->set_error(delegation->current_error)"))
               (setq nthrow (replace nthrow "throw err" "return lisp->delegation->set_error(thrown_error)"))
            )
         )
      )
      (if (in acatch "return lisp->check_error(this, err, line, fileidx);")               
         (setq nthrow (replace nthrow "(this, err" "(this, thrown_error"))
      )
      ;Do we have return values in the ntry?
      (ife (in ntry "if (e->type == l_return) {")
         (setq ntry (replace ntry "if (e->type == l_return) {" (+ nthrow "\nif (e->type == l_return) {")))
         (check 
            (and
               (in ntry "lisp->resetStack")
               (not (in nthrow "lisp->resetStack"))
            )
            (setq ntry (replace ntry "lisp->resetStack"  (+ nthrow "\n" "lisp->resetStack")))
            (setq disp true)
         )
      )
      (setq fonc (replace fonc (+ (@@ atry 0 -"}") "}") ntry))
      (setq fonc (replace fonc acatch nthrow))
      (setq fonc (replace fonc "e = err;" "e = delegation->current_error;"))
      (if (in fonc "LispE::")
         (setq fonc (replace fonc "return err;" "return delegation->current_error;"))
         (setq fonc (replace fonc "return err;" "return thrown_error;"))
      )
      (setq fonc (replace fonc "return lisp->check_error(this, err, -1, 0);" "return lisp->check_error(this, thrown_error, -1, 0);"))
      (if (in fonc "Dictionary_as_list::dictionary")
         (setq fonc (replace fonc "err->release();" "lisp->delegation->reset_error();"))
      )
   )

   ; If we still have some throw in the function
   (check (in fonc "throw ")
      (check (in fonc "::divide_direct")
         (setq throws (unique (rgx_findall (rgx "throw ?+;") fonc)))
         (loop th throws
            (setq fonc (replace fonc th (+ "{\nrelease();\n" (replace th "throw " "return ") "\n}")))
         )
      )
      (if (eq type_return "e")
         (setq fonc (replace fonc "throw " "return "))
         (check (in fonc "lisp")
            (setq throws (unique (rgx_findall (rgx "throw ?+;") fonc)))
            (loop th throws
               (switch type_return
                  ("s" (setq fonc (replace fonc th (+ "{\nlisp->delegation->set_error(" (@@ th " " ";") ");\nreturn U\"\";\n}"))))
                  ("n" (setq fonc (replace fonc th (+ "{\nlisp->delegation->set_error(" (@@ th " " ";") ");\nreturn 0;\n}"))))
                  (true (setq fonc (replace fonc th (+ "{\nlisp->delegation->set_error(" (@@ th " " ";") ");\nreturn;\n}"))))
               )
            )
         )
      )
   )
   (check (in fonc "throw ")
      (check (in fichier "lispe.h")
         (setq throws (unique (rgx_findall (rgx "throw ?+;") fonc)))
         (loop th throws
            (setq fonc (replace fonc th (+ "{\ndelegation->set_error(" (@@ th " " ";") ");\nreturn;\n}")))
         )
      )
   )
   (check (in fonc "i < nbarguments && element->type != l_return")
      (setq fonc 
         (replace fonc 
            "i < nbarguments && element->type != l_return" 
            "i < nbarguments && thrown_error == NULL && element->type != l_return")
      )
   )

   (check (in fonc "i < nbinstructions && element->type != l_return")
      (setq fonc 
         (replace fonc 
            "i < nbinstructions && element->type != l_return" 
            "i < nbinstructions && thrown_error == NULL && element->type != l_return")
      )
   )   

   (setq fonc (indent fonc false))
   (check disp
      (setq cherche `return e;

   if (thrown_error) {`)

      (check (in fonc cherche)            
         (setq fonc (indent (+ (@@ fonc +"E" -+"return e;") "\n}") false))
      )
   )

   (check (in fonc "LispE::")
      (setq posbis (find fonc "if (delegation->current_error)"))
      (setq pos (find fonc "return tree->eval"))
      (check (< pos posbis)
         (setq fonc (replace fonc  (@@ fonc +"if (delegation->current_error)" +"}") ""))
      )
      (setq pos (find fonc "return body;"))
      (check (< pos posbis)
         (setq fonc (replace fonc  (@@ fonc +"if (delegation->current_error)" +"}") ""))
      )
      (setq pos (find fonc "return compile_lisp_code(code)->eval(this);"))
      (check (< pos posbis)
         (setq fonc (replace fonc  (@@ fonc +"if (delegation->current_error)" +"}") ""))
      )
   )


   fonc
)

(defun traite_cxx (fichier)
   (setq corps (fread fichier))
   (setq corps (replace corps "::eval(" "::_eval("))
   (setq corps (replace corps "Element* eval(LispE* lisp)" "Element* _eval(LispE* lisp)"))      
   (setq corps (split (replace corps "::EVAL(" "::eval(") "\n"))
   (setq pt (tokenizer_main))
   (setq cpt  0)
   (setq fonc "")
   (setq lecode (strings))
   (loop l corps
      (setq e (tokenize pt l))
      (+= cpt (count e "{"))
      (check (in e "class")
         (setq cpt 0)
      )
      (-= cpt (count e "}"))
      (if (< cpt 0) (setq cpt 0))
      (if cpt
         (+= fonc l "\n")
         (ncheck fonc
            (push lecode l)
            (+= fonc l "\n")
            (push lecode (transforme fonc fichier))
            (setq fonc "")
         )
      )
   )
   (check fonc
      (push lecode fonc)
   )
   (setq lecode (join lecode "\n"))
   (setq fichier (@@ fichier -"/" 0))
   (fwrite (+ _current "../src/" fichier) lecode)
)

(defun traite_h(fichier)
   (setq corps (fread fichier))
   (setq corps (replace corps "Element* eval(" "Element* _eval("))
   (setq corps (split (replace corps "Element* EVAL(" "Element* eval(") "\n"))
   (setq pt (tokenizer_main))
   (setq cpt  0)
   (setq fonc "")
   (setq lecode (strings))
   (loop l corps
      (setq e (tokenize pt l))
      (+= cpt (count e "{"))
      (check (in e "class")
         (setq cpt 0)
      )
      (-= cpt (count e "}"))
      (if (< cpt 0) (setq cpt 0))
      (if cpt
         (+= fonc l "\n")
         (ncheck fonc
            (push lecode l)
            (+= fonc l "\n")
            (push lecode (transforme fonc fichier))
            (setq fonc "")
         )
      )
   )
   (check fonc
      (push lecode fonc)
   )
   (setq lecode (join lecode "\n"))
   (setq fichier (@@ fichier -"/" 0))
   (fwrite (+ _current "../include/" fichier) lecode)
)

(defun boucle (fichiers)
   (loop f fichiers
      (check 
         (and
            (not (in f "old"))
            (not (in f "DS"))
            (not (in f ".cpp"))
         )         
         (if (in f ".cxx")
            (traite_cxx f)         
            (traite_h f)         
         )
      )
   )
)

(elapse
   (boucle (ls (+ _current "../../src")))
   (boucle (ls (+ _current "../../include")))
)


