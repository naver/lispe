;Date: 16/02/2023
;Author: Claude Roux
;Description: Code génération


(setq rep (ls (+ _current "../../src")))

(defun transforme(fonc)
   (if (in fonc "Error* throw_error(Error* err) {")
      (return fonc)
   )
   (setq retour "")
   (setq return_value (in (@@ (trim fonc) 0 9) "Element* "))   
   (check (not return_value)
      (setq r (trim (@@ (trim fonc) 0 " ")))
      (switch r
         ("string" (setq retour ");\nreturn \"\";\n}"))
         ("void" (setq retour ");\nreturn;\n}"))
         (true (setq retour ");\nreturn 0;\n}"))
      )
   )

   (check (in fonc "try ")
      (setq tries (rgx_findall_i (rgx "try %{") fonc))
      (setq tries (rho (/ (size tries) 2) 2 tries))
      (setq fonc (replace fonc "try " ""))
      (setq fonc (replace fonc "catch (Error* err)" "if (thrown_error != NULL)"))
      (setq fonc (replace fonc "return err;" "return thrown_error;"))
      (setq fonc (replace fonc "e = err;" "e = thrown_error;"))
      (if return_value
         (setq fonc (replace fonc "throw err" "return thrown_error"))
         (setq fonc (replace fonc "throw err" ""))
      )
      (setq fonc (replace fonc "lisp->delegation->setError(err)" "lisp->delegation->setError(thrown_error)"))
      (setq fonc (replace fonc "lisp->check_error(this, err," "lisp->check_error(this, thrown_error, "))      
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

   (check (in fonc "if (e->type == l_return)")
      (setq fonc (replace fonc "if (e->type == l_return)" "if (thrown_error != NULL)\nreturn thrown_error;\nif (e->type == l_return)"))      
   )
   (setq rplc ())
   (check (in fonc "throw ")
      (setq throws (rgx_findall_i (rgx "throw ?+;") fonc))
      (setq throws (rho (/ (size throws) 2) 2 throws))
      (setq catches (rgx_findall_i (rgx "if %(thrown_error != NULL%) %{?+return?+%}") fonc))
      (ncheck catches
         (loop ths throws
            (setq th (@@ fonc (@ ths 0) (@ ths 1))) 
            (if return_value
               (setq c (+ "return throw_error(" (@@ th +"new Error" -+")") ");"))
               (setq c (+ "{\nthrow_error(" (@@ th +"new Error" -+")") retour))
            )
            (push rplc (list ths c))
         )
         (setq catches (rho (/ (size catches) 2) 2 catches))
         (setq icatch 0) 
         (setq c_catch (@ catches 0))
         (setq c_try (@ tries 0))
         (loop ths throws
            (check (or (not c_catch) (> (@ ths 1) (@ c_catch 0)))
               (+= icatch 1)
               (ife (>= icatch (size catches))
                  (setq c_catch nil)
                  (setq c_try (@ tries icatch))
                  (setq c_catch (@ catches icatch))
               )
            )
            (ife
               (or
                  (not c_catch)
                  (< (@ ths 0) (@ c_try 1))
               )
               (block
                  (setq th (@@ fonc (@ ths 0) (@ ths 1))) 
                  (if return_value
                     (setq c (+ "return throw_error(" (@@ th +"new Error" -+")") ");"))
                     (setq c (+ "{\nthrow_error(" (@@ th +"new Error" -+")") retour))
                  )
               )
               (setq c (@@ fonc (@ c_catch 0) (@ c_catch 1)))
               (setq th (@@ fonc (@ ths 0) (@ ths 1))) 
               (setq th (+ "throw_error(" (@@ th +"new Error" -+")") ")"))
               (ife (in c "check_error")
                  (setq c (@@ (replace c "thrown_error," (+ th ",")) +"{" 0))
                  (setq cc (@@ c +"{" +"return "))
                  (if cc
                     (setq c (+ (@@ cc +"{" +"return ") th ";\n}"))
                     (setq c (+ (@@ c +"{" -"}") th ";\n}"))
                  )
               )
            )
            (push rplc (list ths c))
         )
      )

      (loop r (reverse rplc)
         (setq fonc (replace fonc (cadr r) (caar r) (cadar r)))
      )
   )

   (setq fonc (replace fonc "err->release();" "thrown_error->decrement();\nthrown_error = NULL;"))
   (setq fonc (replace fonc "throw_error()" "throw_error(delegation->_THEEND)"))
   (indent fonc false)
)

(defun traite (fichier)
   (println fichier)
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
            (push lecode (transforme fonc))
            (setq fonc "")
         )
      )
   )
   (check fonc
      (push lecode fonc)
   )
   (setq lecode (join lecode "\n"))
   (setq fichier (@@ fichier -"/" 0))
   (println fichier)
   (fwrite (+ _current "../src/" fichier) lecode)
)


(defun boucle (fichiers)
   (loop f fichiers
      (check 
         (and
            (not (in f "old"))
            (not (in f "DS"))
            (not (in f "editor"))
         )         
         (traite f)         
      )
   )
)


(boucle rep)

