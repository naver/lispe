;Date: 18/10/2022
;Author: Claude Roux
;Description: CFG

(load (+ _current "compile_rules.lisp"))
(load (+ _current "apply_rules.lisp"))

(setq rules 
   `S : NP VP
   PP : PREP NNP
   NP : DETM NOUNM
   NP : DETM NOUNM PP
   NP : DETM  NOUNM ADJM
   NP : DETM ADJPREM NOUNM
   NP : DETM  NOUNM ADJM PP
   NP : DETM ADJPREM NOUNM PP
   NNP : DETM NLOCM
   NNP : DETM ADJPREM NLOCM
   NNP : DETM NLOCM ADJM
   NNP : DETM NLOCM PP
   NNP : DETM ADJPREM NLOCM PP
   NNP : DETM NLOCM ADJM PP
   NP : DETF NOUNF
   NP : DETF NOUNF PP
   NP : DETF  NOUNF ADJF
   NP : DETF ADJPREF NOUNF
   NP : DETF  NOUNF ADJF PP
   NP : DETF ADJPREF NOUNF PP
   NNP : DETF NLOCF
   NNP : DETF ADJPREF NLOCF
   NNP : DETF NLOCF ADJF
   NNP : DETF NLOCF PP
   NNP : DETF ADJPREF NLOCF PP
   NNP : DETF NLOCF ADJF PP
   VP : VERB NP
   DETM : "le"  "un" "ce"
   DETF : "la"  "une" "cette"
   NOUNM : "chat"  "chien" "cheval" "chaton"
   NOUNF : "chatte"  "chienne" "jument" "chatonne"
   NLOCF : "maison" "grange" "ville"
   NLOCM : "patio" "hôtel" "restaurant"
   VERB : "carresse"  "chasse"  "mord" "voit"
   PREP : "de" "dans" "depuis"
   ADJPREM : "grand" "petit" "jeune" "vieux" "joli"
   ADJPREF : "grande" "petite"  "jeune" "vieille" "jolie"
   ADJM : "bleu" "rouge" "jaune"
   ADJF : "bleue" "rouge" "jaune"
   `
)

(compile rules)

(defun remplace(s)
   (setq s (replace s "de un" "d'un"))
   (setq s (replace s "de le" "du"))
   s
)

; we start with an incomplete sequence of words
(loopcount 50
   (println (remplace (join (analyse "une jolie chatonne chasse") " ")))
)






