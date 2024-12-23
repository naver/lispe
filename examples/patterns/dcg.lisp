;Date: 03/06/2021
;Author: Claude Roux
;Description: CFG

(load (+ _current "compile_rules.lisp"))
(load (+ _current "apply_rules.lisp"))

(setq rules 
   `S : NP VP
   PP : PREP NNP
   NP : DET NOUN PP
   NP : DET ADJ NOUN
   NP : DET NOUN
   NNP : DET NLOC
   NNP : DET ADJ NLOC
   VP : VERB NP
   DET : "the"  "a" "this" "that"
   NOUN : "cat"  "dog"  "bat" "man" "woman" "child" "puppy"
   NLOC : "house" "barn" "flat" "city" "country"
   VERB : "eats"  "chases"  "bites" "sees"
   PREP : "of" "from" "in"
   ADJ : "big" "small" "blue" "red" "yellow" "petite"
   `
)

(compile rules)

; we start with an incomplete sequence of words
; we start with an incomplete sequence of words
(loopcount 50
   (println (join (analyse "a big") " "))
)










