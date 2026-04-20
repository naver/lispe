;Date: 01/04/2026
;Author: Claude Roux
;Description: Base Class


(load (+ _current "basic.lisp")) 
(load (+ _current "transpiler.lisp"))

(setq code `

class Test

  def __init__(self, x, y, z)
        self.k = x
        self.v = y
  enddef

  def func(self, x)
	self.u = x
  enddef
endclass

s = 'rte"qsdk'

`)


(setq code `
s = appel()
 `
)

(setq c (injecte_labels (+ code "\n")))

;(println . tokenize_rules parser_tok (+ code "\n"))
;(compile c)

(prettify  (transpile c))











