; This code executes some Python instructions

(use 'pylispe)

; we create a python intepreter
(setq py (python))

; We execute some code
(python_run py "i = 100 + 20")
(python_run py "print('Within:',i)")

; We define a Python function
; as a LispE string
(setq myFunc
`def toto(i):
   return i+10;
`
)

; we store our function definition
(python_run py myFunc)

; which we can execute and retrieve a result from it
(println (python_execute py "toto" '(11)))










