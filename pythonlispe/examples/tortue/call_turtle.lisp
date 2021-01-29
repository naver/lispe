; This code executes a Python program  from within LispE
(use 'pylispe)

; we create a python intepreter
(setq py (python))

;We set a path to the current directory
(python_setpath py _current)

;We load a python program
(python_import py "tortue")

; We execute the function loaded with the program
(python_execute py "devant" '(20))
;(python_execute py "gauche" '(10))
;(python_execute py "devant" '(14))




















