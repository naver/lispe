; This code executes a Python program  from within LispE
(use 'pylispe)

; we create a python intepreter
(setq py (python))

;We set a path to the current directory
(python_setpath py _current)

;We load a python program
(python_import py "called_in_lispe")

; We execute the function loaded with the program
(println (python_execute py "testing" '(20)))










