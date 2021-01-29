; This code executes a Python program  from within LispE
(use 'pylispe)

; we create a python intepreter
(setq py (python))

;We set a path to the current directory
(python_setpath py _current)

;We load a python program
(python_runfile py (+ _current "tktest.py"))




























