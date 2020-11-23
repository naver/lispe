# This code executes some LispE code within Python

import pylispe

# we create a lisp interpreter
a=pylispe.lisp()

# We load the current file

print(a.load("called_in_python.lisp"))

# an evaluation
print(a.eval("(setq d 10)"))


# We can actually call an instruction or an operator directly
# Note the '".."' structure to pass strings to LispE, while
# atoms are passed as simple strings: the 'd' is the atom
# that was created above in the 'setq' instruction
print(a.execute("+", '"test"', 'd'))

# We can also use the add function that has been implemented in called.lisp
# The 'd' is again the one that was declared above with the value 10


print(a.execute("add", "d", 31.654))


