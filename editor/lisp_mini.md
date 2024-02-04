# Another Lisp Interpreter

This _lisp interpreter_ is the smallest interpreter that I could implement.

It is mainly implemented in the following files:

1. [minilisp.h](https://github.com/naver/lispe/tree/master/editor/include/minilisp.h)
1. [minilisp.cxx](https://github.com/naver/lispe/tree/master/editor/src/minilisp.cxx)

Basically, each type of objects is implemented as specific class.
Every time an object is created, it is automatically recorded in the _garbages_ variable, which is part of the *lisp_mini* class.
When a function or lambda is executed, a local _garbages_ and a local _variables_ is created to keep track of each value.
When the execution of a lambda or a function is terminated, these elements are deleted.

## Creating a lisp interpreter

It is pretty straightforward:

```C++
#include "minilisp.h"

lisp_mini lisp;
```

You can create different interpreters in memory that won't overlap one over the others.

## Adding a new function

In [minilisp.h](https://github.com/naver/lispe/tree/master/editor/include/minilisp.h), you will find an enum that contains every single type and instructions that the system offers.

For each of these elements, an entry in *code_dictionary* is created.

For instance, to create the method: "cons":

1. First, we record an `l_cons` in the enum.
1. Second, we define its actual name in `initialisation_static_values()` : `code_dictionary["cons"] = cons;`
1. Third, we introduce its code as a `case cons:` in `lisp_list::eval()`.

You can then easily add any function you want.

## LISP Functions Description

Here are the descriptions for each LISP function mentioned in the provided by _lisp mini_:

1. `lambda` - This function represents the lambda (anonymous) function construct in LISP. It allows creating anonymous functions without defining them explicitly.

2. `defun` - The defun function is used to define new functions in LISP. It takes a name and an expression as arguments and creates a named function that can be called later.

3. `eval` - The eval function evaluates its argument(s), which could be a symbol or a list of symbols, and returns the result.

4. `+`, `-`, `*`, `/`, `%` - These functions represent basic arithmetic operations in LISP: addition (+), subtraction (-), multiplication (*), division (/), and modulo (%).

5. `car` - The car function extracts the first element from a cons cell, which is a common data structure in LISP.

6. `cdr` - The cdr function extracts the second element from a cons cell.

7. `cons` - The cons function creates a new cons cell with two elements: a value and a rest pointer.

8. `split` - The split function splits a string into multiple parts based on a specified delimiter.

9. `print` - The print function prints its argument(s) to the console or output stream.

10. `at` - The at function retrieves the nth element from a given list, where n is a positive integer.

11. `list` - The list function creates a new list by taking any number of arguments and returning a list containing those values.

12. `loop` - (loop constraint code) loop executes the code as long as the constraint is true.

13. `cond` - The cond function tests a series of conditions and executes the corresponding action if the condition is true. If no condition is met, it does nothing.

14. `if` - The if function checks whether a condition is true or false and performs different actions accordingly.

15. `eq` - The eq function compares two objects for equality. In LISP, eq checks if both objects have the same memory address.

16. `neq` - The neq function checks if two objects are not equal.

17. `<` - The inf function checks if one object is less than another.

18. `>` - The sup function checks if one object is greater than another.

19. `<=` - The infeq function checks if one object is less than or equal to another.

20. `>=` - The supeq function checks if one object is greater than or equal to another.

21. `setq` - The setq function assigns a value to a variable.

22. `quote` - The quote function marks its argument so that it will not be evaluated during execution.

23. `command` - The command function executes a shell command

24. `size` - The size function returns the length of a given list or of a string.

25. `block` - The block function defines a local scope for variables within a specific block of code.

26. `clean` - The clean function clears the screen or terminal window.

27. `type` - The type function determines the type of a given object.

28. `cons?` - The consp function checks if an object is a cons cell.

29. `zero?` - The zerop function checks if a number is zero.

30. `null?` - The nullp function checks if a list is empty.

31. `string?` - The stringp function checks if an object is a string.

32. `number?` - The numberp function checks if an object is a number.

33. `number` - The number function converts a string representation of a number to a numeric value.

34. `string` - The string function converts a string representation of a word to a string value.

35. `push` - The push function adds an element to the end of a list.

36. `pop` - The pop function removes the last element from a list.


# Examples

```Lisp
(+ 10 20 30 40)
((lambda (x y) (* x y)) 10 20)

(defun tst(a)
   (+ a 100)
)

(cons 'a '(b c d))

(setq r ())
(push r 10)
(print r)
```

