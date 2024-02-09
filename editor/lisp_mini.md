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

# Markdown Document for Listed Lisp Functions

Here's a summary of the listed Lisp functions along with brief descriptions:

1. `(append pathname txt)` — Concatenates txt into file: `pathname``.
2. `(apply 'operator list)` — Applies the operator to elements of the list.
3. `(at e pos)` — Returns the character at position `pos` within the string `e`.
4. `(atom str)` — Returns `str` as an atom.
5. `(atom? e)` — Same as `(atom e)`, tests whether `e` is an atom.
6. `(base value base conversion)` — Convert `value` to base `conversion` if specified, otherwise convert to base 10.
7. `(car l)` — Returns the car (first element) of  `l`.
8. `(cdr l)` — Returns the cdr (rest of elements) of  `l`.
9. `(chr code)` — Returns a character represented by the integer `code`.
10. `(command example)` — Runs Unix command `example` and returns its output.
11. `(cond ((test) code) ((test) code)...)` — Conditional evaluation based on truthiness of expressions.
12. `(cons e l)` — Creates a cons cell containing `e` and `l`.
13. `(cons? l)` — Tests whether `l` is a cons cell.
14. `(defun name(p1 p2..) code)` — Defines a function named `name` taking arguments `p1`, `p2`, etc., and executing `code`.
15. `(!= e v)` — Tests whether `e` is not equal to `v`.
16. `(/ e v)` — Divides `e` by `v`.
17. `(eq e val)` — Tests whether `e` is equivalent to `val`.
18. `(equal e val)` — Tests whether `e` is equal to `val`.
19. `(eval expression)` — Evaluates the expression passed as argument.
20. `(filtercar 'operator list)` — Filters the list `list` using the predicate `operator`.
21. `(find ct val)` — Searches for the first occurrence of `ct` in `val`.
22. `(float v)` — Floats the numeric value `v`.
23. `(if pred then else)` — Conditionally evaluates `then` if `pred` is true, otherwise evaluates `else`.
24. `(< e1 e2)` — Tests whether `e1` is less than `e2`.
25. `(<= e1 e2)` — Tests whether `e1` is less than or equal to `e2`.
26. `(integer x)` — Parses `x` as an integer.
27. `(join lst sep)` — Joins the elements of `lst` separated by `sep`.
28. `(insert map key value)` — insert the `value` at position `key` in map.
29. `(insert lst pos value)` — insert the `value` at position `key` in lst.
29. `(list a1 a2 ...)` — Constructs a list containing `a1`, `a2`, etc.
30. `(loop a lst code)` — Iterative construct similar to C++ loops.
31. `(map (key value) (key value) ..)` — Creates a map with a series of key/value
32. `(- a1 a2 ...)` — Subtracts `a2` from `a1`, continuing leftward.
33. `(% a1 a2 ..)` — Performs modulo operation between `a1` and `a2`, continuing leftward.
34. `(* a1 a2 ..)` — Multiplies `a1` by `a2`, continuing leftward.
34. `(nconc l ll)` - Concatenates `ll` in `l`
35. `(neq e val)` — Tests whether `e` is not equal to `val`.
36. `(not v)` — Logically negates the boolean value `v`.
37. `(null? value)` — Tests whether `value` is null.
38. `(number? value)` — Tests whether `value` is a number.
39. `(ord str)` — Returns the ordinal value of all characters in `str`.
40. `(+ a1 a2 ..)` — Adds `a1` to `a2`, continuing leftward.
41. `(pop lst k)` — Removes the `k`th element from `lst`.
42. `(push lst v)` — Appends `v` to the end of `lst`.
43. `(range init limit increment)` — Generates a range of numbers.
44. `(read pathname)` — Reads contents of `pathname` as a Lisp object.
45. `(replace s a v)` — Replaces all instances of `a` in `s` with `v`.
46. `(setq n value)` — Sets the variable `n` to `value`.
47. `(sort lst true/nil)` — Sorts the list `lst` in ascending/descending order.
48. `(split e splitter)` — Splits `e` at the delimiter `splitter`.
49. `(string e)` — Converts `e` to a string.
50. `(string? v)` — Tests whether `v` is a string.
51. `(sub s beg (end))` — Extracts substring from `s` beginning at index `beg` up to `end`.
52. `(> e val)` — Tests whether `e` is greater than `val`.
53. `(>= e val)` — Tests whether `e` is greater than or equal to `val`.
54. `(trim str)` — Trims leading and trailing whitespaces from `str`.
55. `(type e)` — Determines the type of `e`.
56. `(while condition code)` — Executes `code` repeatedly until `condition` becomes false.
57. `(write pathname txt)` — Writes the content of `txt` to `pathname`.
58. `(zero? v)` — Tests whether `v` is zero.
58. `(zip l1 l2 l3...)` — combines the elements from each input list into a list of tuples
59. `((lambda (a1 a2) code) v1 v2)` — Anonymous function call with arguments `v1` and `v2`.


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

