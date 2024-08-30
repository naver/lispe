# Another Lisp Interpreter

This _lisp interpreter_ is the smallest interpreter that I could implement.

It is mainly implemented in the following files:

1. [minilisp.h](https://github.com/naver/lispe/tree/master/editor/include/minilisp.h)
1. [minilisp.cxx](https://github.com/naver/lispe/tree/master/editor/src/minilisp.cxx)
1. [compiling.cxx](https://github.com/naver/lispe/tree/master/editor/src/compiling.cxx)
1. [eval.cxx](https://github.com/naver/lispe/tree/master/editor/src/eval.cxx)
1. [tokens.cxx](https://github.com/naver/lispe/tree/master/editor/src/tokens.cxx)
1. [tools.cxx](https://github.com/naver/lispe/tree/master/editor/src/tools.cxx)

Basically, each type of objects is implemented as specific class.
Every time an object is created, it is automatically recorded in the _garbages_ variable, which is part of the *lisp_mini* class.
When a function or lambda is executed, a local _garbages_ and a local _variables_ is created to keep track of each value.
When the execution of a lambda or a function is terminated, these elements are deleted.

## Compiling

You can compile two different versions:

1. [minimain.cxx](https://github.com/naver/lispe/tree/master/editor/src/minimain.cxx) contains a specific implementation to execute lisp mini instructions. This is given as an example to help you better understand how to proceed (use `make mini`).
1. [lispminimain.cxx](https://github.com/naver/lispe/tree/master/editor/src/lispminimain.cxx) contains a specific implementation to execute lisp mini instructions (make `lispmini`). The interface is very very limited to one instruction per line.


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

1. First, we record an `l_cons` in `lisp_instruction_code` enum (see `minilisp.h`)
1. Second, we define its actual name in `initialisation_static_values()` : `code_dictionary["cons"] = cons;` (see `compiling.cxx`)
1. Third, we introduce its code as a `case cons:` in `lisp_list::eval()` (see `eval.cxx`)

You can then easily add any function you want.

## LISP Functions Description

Here are the descriptions for each available LISP function.

**Note** that their implementation is in `eval.cxx`.

1. `(append pathname txt)` — Concatenates txt into file: `pathname``.
1. `(apply 'operator list)` — Applies the operator to elements of the list.
1. `(at e pos)` or `(@ e pos)` — Returns the character at position `pos` within the string `e`.
1. `(atom str)` — Returns `str` as an atom.
1. `(atom? e)` — Same as `(atom e)`, tests whether `e` is an atom.
1. `(base value base conversion)` — Convert `value` to base `conversion` if specified, otherwise convert to base 10.
1. `(car l)` — Returns the car (first element) of  `l`.
1. `(cdr l)` — Returns the cdr (rest of elements) of  `l`.
1. `(chr code)` — Returns a character represented by the integer `code`.
1. `(command example)` — Runs Unix command `example` and returns its output.
1. `(cond ((test) code) ((test) code)...)` — Conditional evaluation based on truthiness of expressions.
1. `(cons e l)` — Creates a cons cell containing `e` and `l`.
1. `(cons? l)` — Tests whether `l` is a cons cell.
1. `(defun name(p1 p2..) code)` — Defines a function named `name` taking arguments `p1`, `p2`, etc., and executing `code`.
1. `(!= e v)` — Tests whether `e` is not equal to `v`.
1. `(/ e v)` — Divides `e` by `v`.
1. `(eq e val)` — Tests whether `e` is equivalent to `val`.
1. `(equal e val)` — Tests whether `e` is equal to `val`.
1. `(eval expression)` — Evaluates the expression passed as argument.
1. `(filtercar 'function cnt)` — Filters the container `cnt` with predicate `function`. Returns a list. *Note that for `maps`, `function` should have two arguments.*
1. `(find ct val)` — Searches for the first occurrence of `ct` in `val`.
1. `(float v)` — Floats the numeric value `v`.
1. `(if pred then else)` — Conditionally evaluates `then` if `pred` is true, otherwise evaluates `else`.
1. `(< e1 e2)` — Tests whether `e1` is less than `e2`.
1. `(<= e1 e2)` — Tests whether `e1` is less than or equal to `e2`.
1. `(integer x)` — Parses `x` as an integer.
1. `(join lst sep)` — Joins the elements of `lst` separated by `sep`.
1. `(insert map key value)` — insert the `value` at position `key` in map.
1. `(insert lst pos value)` — insert the `value` at position `key` in lst.
1. `(list a1 a2 ...)` — Constructs a list containing `a1`, `a2`, etc.
1. `(loop a lst code)` — Iterative construct similar to C++ loops.
1. `(map (key value) (key value) ..)` — Creates a map with a series of key/value
1. `(mapcar 'function cnt)` — applies function to cnt. Returns a list. *Note that for `maps`, function should have two arguments.*
1. `(- a1 a2 ...)` — Subtracts `a2` from `a1`, continuing leftward.
1. `(% a1 a2 ..)` — Performs modulo operation between `a1` and `a2`, continuing leftward.
1. `(* a1 a2 ..)` — Multiplies `a1` by `a2`, continuing leftward.
1. `(nconc l ll)` - Concatenates `ll` in `l`
1. `(neq e val)` — Tests whether `e` is not equal to `val`.
1. `(not v)` — Logically negates the boolean value `v`.
1. `(null? value)` — Tests whether `value` is null.
1. `(number? value)` — Tests whether `value` is a number.
1. `(ord str)` — Returns the ordinal value of all characters in `str`.
1. `(+ a1 a2 ..)` — Adds `a1` to `a2`, continuing leftward.
1. `(pop lst k)` — Removes the `k`th element from `lst`.
1. `(push lst v)` — Appends `v` to the end of `lst`.
1. `(put cnt key v)` — Puts a value in `cnt (map, list or string)` at position `key`
1. `(range init limit increment)` — Generates a range of numbers.
1. `(read pathname)` — Reads contents of `pathname` as a Lisp object.
1. `(replace s a v)` — Replaces all instances of `a` in `s` with `v`.
1. `(setq n value)` — Sets the variable `n` to `value`.
1. `(sort lst true/nil)` — Sorts the list `lst` in ascending/descending order.
1. `(split e splitter)` — Splits `e` at the delimiter `splitter`.
1. `(string e)` — Converts `e` to a string.
1. `(string? v)` — Tests whether `v` is a string.
1. `(sub s beg (end))` — Extracts substring from `s` beginning at index `beg` up to `end`.
1. `(> e val)` — Tests whether `e` is greater than `val`.
1. `(>= e val)` — Tests whether `e` is greater than or equal to `val`.
1. `(trim str)` — Trims leading and trailing whitespaces from `str`.
1. `(type e)` — Determines the type of `e`.
1. `(while condition code)` — Executes `code` repeatedly until `condition` becomes false.
1. `(write pathname txt)` — Writes the content of `txt` to `pathname`.
1. `(zero? v)` — Tests whether `v` is zero.
1. `(zip l1 l2 l3...)` — combines the elements from each input list into a list of tuples
1. `((lambda (a1 a2) code) v1 v2)` — Anonymous function call with arguments `v1` and `v2`. *lambdas* can also be written as:
    - `(\(a1 a2..) ..)`
    - `(λ(a1 a2..) ..)`


## Mathematical Functions

Here are descriptions of mini lisp mathematical functions:

1. `acos`: Returns the arc cosine of `arg`, which is the angle in radians whose cosine is `arg`. The result is between 0 and π (inclusive). If `arg` is outside the range [-1, 1], `acos` returns a NaN.
1. `acosh`: Returns the inverse hyperbolic cosine of `arg`, which is the principal branch of the arc cosine function applied to hyperbolic values.
1. `asin`: Returns the arc sine of `arg`, which is the angle in radians whose sine is `arg`. The result is between -π/2 and π/2 (inclusive). If `arg` is outside the range [-1, 1], `asin` returns a NaN.
1. `asinh`: Returns the inverse hyperbolic sine of `arg`, which is the principal branch of the arc sinh function applied to hyperbolic values.
1. `atan`: Returns the arc tangent of `y`, which is the angle in radians between the positive X-axis and the line connecting the origin and point (`x`, `y`). The result is between -π/2 and π/2 (exclusive). If `x` is omitted, `atan` computes the principal value of the arc tangent of `y`.
1. `atanh`: Returns the inverse hyperbolic tangent of `arg`, which is the principal branch of the arc tanh function applied to hyperbolic values.
1. `cbrt` or `∛`: Returns the cube root of `arg`.
1. `cos`: Returns the cosine of `angle` in radians.
1. `cosh`: Returns the hyperbolic cosine of `arg`.
1. `degree`: Converts radians to degrees.
1. `erf`: Returns the Gaussian error function of `z`, which is approximately equal to P(Z < z) where Z has a normal distribution with mean 0 and variance 1/2.
1. `erfc`: Returns the complementary error function of `z`, which is approximately equal to 1 - erf(z).
1. `exp`: Returns the exponentiation of `arg` to the base `e`.
1. `exp2`: Returns the exponentiation of `arg` to the base 2.
1. `expm1`: Returns `e^x - 1`, i.e., the natural exponential function minus 1.
1. `fabs`: Returns the absolute value of `arg`.
1. `floor`: Rounds down `arg` to the nearest integer.
1. `gcd`: Returns the greatest common divisor of `a` and `b`.
1. `hcf`: Synonym for gcd.
1. `lgamma`: Returns the logarithmic gamma function of `arg`, which is the limit as `x` approaches `arg` of (lnΓ(`x`) - ln(`x`) + (`x` - 1/2)).
1. `log`: Returns the natural logarithm of `arg`, using base `e`.
1. `log10`: Returns the base 10 logarithm of `arg`.
1. `log1p`: Returns the natural logarithm of (1 + `arg`).
1. `log2`: Returns the base 2 logarithm of `arg`.
1. `logb`: Returns the base `base` logarithm of `arg`.
1. `nearbyint`: Rounds `arg` to the nearest integer.
1. `radian`: Converts degrees to radians.
1. `rint`: Rounds `arg` to the nearest integer.
1. `round`: Rounds `arg` to the nearest integer.
1. `sin`: Returns the sine of `angle` in radians.
1. `sinh`: Returns the hyperbolic sine of `arg`.
1. `sqrt` or `√`: Returns the square root of `arg`.
1. `tan`: Returns the tangent of `angle` in radians.
1. `tanh`: Returns the hyperbolic tangent of `arg`.
1. `tgamma`: Returns the factorial gamma function of `arg`, which is the limit as `x` approaches `arg` of (x!(x - 1)).
1. `trunc`: Truncates `arg` to the nearest integer, discarding the fractional part.


### Mathematical Values:

1. `_pi` or `π`: _pi_ value (3.14159)
1. `_tau` or `τ`: 2 x π (6.28319)
1. `_e` or `ℯ`: the Euler value (2.71828)
1. `_phi` or `ϕ`: The Golden ratio (1.61803)

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

(mapcar (lambda (x y) (+ x y)) {"a":"b" "c":"d"})
(mapcar (lambda(x) (* x 2)) (range 1 10 1))
```

