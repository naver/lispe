; FIZZBUZZ IMPLEMENTATION

(defun checking (x y) (eq 0 (% y x)))

(defpat fizzbuzz ((integer_ (checking 15 x)))
   'fizzbuzz
)

(defpat fizzbuzz ((integer_ (checking 3 x)))
   'fizz
)

(defpat fizzbuzz ((integer_ (checking 5 x)))
   'buzz
)

(defpat fizzbuzz (x)
   x
)


(map 'fizzbuzz (range 1 100 1))















