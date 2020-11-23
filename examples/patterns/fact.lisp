
; A factorial example based on pattern function definition

(defpat fact(1) 1)
(defpat fact ( (integer_ x)) (* x (fact (- x 1))))


(fact 15)















