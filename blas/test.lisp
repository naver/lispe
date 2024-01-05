(use "lispe_blas")
(setq v1 (iota 100))
(println (blas_asum v1))

(setq v2 (iota 100))
(println (blas_axpy v1 v2))

(setq d1 (range 1 100 1.0))
(setq d2 (range 2 200 2.0))
(println (blas_dot d1 d2 1 1))
(println (blas_dotu d1 d2 1 1))

(println (blas_iamax d1))
(println (blas_nrm2 d2))

(println (blas_scal d1 2.1))

(setq A (numbers 1 2 3 4 5 6))
(setq x (numbers 9 10 11)) 
(setq y (numbers 7 8))

(println (blas_gemv A 2 3 2 x 1 y 1))


