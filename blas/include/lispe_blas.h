/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_blas.cxx
//

#ifndef lispe_blas_h
#define lispe_blas_h

typedef enum {blas_asum,  blas_axpy, blas_dot, blas_dotu, blas_iamax, blas_nrm2, blas_scale, blas_gemv,
 blas_ger, blas_geru, blas_hemv, blas_her, blas_her2, blas_symv, blas_syr, blas_syr2,
 blas_trmv, blas_trsv,
 blas_gemm, blas_hemm, blas_symm, blas_herk, blas_syrk, blas_her2k, blas_syr2k, blas_trmm, blas_trsm  
} blas_t;

class Lispe_blas : public Element {
public:
    blas_t action;

    Lispe_blas(blas_t a) : action(a), Element(l_lib) {}

    Element* eval(LispE* lisp);

    //We use this instruction to return a description of the instruction
    //Indeed, just do: (print getenv) to get this information
    wstring asString(LispE* lisp);
    Element* asum(LispE* lisp);
    Element* axpy(LispE* lisp);
    Element* dot(LispE* lisp);
    Element* dotu(LispE* lisp);
    Element* iamax(LispE* lisp);
    Element* nrm2(LispE* lisp);
    Element* scale(LispE* lisp);
    Element* gemv(LispE* lisp);
    Element* ger(LispE* lisp);
    Element* geru(LispE* lisp);
    Element* hemv(LispE* lisp);
    Element* her(LispE* lisp);
    Element* her2(LispE* lisp);
    Element* symv(LispE* lisp);
    Element* syr(LispE* lisp);
    Element* syr2(LispE* lisp);
    Element* trmv(LispE* lisp);
    Element* trsv(LispE* lisp);
    Element* gemm(LispE* lisp);
    Element* hemm(LispE* lisp);
    Element* symm(LispE* lisp);
    Element* herk(LispE* lisp);
    Element* syrk(LispE* lisp);
    Element* her2k(LispE* lisp);
    Element* syr2k(LispE* lisp);
    Element* trmm(LispE* lisp);
    Element* trsm(LispE* lisp);
};

#endif


