/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_blas.cxx

/*
 This the template for new 'extensions' in lispe
 */

#include "lispe.h"
#include "lispe_blas.h"
#include "listes.h"
#include <cblas.h>

#ifdef WIN32
#if (_MSC_VER >= 1900)
#pragma comment(lib, "legacy_stdio_definitions.lib")
FILE _iob[] = {*stdin, *stdout, *stderr};
extern "C" FILE *__cdecl __iob_func(void) { return _iob; }
#endif
#endif

Element *Lispe_blas::asum(LispE *lisp)
{
    Element *argument = lisp->get_variable(L"x");
    long incx = lisp->get_variable(L"incx")->asInteger();

    if(incx <= 0) {
        throw new Error("Error: 'incx' should be greater than 0");
    }

    switch (argument->type)
    {
    case t_shorts:
    {
        // There's no equivalent C BLAS API for int16_t type
        throw new Error("Error: 'blas_asum' does not apply to shorts_");
    }
    case t_integers:
    {
        // There's no equivalent C BLAS API for long type
        throw new Error("Error: 'blas_asum' does not apply to integers_");
    }
    case t_floats:
    {
        float *v = ((Floats *)argument)->liste.items->buffer;
        int n = argument->size();
        if(n <= 0) {
            throw new Error("Error: 'n' should be greater than 0");
        }
        float val = cblas_sasum(n, v, incx);
        return lisp->provideFloat(val);
    }
    case t_numbers:
    {
        double *v = ((Numbers *)argument)->liste.items->buffer;
        int n = argument->size();
        if(n <= 0) {
            throw new Error("Error: 'n' should be greater than 0");
        }
        double val = cblas_dasum(n, v, incx);
        return lisp->provideNumber(val);
    }
    default:
        throw new Error("Error: 'blas_asum' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::iamax(LispE *lisp)
{
    Element *argument = lisp->get_variable(L"x");
    long incx = lisp->get_variable(L"incx")->asInteger();

    if (incx <= 0) {
        throw new Error("Error: 'incx' should be a positive integer");
    }

    switch (argument->type)
    {
    case t_shorts:
    case t_integers:
        throw new Error("Error: 'blas_iamax' does not apply to shorts_ and integers_");
    case t_floats:
    {
        float *v = ((Floats *)argument)->liste.items->buffer;
        int size = argument->size();
        if (size <= 0) {
            throw new Error("Error: 'x' should be a non-empty array");
        }
        long val = cblas_isamax(size, v, incx);
        return lisp->provideInteger(val);
    }
    case t_numbers:
    {
        double *v = ((Numbers *)argument)->liste.items->buffer;
        int size = argument->size();
        if (size <= 0) {
            throw new Error("Error: 'x' should be a non-empty array");
        }
        long val = cblas_idamax(size, v, incx);
        return lisp->provideInteger(val);
    }
    default:
        throw new Error("Error: 'blas_iamax' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::nrm2(LispE *lisp)
{
    Element *argument = lisp->get_variable(L"x");
    long incx = lisp->get_variable(L"incx")->asInteger();

    if (incx <= 0)
    {
        throw new Error("Error: 'incx' must be a positive integer");
    }

    switch (argument->type)
    {
    case t_floats:
    {
        float *v = ((Floats *)argument)->liste.items->buffer;
        if (v == NULL)
        {
            throw new Error("Error: Null pointer encountered in input array");
        }
        float val = cblas_snrm2(argument->size(), v, incx);
        return lisp->provideFloat(val);
    }
    case t_numbers:
    {
        double *v = ((Numbers *)argument)->liste.items->buffer;
        if (v == NULL)
        {
            throw new Error("Error: Null pointer encountered in input array");
        }
        double val = cblas_dnrm2(argument->size(), v, incx);
        return lisp->provideNumber(val);
    }
    default:
        throw new Error("Error: 'blas_nrm2' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::scale(LispE *lisp)
{
    Element *argument = lisp->get_variable(L"x");
    long incx = lisp->get_variable(L"incx")->asInteger();

    if(incx <= 0)
    {
        throw new Error("Error: 'incx' should be a positive integer");
    }

    switch (argument->type)
    {
    case t_floats:
    {
        float *v = ((Floats *)argument)->liste.items->buffer;
        float alpha = lisp->get_variable(L"scale")->asFloat();
        
        if(alpha <= 0)
        {
            throw new Error("Error: 'alpha' should be a positive float");
        }

        cblas_sscal(argument->size(), alpha, v, incx);
        return argument;
    }
    case t_numbers:
    {
        double *v = ((Numbers *)argument)->liste.items->buffer;
        double alpha = lisp->get_variable(L"scale")->asNumber();

        if(alpha <= 0)
        {
            throw new Error("Error: 'alpha' should be a positive number");
        }

        cblas_dscal(argument->size(), alpha, v, incx);
        return argument;
    }
    default:
        throw new Error("Error: 'blas_scal' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::axpy(LispE *lisp)
{
    Element *argument1 = lisp->get_variable(L"x");
    Element *argument2 = lisp->get_variable(L"y");
    if (argument1->type != argument2->type)
        throw new Error("Error: Vectors should be of the same type");

    int64_t sz = argument1->size();
    if (sz != argument2->size())
        throw new Error("Error: Vectors should have the same size");

    long incx = lisp->get_variable(L"incx")->asInteger();
    long incy = lisp->get_variable(L"incy")->asInteger();

    switch (argument1->type)
    {
    case t_floats:
    {
        float *xbuff = ((Floats *)argument1)->liste.items->buffer;
        Floats *result = new Floats((Floats *)argument2);
        float *ybuff = ((Floats *)result)->liste.items->buffer;
        float alpha = 1.0;
        cblas_saxpy(sz, alpha, xbuff, incx, ybuff, incy);
        return result;
    }
    case t_numbers:
    {
        double *xbuff = ((Numbers *)argument1)->liste.items->buffer;
        Numbers *result = new Numbers((Numbers *)argument2);
        double *ybuff = ((Numbers *)result)->liste.items->buffer;
        double alpha = 1.0;
        cblas_daxpy(sz, alpha, xbuff, incx, ybuff, incy);
        return result;
    }
    default:
        throw new Error("Error: 'blas_axpy' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::dot(LispE *lisp)
{
    Element *argument1 = lisp->get_variable(L"x");
    Element *argument2 = lisp->get_variable(L"y");
    if (argument1->type != argument2->type)
        throw new Error("Error: Vectors should be of the same type");

    int64_t sz = argument1->size();
    if (sz != argument2->size())
        throw new Error("Error: Vectors should have the same size");

    long incx = lisp->get_variable(L"incx")->asInteger();
    long incy = lisp->get_variable(L"incy")->asInteger();

    switch (argument1->type)
    {
    case t_floats:
    {
        float *xbuff = ((Floats *)argument1)->liste.items->buffer;
        float *ybuff = ((Floats *)argument2)->liste.items->buffer;

        float val = cblas_sdot(sz, xbuff, incx, ybuff, incy);
        if (val == 0)
        {
            throw new Error("Error: cblas_sdot failed");
        }
        return lisp->provideFloat(val);
    }
    case t_numbers:
    {
        double *xbuff = ((Numbers *)argument1)->liste.items->buffer;
        double *ybuff = ((Numbers *)argument2)->liste.items->buffer;

        double val = cblas_ddot(sz, xbuff, incx, ybuff, incy);
        if (val == 0)
        {
            throw new Error("Error: cblas_ddot failed");
        }
        return lisp->provideNumber(val);
    }
    default:
        throw new Error("Error: 'blas_dot' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::dotu(LispE *lisp)
{
    Element *argument1 = lisp->get_variable(L"x");
    Element *argument2 = lisp->get_variable(L"y");
    if (argument1->type != argument2->type)
        throw new Error("Error: Vectors should be of the same type");

    int64_t sz = argument1->size();
    if (sz != argument2->size())
        throw new Error("Error: Vectors should have the same size");

    long incx = lisp->get_variable(L"incx")->asInteger();
    long incy = lisp->get_variable(L"incy")->asInteger();

    switch (argument1->type)
    {
    case t_floats:
    {
        float *xbuff = ((Floats *)argument1)->liste.items->buffer;
        float *ybuff = ((Floats *)argument2)->liste.items->buffer;
        if (xbuff == NULL || ybuff == NULL)
            throw new Error("Error: Null pointer exception");
        float val = cblas_sdot(sz, xbuff, incx, ybuff, incy);
        return lisp->provideFloat(val);
    }
    case t_numbers:
    {
        double *xbuff = ((Numbers *)argument1)->liste.items->buffer;
        double *ybuff = ((Numbers *)argument2)->liste.items->buffer;
        if (xbuff == NULL || ybuff == NULL)
            throw new Error("Error: Null pointer exception");
        double val = cblas_ddot(sz, xbuff, incx, ybuff, incy);
        return lisp->provideNumber(val);
    }
    default:
        throw new Error("Error: 'blas_dotu' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::gemv(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *x = lisp->get_variable(L"x");
    Element *y = lisp->get_variable(L"y");
    if (x->type != y->type || A->type != x->type)
        throw new Error("Error: Vectors should be of the same type");

    long m = lisp->get_variable(L"m")->asInteger();
    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long incx = lisp->get_variable(L"incx")->asInteger();
    long incy = lisp->get_variable(L"incy")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    long trans = lisp->get_variable(L"trans")->asInteger();

    if (A->size() != m * n)
        throw new Error("Error: the size of A does not match mxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    CBLAS_TRANSPOSE op;
    switch (trans)
    {
    case 0:
        if (x->size() != n)
            throw new Error("Error: size of x should be n");
        if (y->size() != m)
            throw new Error("Error: size of y should be m");
        op = CblasNoTrans;
        break;
    case 1:
        if (x->size() != m)
            throw new Error("Error: size of x should be m");
        if (y->size() != n)
            throw new Error("Error: size of y should be n");
        op = CblasTrans;
        break;
    default:
        if (x->size() != m)
            throw new Error("Error: size of x should be m");
        if (y->size() != n)
            throw new Error("Error: size of y should be n");
        op = CblasConjTrans;
    }

    switch (x->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *xbuff = ((Floats *)x)->liste.items->buffer;
        float *ybuff = ((Floats *)y)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        float beta = lisp->get_variable(L"beta")->asFloat();

        cblas_sgemv(lay, op, m, n, alpha, a, lda, xbuff, incx, beta, ybuff, incy);
        return y;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *xbuff = ((Numbers *)x)->liste.items->buffer;
        double *ybuff = ((Numbers *)y)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        double beta = lisp->get_variable(L"beta")->asNumber();

        cblas_dgemv(lay, op, m, n, alpha, a, lda, xbuff, incx, beta, ybuff, incy);
        return y;
    }
    default:
        throw new Error("Error: 'blas_gemv' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::ger(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *x = lisp->get_variable(L"x");
    Element *y = lisp->get_variable(L"y");
    if (x->type != y->type || A->type != x->type)
        throw new Error("Error: Vectors should be of the same type");

    long m = lisp->get_variable(L"m")->asInteger();
    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long incx = lisp->get_variable(L"incx")->asInteger();
    long incy = lisp->get_variable(L"incy")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();

    if (A->size() != m * n)
        throw new Error("Error: the size of A does not match mxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    if (x->size() != m)
        throw new Error("Error: size of x should be m");
    if (y->size() != n)
        throw new Error("Error: size of y should be n");

    switch (x->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *xbuff = ((Floats *)x)->liste.items->buffer;
        float *ybuff = ((Floats *)y)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        cblas_sger(lay, m, n, alpha, xbuff, incx, ybuff, incy, a, lda);
        return A;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *xbuff = ((Numbers *)x)->liste.items->buffer;
        double *ybuff = ((Numbers *)y)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        cblas_dger(lay, m, n, alpha, xbuff, incx, ybuff, incy, a, lda);
        return A;
    }
    default:
        throw new Error("Error: 'blas_ger' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::geru(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *x = lisp->get_variable(L"x");
    Element *y = lisp->get_variable(L"y");
    if (x->type != y->type || A->type != x->type)
        throw new Error("Error: Vectors should be of the same type");

    long m = lisp->get_variable(L"m")->asInteger();
    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long incx = lisp->get_variable(L"incx")->asInteger();
    long incy = lisp->get_variable(L"incy")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();

    if (A->size() != m * n)
        throw new Error("Error: the size of A does not match mxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    if (x->size() != m)
        throw new Error("Error: size of x should be m");
    if (y->size() != n)
        throw new Error("Error: size of y should be n");

    switch (x->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *xbuff = ((Floats *)x)->liste.items->buffer;
        float *ybuff = ((Floats *)y)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        cblas_sger(lay, m, n, alpha, xbuff, incx, ybuff, incy, a, lda);
        return A;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *xbuff = ((Numbers *)x)->liste.items->buffer;
        double *ybuff = ((Numbers *)y)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        cblas_dger(lay, m, n, alpha, xbuff, incx, ybuff, incy, a, lda);
        return A;
    }
    default:
        throw new Error("Error: 'blas_geru' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::hemv(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *x = lisp->get_variable(L"x");
    Element *y = lisp->get_variable(L"y");
    if (x->type != y->type || A->type != x->type)
        throw new Error("Error: Vectors should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long incx = lisp->get_variable(L"incx")->asInteger();
    long incy = lisp->get_variable(L"incy")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    bool uplo = lisp->get_variable(L"uplo")->Boolean();

    if (A->size() != n * n)
        throw new Error("Error: the size of A does not match nxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    if (x->size() != n)
        throw new Error("Error: size of x should be m");
    if (y->size() != n)
        throw new Error("Error: size of y should be n");

    switch (x->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *xbuff = ((Floats *)x)->liste.items->buffer;
        float *ybuff = ((Floats *)y)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        float beta = lisp->get_variable(L"beta")->asFloat();

        cblas_ssymv(lay, up, n, alpha, a, lda, xbuff, incx, beta, ybuff, incy);
        return y;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *xbuff = ((Numbers *)x)->liste.items->buffer;
        double *ybuff = ((Numbers *)y)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        double beta = lisp->get_variable(L"beta")->asNumber();

        cblas_dsymv(lay, up, n, alpha, a, lda, xbuff, incx, beta, ybuff, incy);
        return y;
    }
    default:
        throw new Error("Error: 'blas_hemv' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::symv(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *x = lisp->get_variable(L"x");
    Element *y = lisp->get_variable(L"y");
    if (x->type != y->type || A->type != x->type)
        throw new Error("Error: Vectors should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long incx = lisp->get_variable(L"incx")->asInteger();
    long incy = lisp->get_variable(L"incy")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    bool uplo = lisp->get_variable(L"uplo")->Boolean();

    if (A->size() != n * n)
        throw new Error("Error: the size of A does not match nxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;
    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    if (x->size() != n)
        throw new Error("Error: size of x should be m");
    if (y->size() != n)
        throw new Error("Error: size of y should be n");

    switch (x->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *xbuff = ((Floats *)x)->liste.items->buffer;
        float *ybuff = ((Floats *)y)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        float beta = lisp->get_variable(L"beta")->asFloat();
        cblas_ssymv(lay, up, n, alpha, a, lda, xbuff, incx, beta, ybuff, incy);
        return y;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *xbuff = ((Numbers *)x)->liste.items->buffer;
        double *ybuff = ((Numbers *)y)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        double beta = lisp->get_variable(L"beta")->asNumber();
        cblas_dsymv(lay, up, n, alpha, a, lda, xbuff, incx, beta, ybuff, incy);
        return y;
    }
    default:
        throw new Error("Error: 'blas_symv' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::syr(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *x = lisp->get_variable(L"x");
    if (A->type != x->type)
        throw new Error("Error: Vectors should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long incx = lisp->get_variable(L"incx")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    bool uplo = lisp->get_variable(L"uplo")->Boolean();

    if (A->size() != n * n)
        throw new Error("Error: the size of A does not match nxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    if (x->size() != n)
        throw new Error("Error: size of x should be m");

    switch (x->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *xbuff = ((Floats *)x)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        cblas_ssyr(lay, up, n, alpha, xbuff, incx, a, lda);
        return A;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *xbuff = ((Numbers *)x)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        cblas_dsyr(lay, up, n, alpha, xbuff, incx, a, lda);
        return A;
    }
    default:
        throw new Error("Error: 'blas_syr' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::her(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *x = lisp->get_variable(L"x");
    if (A->type != x->type)
        throw new Error("Error: Vectors should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long incx = lisp->get_variable(L"incx")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    bool uplo = lisp->get_variable(L"uplo")->Boolean();

    if (A->size() != n * n)
        throw new Error("Error: the size of A does not match nxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    if (x->size() != n)
        throw new Error("Error: size of x should be m");

    switch (x->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *xbuff = ((Floats *)x)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        if(a == NULL || xbuff == NULL)
            throw new Error("Error: Null pointer exception");
        cblas_sher(lay, up, n, alpha, xbuff, incx, a, lda);
        return A;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *xbuff = ((Numbers *)x)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        if(a == NULL || xbuff == NULL)
            throw new Error("Error: Null pointer exception");
        cblas_dher(lay, up, n, alpha, xbuff, incx, a, lda);
        return A;
    }
    default:
        throw new Error("Error: 'blas_her' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::her2(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *x = lisp->get_variable(L"x");
    Element *y = lisp->get_variable(L"y");
    if (x->type != y->type || A->type != x->type)
        throw new Error("Error: Vectors should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long incx = lisp->get_variable(L"incx")->asInteger();
    long incy = lisp->get_variable(L"incy")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    bool uplo = lisp->get_variable(L"uplo")->Boolean();

    if (A->size() != n * n)
        throw new Error("Error: the size of A does not match nxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    if (x->size() != n)
        throw new Error("Error: size of x should be m");
    if (y->size() != n)
        throw new Error("Error: size of y should be n");

    switch (x->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *xbuff = ((Floats *)x)->liste.items->buffer;
        float *ybuff = ((Floats *)y)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        cblas_sher2(CblasColMajor, CblasUpper, n, alpha, xbuff, incx, ybuff, incy, a, lda);
        return A;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *xbuff = ((Numbers *)x)->liste.items->buffer;
        double *ybuff = ((Numbers *)y)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        cblas_dher2(CblasColMajor, CblasUpper, n, alpha, xbuff, incx, ybuff, incy, a, lda);
        return A;
    }
    default:
        throw new Error("Error: 'blas_her2' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::syr2(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *x = lisp->get_variable(L"x");
    Element *y = lisp->get_variable(L"y");
    if (x->type != y->type || A->type != x->type)
        throw new Error("Error: Vectors should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long incx = lisp->get_variable(L"incx")->asInteger();
    long incy = lisp->get_variable(L"incy")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    bool uplo = lisp->get_variable(L"uplo")->Boolean();

    if (A->size() != n * n)
        throw new Error("Error: the size of A does not match nxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    if (x->size() != n)
        throw new Error("Error: size of x should be m");
    if (y->size() != n)
        throw new Error("Error: size of y should be n");

    switch (x->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *xbuff = ((Floats *)x)->liste.items->buffer;
        float *ybuff = ((Floats *)y)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();

        cblas_ssyr2(CblasColMajor, up, n, alpha, xbuff, incx, ybuff, incy, a, lda);
        return A;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *xbuff = ((Numbers *)x)->liste.items->buffer;
        double *ybuff = ((Numbers *)y)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();

        cblas_dsyr2(CblasColMajor, up, n, alpha, xbuff, incx, ybuff, incy, a, lda);
        return A;
    }
    default:
        throw new Error("Error: 'blas_syr2' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::trmv(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *x = lisp->get_variable(L"x");
    if (A->type != x->type)
        throw new Error("Error: Vectors should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long incx = lisp->get_variable(L"incx")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    long trans = lisp->get_variable(L"trans")->asInteger();
    bool uplo = lisp->get_variable(L"uplo")->Boolean();
    bool diag = lisp->get_variable(L"diag")->Boolean();

    if (A->size() != n * n)
        throw new Error("Error: the size of A does not match nxn");

    if (x->size() != n)
        throw new Error("Error: size of x should be m");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;
    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    CBLAS_TRANSPOSE op;
    switch (trans)
    {
    case 0:
        op = CblasNoTrans;
        break;
    case 1:
        op = CblasTrans;
        break;
    default:
        op = CblasConjTrans;
    }

    CBLAS_DIAG unit = diag ? CblasUnit : CblasNonUnit;
    
    switch (x->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *xbuff = ((Floats *)x)->liste.items->buffer;
        cblas_strmv(CblasRowMajor, CblasUpper, CblasNoTrans, CblasNonUnit, n, a, lda, xbuff, incx);
        return x;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *xbuff = ((Numbers *)x)->liste.items->buffer;
        cblas_dtrmv(CblasRowMajor, CblasUpper, CblasNoTrans, CblasNonUnit, n, a, lda, xbuff, incx);
        return x;
    }
    default:
        throw new Error("Error: 'blas_trmv' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::trsv(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *x = lisp->get_variable(L"x");
    if (A->type != x->type)
        throw new Error("Error: Vectors should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long incx = lisp->get_variable(L"incx")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    bool uplo = lisp->get_variable(L"uplo")->Boolean();
    long trans = lisp->get_variable(L"trans")->Boolean();
    bool diag = lisp->get_variable(L"diag")->Boolean();

    if (A->size() != n * n)
        throw new Error("Error: the size of A does not match nxn");

    if (x->size() != n)
        throw new Error("Error: size of x should be m");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor; 
    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    CBLAS_TRANSPOSE op;
    switch (trans)
    {
    case 0:
        op = CblasNoTrans;
        break;
    case 1:
        op = CblasTrans;
        break;
    default:
        op = CblasConjTrans;
    }

    CBLAS_DIAG unit = diag ? CblasUnit : CblasNonUnit;

    switch (x->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *xbuff = ((Floats *)x)->liste.items->buffer;
        cblas_strsv(lay, up, op, unit, n, a, lda, xbuff, incx);
        return x;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *xbuff = ((Numbers *)x)->liste.items->buffer;
        cblas_dtrsv(lay, up, op, unit, n, a, lda, xbuff, incx);
        return x;
    }
    default:
        throw new Error("Error: 'blas_trsv' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::gemm(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *B = lisp->get_variable(L"B");
    Element *C = lisp->get_variable(L"C");
    if (A->type != B->type || A->type != C->type)
        throw new Error("Error: Matrices should be of the same type");

    long m = lisp->get_variable(L"m")->asInteger();
    long n = lisp->get_variable(L"n")->asInteger();
    long k = lisp->get_variable(L"k")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long ldb = lisp->get_variable(L"ldb")->asInteger();
    long ldc = lisp->get_variable(L"ldc")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    long transA = lisp->get_variable(L"transA")->asInteger();
    long transB = lisp->get_variable(L"transB")->asInteger();

    if (A->size() != m * k)
        throw new Error("Error: the size of A does not match mxn");

    if (B->size() != n * k)
        throw new Error("Error: the size of A does not match mxn");

    CBLAS_TRANSPOSE op1;
    switch (transA)
    {
    case 0:
        op1 = CblasNoTrans;
        break;
    case 1:
        op1 = CblasTrans;
        break;
    default:
        op1 = CblasConjTrans;
    }

    CBLAS_TRANSPOSE op2;
    switch (transB)
    {
    case 0:
        op2 = CblasNoTrans;
        break;
    case 1:
        op2 = CblasTrans;
        break;
    default:
        op2 = CblasConjTrans;
    }

    switch (A->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *b = ((Floats *)B)->liste.items->buffer;
        ((Floats*)C)->reserve(n*m);
        float *c = ((Floats *)C)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        float beta = lisp->get_variable(L"beta")->asFloat();

        cblas_sgemm(CblasRowMajor, op1, op2, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc);

        return C;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *b = ((Numbers *)B)->liste.items->buffer;
        ((Numbers*)C)->reserve(n*m);
        double *c = ((Numbers *)C)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        double beta = lisp->get_variable(L"beta")->asNumber();

        cblas_dgemm(CblasRowMajor, op1, op2, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc);

        return C;
    }
    default:
        throw new Error("Error: 'blas_gemm' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::hemm(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *B = lisp->get_variable(L"B");
    Element *C = lisp->get_variable(L"C");
    if (A->type != B->type || A->type != C->type)
        throw new Error("Error: Matrices should be of the same type");

    long m = lisp->get_variable(L"m")->asInteger();
    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long ldb = lisp->get_variable(L"ldb")->asInteger();
    long ldc = lisp->get_variable(L"ldc")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    long side = lisp->get_variable(L"side")->asInteger();
    long uplo = lisp->get_variable(L"uplo")->asInteger();

    CBLAS_SIDE sd = side ? CblasLeft : CblasRight;

    if (side) {   
        if (A->size() != m * m)
            throw new Error("Error: the size of A does not match mxn");
    }
    else {
        if (A->size() != n * n)
            throw new Error("Error: the size of A does not match mxn");
    }

    if (B->size() != m * n)
        throw new Error("Error: the size of A does not match mxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;
    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    switch (A->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *b = ((Floats *)B)->liste.items->buffer;
        ((Floats*)C)->reserve(m*m);
        float *c = ((Floats *)C)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        float beta = lisp->get_variable(L"beta")->asFloat();
        cblas_ssymm(lay, sd, up, m, n, alpha, a, lda, b, ldb, beta, c, ldc);
        return C;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *b = ((Numbers *)B)->liste.items->buffer;
        ((Numbers*)C)->reserve(m*m);
        double *c = ((Numbers *)C)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        double beta = lisp->get_variable(L"beta")->asNumber();
        cblas_dsymm(lay, sd, up, m, n, alpha, a, lda, b, ldb, beta, c, ldc);
        return C;
    }
    default:
        throw new Error("Error: 'blas_hemm' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::symm(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *B = lisp->get_variable(L"B");
    Element *C = lisp->get_variable(L"C");
    if (A->type != B->type || A->type != C->type)
        throw new Error("Error: Matrices should be of the same type");

    long m = lisp->get_variable(L"m")->asInteger();
    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long ldb = lisp->get_variable(L"ldb")->asInteger();
    long ldc = lisp->get_variable(L"ldc")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    long side = lisp->get_variable(L"side")->asInteger();
    long uplo = lisp->get_variable(L"uplo")->asInteger();

    CBLAS_SIDE sd = side ? CblasLeft : CblasRight;

    if (side) {   
        if (A->size() != m * m)
            throw new Error("Error: the size of A does not match mxn");
    }
    else {
        if (A->size() != n * n)
            throw new Error("Error: the size of A does not match mxn");
    }

    if (B->size() != m * n)
        throw new Error("Error: the size of A does not match mxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    switch (A->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *b = ((Floats *)B)->liste.items->buffer;
        ((Floats*)C)->reserve(m*m);
        float *c = ((Floats *)C)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        float beta = lisp->get_variable(L"beta")->asFloat();
        cblas_ssymm(CblasRowMajor, CblasLeft, CblasUpper, m, n, alpha, a, lda, b, ldb, beta, c, ldc);
        return C;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *b = ((Numbers *)B)->liste.items->buffer;
        ((Numbers*)C)->reserve(m*m);
        double *c = ((Numbers *)C)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        double beta = lisp->get_variable(L"beta")->asNumber();
        cblas_dsymm(CblasRowMajor, CblasLeft, CblasUpper, m, n, alpha, a, lda, b, ldb, beta, c, ldc);
        return C;
    }
    default:
        throw new Error("Error: 'blas_symm' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::herk(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *C = lisp->get_variable(L"C");
    if (A->type != C->type)
        throw new Error("Error: Matrices should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long k = lisp->get_variable(L"k")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long ldc = lisp->get_variable(L"ldc")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    long trans = lisp->get_variable(L"trans")->asInteger();
    long uplo = lisp->get_variable(L"uplo")->asInteger();

    if (A->size() != k * n)
        throw new Error("Error: the size of A does not match mxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    CBLAS_TRANSPOSE op;
    switch (trans)
    {
    case 0:
        op = CblasNoTrans;
        break;
    case 1:
        op = CblasTrans;
        break;
    default:
        op = CblasConjTrans;
    }

    switch (A->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        ((Floats*)C)->reserve(n*n);
        float *c = ((Floats *)C)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        float beta = lisp->get_variable(L"beta")->asFloat();
        ssyrk(&up, &op, &n, &k, &alpha, a, &lda, &beta, c, &ldc);
        return C;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        ((Numbers*)C)->reserve(n*n);
        double *c = ((Numbers *)C)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        double beta = lisp->get_variable(L"beta")->asNumber();
        dsyrk(&up, &op, &n, &k, &alpha, a, &lda, &beta, c, &ldc);
        return C;
    }
    default:
        throw new Error("Error: 'blas_herk' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::syrk(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *C = lisp->get_variable(L"C");
    if (A->type != C->type)
        throw new Error("Error: Matrices should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long k = lisp->get_variable(L"k")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long ldc = lisp->get_variable(L"ldc")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    long trans = lisp->get_variable(L"trans")->asInteger();
    long uplo = lisp->get_variable(L"uplo")->asInteger();

    if (A->size() != k * n)
        throw new Error("Error: the size of A does not match mxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;
    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    CBLAS_TRANSPOSE op;
    switch (trans)
    {
    case 0:
        op = CblasNoTrans;
        break;
    case 1:
        op = CblasTrans;
        break;
    default:
        op = CblasConjTrans;
    }

    switch (A->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        ((Floats*)C)->reserve(n*n);
        float *c = ((Floats *)C)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        float beta = lisp->get_variable(L"beta")->asFloat();
        
        cblas_ssyrk(lay, up, op, n, k, alpha, a, lda, beta, c, ldc);
        return C;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        ((Numbers*)C)->reserve(n*n);
        double *c = ((Numbers *)C)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        double beta = lisp->get_variable(L"beta")->asNumber();
        
        cblas_dsyrk(lay, up, op, n, k, alpha, a, lda, beta, c, ldc);
        return C;
    }
    default:
        throw new Error("Error: 'blas_syrk' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::her2k(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *B = lisp->get_variable(L"B");
    Element *C = lisp->get_variable(L"C");
    if (A->type != C->type || A->type != B->type)
        throw new Error("Error: Matrices should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long k = lisp->get_variable(L"k")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long ldb = lisp->get_variable(L"ldb")->asInteger();
    long ldc = lisp->get_variable(L"ldc")->asInteger();        
    bool layout = lisp->get_variable(L"layout")->Boolean();
    long trans = lisp->get_variable(L"trans")->asInteger();
    long uplo = lisp->get_variable(L"uplo")->asInteger();

    if (A->size() != k * n)
        throw new Error("Error: the size of A does not match mxn");

    if (B->size() != k * n)
        throw new Error("Error: the size of B does not match mxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    CBLAS_TRANSPOSE op;
    switch (trans)
    {
    case 0:
        op = CblasNoTrans;
        break;
    case 1:
        op = CblasTrans;
        break;
    default:
        op = CblasConjTrans;
    }

    switch (A->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *b = ((Floats *)B)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        float beta = lisp->get_variable(L"beta")->asFloat();

        ((Floats*)C)->reserve(n*n);
        float *c = ((Floats *)C)->liste.items->buffer;

        cblas_ssyr2k(CblasColMajor, CblasUpper, CblasNoTrans, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
        return C;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *b = ((Numbers *)B)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        double beta = lisp->get_variable(L"beta")->asNumber();
        ((Numbers*)C)->reserve(n*n);
        double *c = ((Numbers *)C)->liste.items->buffer;

        cblas_dsyr2k(CblasColMajor, CblasUpper, CblasNoTrans, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
        return C;
    }
    default:
        throw new Error("Error: 'blas_her2k' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::syr2k(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *B = lisp->get_variable(L"B");
    Element *C = lisp->get_variable(L"C");
    if (A->type != C->type || A->type != B->type)
        throw new Error("Error: Matrices should be of the same type");

    long n = lisp->get_variable(L"n")->asInteger();
    long k = lisp->get_variable(L"k")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long ldb = lisp->get_variable(L"ldb")->asInteger();
    long ldc = lisp->get_variable(L"ldc")->asInteger();        
    bool layout = lisp->get_variable(L"layout")->Boolean();
    long trans = lisp->get_variable(L"trans")->asInteger();
    long uplo = lisp->get_variable(L"uplo")->asInteger();

    if (A->size() != k * n)
        throw new Error("Error: the size of A does not match mxn");

    if (B->size() != k * n)
        throw new Error("Error: the size of B does not match mxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;

    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    CBLAS_TRANSPOSE op;
    switch (trans)
    {
    case 0:
        op = CblasNoTrans;
        break;
    case 1:
        op = CblasTrans;
        break;
    default:
        op = CblasConjTrans;
    }

    switch (A->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *b = ((Floats *)B)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        float beta = lisp->get_variable(L"beta")->asFloat();

        ((Floats*)C)->reserve(n*n);
        float *c = ((Floats *)C)->liste.items->buffer;
        cblas_ssyr2k(CblasColMajor, CblasUpper, CblasNoTrans, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
        return C;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *b = ((Numbers *)B)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        double beta = lisp->get_variable(L"beta")->asNumber();
        ((Numbers*)C)->reserve(n*n);
        double *c = ((Numbers *)C)->liste.items->buffer;
        cblas_dsyr2k(CblasColMajor, CblasUpper, CblasNoTrans, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
        return C;
    }
    default:
        throw new Error("Error: 'blas_syr2k' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::trmm(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *B = lisp->get_variable(L"B");
    if (A->type != B->type)
        throw new Error("Error: Matrices should be of the same type");

    long m = lisp->get_variable(L"m")->asInteger();
    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long ldb = lisp->get_variable(L"ldb")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    long side = lisp->get_variable(L"side")->asInteger();
    long uplo = lisp->get_variable(L"uplo")->asInteger();
    bool diag = lisp->get_variable(L"diag")->Boolean();
    long trans = lisp->get_variable(L"trans")->asInteger();    

    CBLAS_SIDE sd = side ? CblasLeft : CblasRight;
    CBLAS_DIAG unit = diag ? CblasUnit : CblasNonUnit;

    if (side) {   
        if (A->size() != m * m)
            throw new Error("Error: the size of A does not match mxn");
    }
    else {
        if (A->size() != n * n)
            throw new Error("Error: the size of A does not match mxn");
    }

    if (B->size() != m * n)
        throw new Error("Error: the size of A does not match mxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;
    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    CBLAS_TRANSPOSE op;
    switch (trans)
    {
    case 0:
        op = CblasNoTrans;
        break;
    case 1:
        op = CblasTrans;
        break;
    default:
        op = CblasConjTrans;
    }

    switch (A->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *b = ((Floats *)B)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();
        blas_strmm(lay, sd, up, op, unit, m, n, alpha, a, lda, b, ldb);
        return B;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *b = ((Numbers *)B)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();
        blas_dtrmm_(lay, sd, up, op, unit, m, n, alpha, a, lda, b, ldb);
        return B;
    }
    default:
        throw new Error("Error: 'blas_trmm' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::trsm(LispE *lisp)
{
    Element *A = lisp->get_variable(L"A");
    Element *B = lisp->get_variable(L"B");
    if (A->type != B->type)
        throw new Error("Error: Matrices should be of the same type");

    long m = lisp->get_variable(L"m")->asInteger();
    long n = lisp->get_variable(L"n")->asInteger();
    long lda = lisp->get_variable(L"lda")->asInteger();
    long ldb = lisp->get_variable(L"ldb")->asInteger();
    bool layout = lisp->get_variable(L"layout")->Boolean();
    long side = lisp->get_variable(L"side")->asInteger();
    long uplo = lisp->get_variable(L"uplo")->asInteger();
    bool diag = lisp->get_variable(L"diag")->Boolean();
    long trans = lisp->get_variable(L"trans")->asInteger();

    CBLAS_SIDE sd = side ? CblasLeft : CblasRight;
    CBLAS_DIAG unit = diag ? CblasUnit : CblasNonUnit;
    
    if (side) {   
        if (A->size() != m * m)
            throw new Error("Error: the size of A does not match mxn");
    }
    else {
        if (A->size() != n * n)
            throw new Error("Error: the size of A does not match mxn");
    }

    if (B->size() != m * n)
        throw new Error("Error: the size of A does not match mxn");

    CBLAS_LAYOUT lay = layout ? CblasColMajor : CblasRowMajor;
    CBLAS_UPLO up = uplo ? CblasUpper : CblasLower;

    CBLAS_TRANSPOSE op;
    switch (trans)
    {
    case 0:
        op = CblasNoTrans;
        break;
    case 1:
        op = CblasTrans;
        break;
    default:
        op = CblasConjTrans;
    }

    switch (A->type)
    {
    case t_floats:
    {
        float *a = ((Floats *)A)->liste.items->buffer;
        float *b = ((Floats *)B)->liste.items->buffer;
        float alpha = lisp->get_variable(L"alpha")->asFloat();

        cblas_strsm(lay, sd, up, op, unit, m, n, alpha, a, lda, b, ldb);
        
        return B;
    }
    case t_numbers:
    {
        double *a = ((Numbers *)A)->liste.items->buffer;
        double *b = ((Numbers *)B)->liste.items->buffer;
        double alpha = lisp->get_variable(L"alpha")->asNumber();

        cblas_dtrsm(lay, sd, up, op, unit, m, n, alpha, a, lda, b, ldb);
        
        return B;
    }
    default:
        throw new Error("Error: 'blas_trsm' only apply to floats_ and numbers_");
    }
}

Element *Lispe_blas::eval(LispE *lisp)
{
    // The name defined in the extension is not insignificant, it is used to retrieve our arguments.
    switch (action)
    {
    case blas_asum:
    {
        return asum(lisp);
    }
    case blas_axpy:
    {
        return axpy(lisp);
    }
    case blas_dot:
    {
        return dot(lisp);
    }
    case blas_dotu:
    {
        return dotu(lisp);
    }
    case blas_iamax:
    {
        return iamax(lisp);
    }
    case blas_nrm2:
    {
        return nrm2(lisp);
    }
    case blas_scale:
    {
        return scale(lisp);
    }
    case blas_gemv:
    {
        return gemv(lisp);
    }
    case blas_ger:
    {
        return ger(lisp);
    }
    case blas_geru:
    {
        return geru(lisp);
    }
    case blas_hemv:
    {
        return hemv(lisp);
    }
    case blas_her:
    {
        return her(lisp);
    }
    case blas_her2:
    {
        return her2(lisp);
    }
    case blas_symv:
    {
        return symv(lisp);
    }
    case blas_syr:
    {
        return syr(lisp);
    }
    case blas_syr2:
    {
        return syr(lisp);
    }
    case blas_trmv:
    {
        return trmv(lisp);
    }
    case blas_trsv:
    {
        return trsv(lisp);
    }
    case blas_gemm:
    {
        return gemm(lisp);
    }
    case blas_hemm:
    {
        return hemm(lisp);
    }

    case blas_symm:
    {
        return symm(lisp);
    }

    case blas_herk:
    {
        return herk(lisp);
    }

    case blas_syrk:
    {
        return syrk(lisp);
    }

    case blas_her2k:
    {
        return her2k(lisp);
    }

    case blas_syr2k:
    {
        return syr2k(lisp);
    }

    case blas_trmm:
    {
        return trmm(lisp);
    }

    case blas_trsm:
    {
        return trsm(lisp);
    }
    }
}

// We use this instruction to return a description of the instruction
// Indeed, just do: (print blas_example) to get this information
wstring Lispe_blas::asString(LispE *lisp)
{
    // The name defined in the extension is not insignificant, it is used to retrieve our arguments.
    switch (action)
    {
    case blas_asum:
    {
        return L"Compute the sum of absolute values of vector x.";
    }
    case blas_axpy:
    {
        return L"Compute a vector scaling and a sum: y = a*x + y.";
    }
    case blas_dot:
    {
        return L"Compute the dot product of vectors x and y.";
    }
    case blas_dotu:
    {
        return L"Compute the dot product of vectors x and y with conjugation of complex numbers disabled.";
    }
    case blas_iamax:
    {
        return L"Find the index of the element with the maximum absolute value in vector x.";
    }
    case blas_nrm2:
    {
        return L"Compute the Euclidean norm of vector x.";
    }
    case blas_scale:
    {
        return L"Scale a vector by a constant value: x = a*x.";
    }
    case blas_gemv:
    {
        return L"Compute matrix-vector product: y = alpha*A*x + beta*y.";
    }
    case blas_ger:
    {
        return L"Perform a rank-1 update of matrix A with a vector: A = alpha*x*y' + A.";
    }
    case blas_geru:
    {
        return L"Perform a rank-1 update of matrix A with a vector, without conjugation of the elements of vector y: A = alpha*x*y' + A.";
    }
    case blas_hemv:
    {
        return L"Compute Hermitian matrix-vector product: y = alpha*A*x + beta*y, where A is a Hermitian matrix.";
    }
    case blas_her:
    {
        return L"Perform a rank-1 update of Hermitian matrix A with a vector: A = alpha*x*x' + A.";
    }
    case blas_her2:
    {
        return L"Perform a rank-2 update of Hermitian matrix A with two vectors: A = alpha*x*y' + conj(alpha)*y*x' + A.";
    }
    case blas_symv:
    {
        return L"Compute symmetric matrix-vector product: y = alpha*A*x + beta*y, where A is a symmetric matrix.";
    }
    case blas_syr:
    {
        return L"Perform a rank-1 update of symmetric matrix A with a vector: A = alpha*x*x' + A.";
    }
    case blas_syr2:
    {
        return L"Perform a rank-2 update of symmetric matrix A with two vectors: A = alpha*x*y' + alpha*y*x' + A.";
    }
    case blas_trmv:
    {
        return L"Compute matrix-vector product with a triangular matrix: x = A*x.";
    }
    case blas_trsv:
    {
        return L"Solve a system of linear equations with a triangular matrix: x = A^(-1)*x.";
    }
    case blas_gemm:
    {
        return L"Compute matrix-matrix product: C = alpha*A*B + beta*C.";
    }
    case blas_hemm:
    {
        return L"Compute Hermitian matrix-matrix product: C = alpha*A*B + beta*C, where A is a Hermitian matrix.";
    }

    case blas_symm:
    {
        return L"Compute symmetric matrix-matrix product: C = alpha*A*B + beta*C, where A is a symmetric matrix.";
    }

    case blas_herk:
    {
        return L"Perform a rank-k update of a Hermitian matrix: C = alpha*A*A^H + beta*C, where A is a Hermitian matrix.";
    }

    case blas_syrk:
    {
        return L"Perform a rank-k update of a symmetric matrix: C = alpha*A*A^T + beta*C, where A is a symmetric matrix.";
    }

    case blas_her2k:
    {
        return L"Perform a rank-2k update of a Hermitian matrix: C = alpha*A*B^H + conj(alpha)*B*A^H + beta*C, where A and B are Hermitian matrices.";
    }

    case blas_syr2k:
    {
        return L"Perform a rank-2k update of a symmetric matrix: C = alpha*A*B^T + alpha*B*A^T + beta*C, where A and B are symmetric matrices.";
    }

    case blas_trmm:
    {
        return L"Compute matrix-matrix product with a triangular matrix: B = alpha*A*B.";
    }

    case blas_trsm:
    {
        return L"Solve a system of linear equations with a triangular matrix: B = alpha*A^(-1)*B.";
    }
    }
}

extern "C"
{
    Exporting bool InitialisationModule(LispE *lisp)
    {
        // We first create the body of the function
        Element *body;
        // Level 1: vectors operations, O(n) work
        body = lisp->extension("deflib blas_asum(x (incx 1))", new Lispe_blas(blas_asum));
        body = lisp->extension("deflib blas_axpy(x y (incx 1) (incy 1))", new Lispe_blas(blas_axpy));
        body = lisp->extension("deflib blas_dot(x y (incx 1) (incy 1))", new Lispe_blas(blas_dot));
        body = lisp->extension("deflib blas_dotu(x y (incx 1) (incy 1))", new Lispe_blas(blas_dotu));
        body = lisp->extension("deflib blas_iamax(x (incx 1))", new Lispe_blas(blas_iamax));
        body = lisp->extension("deflib blas_nrm2(x (incx 1))", new Lispe_blas(blas_nrm2));
        body = lisp->extension("deflib blas_scal(x scale (incx 1))", new Lispe_blas(blas_scale));

        // Level 2: matrix-vector operations, O(n^2) work
        body = lisp->extension("deflib blas_gemv(A m n lda x incx y incy (alpha 1) (beta 1) (layout true) (trans 0))", new Lispe_blas(blas_gemv));
        body = lisp->extension("deflib blas_ger(A m n lda x incx y incy (alpha 1) (layout true))", new Lispe_blas(blas_ger));
        body = lisp->extension("deflib blas_geru(A m n lda x incx y incy (alpha 1) (layout true))", new Lispe_blas(blas_geru));
        body = lisp->extension("deflib blas_hemv(A n lda x incx y incy (alpha 1) (beta 1) (layout true) (uplo true))", new Lispe_blas(blas_hemv));
        body = lisp->extension("deflib blas_symv(A n lda x incx y incy (alpha 1) (beta 1) (layout true) (uplo true))", new Lispe_blas(blas_symv));
        body = lisp->extension("deflib blas_her(A n lda x incx (alpha 1) (layout true) (uplo true))", new Lispe_blas(blas_her));
        body = lisp->extension("deflib blas_syr(A n lda x incx (alpha 1) (layout true) (uplo true))", new Lispe_blas(blas_syr));
        body = lisp->extension("deflib blas_her2(A n lda x incx y incy (alpha 1) (layout true) (uplo true))", new Lispe_blas(blas_her2));
        body = lisp->extension("deflib blas_syr2(A n lda x incx y incy (alpha 1) (layout true) (uplo true))", new Lispe_blas(blas_syr2));
        body = lisp->extension("deflib blas_trmv(A n lda x incx (layout true) (uplo true) (trans 0) (diag true))", new Lispe_blas(blas_trmv));
        body = lisp->extension("deflib blas_trsv(A n lda x incx (layout true) (uplo true) (trans 0) (diag true))", new Lispe_blas(blas_trsv));

        // Level 3: matrix-matrix operations, O(n^3) work
        body = lisp->extension("deflib blas_gemm(A m n k lda B ldb C ldc (alpha 1) (beta 1) (layout true) (transA 0) (transB 0))", new Lispe_blas(blas_gemm));
        body = lisp->extension("deflib blas_hemm(A m n lda B ldb C ldc (alpha 1) (beta 1) (layout true) (side true) (uplo true))", new Lispe_blas(blas_hemm));
        body = lisp->extension("deflib blas_symm(A m n lda B ldb C ldc (alpha 1) (beta 1) (layout true) (side true) (uplo true))", new Lispe_blas(blas_symm));
        body = lisp->extension("deflib blas_herk(A n k lda C ldc (alpha 1) (beta 1) (layout true) (uplo true) (trans 0))", new Lispe_blas(blas_herk));
        body = lisp->extension("deflib blas_syrk(A n k lda C ldc (alpha 1) (beta 1) (layout true) (uplo true) (trans 0))", new Lispe_blas(blas_syrk));
        body = lisp->extension("deflib blas_her2k(A n k lda B ldb C ldc (alpha 1) (beta 1) (layout true) (uplo true) (trans 0))", new Lispe_blas(blas_her2k));
        body = lisp->extension("deflib blas_syr2k(A n k lda B ldb C ldc (alpha 1) (beta 1) (layout true) (uplo true) (trans 0))", new Lispe_blas(blas_syr2k));
        body = lisp->extension("deflib blas_trmm(A m n lda B ldb (alpha 1) (layout true) (side true) (uplo true) (trans 0) (diag  true))", new Lispe_blas(blas_trmm));
        body = lisp->extension("deflib blas_trsm(A m n lda B ldb (alpha 1) (layout true) (side true) (uplo true) (trans 0) (diag  true))", new Lispe_blas(blas_trsm));

        return true;
    }
}


