/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  math.cxx
//
//


/*
 Implantation des fonctions mathématiques
 */

#include "lispe.h"
#include "elements.h"
#include "tools.h"
#include "vecte.h"
#include <algorithm>

#ifdef WIN32
#define _USE_MATH_DEFINES
#endif

#include <math.h>

#ifdef UNIX
#define ABS(x) fabs((double)x)
#define TRUE true
#define FALSE false
#else
#define ABS(x) abs((double)x)
#endif

#define NMAX 100
#define TINY 1.5e-16
#define REAL double
#define ZERO (double)0.0
#define ONE (double)1.0
#define TWO (double)2.0

#ifdef INTELINTRINSICS
#ifdef WIN32
#include <intrin.h>
#else
#include <x86intrin.h>
#endif
#endif

static const double M_GOLDEN = 1.61803398874989484820458683436563811772030917980576286213544862270526046281890244970720720418939113748475;
#define lmin(x,y) x<y?x:y
//------------------------------------------------------------------------------------------
Element* range(LispE* lisp, double init, double limit, double inc) {
    double d = (limit - init) / inc;
    if (d<0)
        d *= -1;
    
    if (init > limit && inc > 0)
        inc *= -1;
    
    if (d <= 100000) {
        if (inc == 0)
            return emptylist_;
        
        //Integers ?
        if (inc == (long)inc && init == (long)init && limit == (long)limit) {
            Integers* range_list = lisp->provideIntegers();
            range_list->liste.reserve((long)d);
            if (inc > 0) {
                for (long i = init; i < limit; i += inc) {
                    range_list->liste.push_back(i);
                }
            }
            else {
                for (long i = init; i > limit; i += inc)
                    range_list->liste.push_back(i);
            }
            return range_list;
        }
        
        Numbers* range_list = lisp->provideNumbers();
        range_list->liste.reserve((long)d);
        if (inc > 0) {
            for (double i = init; i < limit; i += inc) {
                range_list->liste.push_back(i);
            }
        }
        else {
            for (double i = init; i > limit; i += inc)
            range_list->liste.push_back(i);
        }
        return range_list;
    }
    throw new Error("Error: Exceeding range");
}

Element* List::evall_range(LispE* lisp) {

    double init, limit, inc;
    evalAsNumber(1, lisp, init);
    evalAsNumber(2, lisp, limit);
    evalAsNumber(3, lisp, inc);
    return range(lisp, init, limit, inc);
}

Element* List::evall_irange(LispE* lisp) {
    long sz  = liste.size();
    
    double init, inc;
    evalAsNumber(1, lisp, init);
    evalAsNumber(2, lisp, inc);
    if (sz == 4) {
        double bound;
        evalAsNumber(3, lisp, bound);
        if (init == (long)init && inc == (long)inc)
            return new InfiniterangeInteger(init, inc, bound);
        return new InfiniterangeNumber(init, inc, bound);
    }
    if (init == (long)init && inc == (long)inc)
        return new InfiniterangeInteger(init, inc);
    return new InfiniterangeNumber(init, inc);
}



//------------------------------------------------------------------------------------------

typedef enum {math_fabs,math_acos,math_acosh,math_asin,math_asinh,math_atan,math_atanh,math_cbrt,math_cos,math_cosh,math_erf,math_erfc,math_exp,math_exp2,math_expm1,math_floor,math_lgamma,math_log,math_log10,math_log1p,math_log2,math_logb,math_nearbyint,math_rint,math_round,math_sin,math_sinh,math_sqrt,math_tan,math_tanh,math_tgamma,math_trunc, math_radian, math_degree, math_gcd, math_hcf} math;


long gcd_math(long a, long b)
{
    // Everything divides 0
    if (a == 0)
       return b;
    if (b == 0)
       return a;
    // base case
    if (a == b)
        return a;
    // a is greater
    if (a > b)
        return gcd_math(a-b, b);
    return gcd_math(a, b-a);
}

long hcf_math(long x, long y) {
  return (!y)?x:hcf_math(y, x%y);
}

/*
 Tout d'abord on crée une nouvelle dérivation de Element
 On implante alors les méthodes "eval" et "commeChaine"
 Cette extension exporte la majorité des opérateurs mathématique
 */

class Math : public Element {
public:
    math m;
    short v_val;
    Math(LispE* lisp, math s) : m(s), Element(l_lib) {
        //We chose val as variable name everywhere
        // we recover his code to speed up processing ...
        u_ustring val = U"val";
        v_val = lisp->encode(val);
    }
    
    Element* eval(LispE* lisp) {
        //eval is either: command, setenv or getenv...
        double v;
        switch (m) {
            case math_fabs: {
                v = lisp->get_variable(v_val)->asNumber();
                v = fabs(v);
                return lisp->provideNumber(v);
            }
            case math_acos: {
                v = lisp->get_variable(v_val)->asNumber();
                v = acos(v);
                return lisp->provideNumber(v);
            }
            case math_acosh: {
                v = lisp->get_variable(v_val)->asNumber();
                v = acosh(v);
                return lisp->provideNumber(v);
            }
            case math_asin: {
                v = lisp->get_variable(v_val)->asNumber();
                v = asin(v);
                return lisp->provideNumber(v);
            }
            case math_asinh: {
                v = lisp->get_variable(v_val)->asNumber();
                v = asinh(v);
                return lisp->provideNumber(v);
            }
            case math_atan: {
                v = lisp->get_variable(v_val)->asNumber();
                v = atan(v);
                return lisp->provideNumber(v);
            }
            case math_atanh: {
                v = lisp->get_variable(v_val)->asNumber();
                v = atanh(v);
                return lisp->provideNumber(v);
            }
            case math_cbrt: {
                v = lisp->get_variable(v_val)->asNumber();
                v = cbrt(v);
                return lisp->provideNumber(v);
            }
            case math_cos: {
                v = lisp->get_variable(v_val)->asNumber();
                v = cos(v);
                return lisp->provideNumber(v);
            }
            case math_cosh: {
                v = lisp->get_variable(v_val)->asNumber();
                v = cosh(v);
                return lisp->provideNumber(v);
            }
            case math_erf: {
                v = lisp->get_variable(v_val)->asNumber();
                v = erf(v);
                return lisp->provideNumber(v);
            }
            case math_erfc: {
                v = lisp->get_variable(v_val)->asNumber();
                v = erfc(v);
                return lisp->provideNumber(v);
            }
            case math_exp: {
                v = lisp->get_variable(v_val)->asNumber();
                v = exp(v);
                return lisp->provideNumber(v);
            }
            case math_exp2: {
                v = lisp->get_variable(v_val)->asNumber();
                v = exp2(v);
                return lisp->provideNumber(v);
            }
            case math_expm1: {
                v = lisp->get_variable(v_val)->asNumber();
                v = expm1(v);
                return lisp->provideNumber(v);
            }
            case math_floor: {
                v = lisp->get_variable(v_val)->asNumber();
                v = floor(v);
                return lisp->provideNumber(v);
            }
            case math_gcd: {
                long v = lisp->get_variable(v_val)->asInteger();
                long vv = lisp->get_variable(U"vaal")->asInteger();
                return lisp->provideInteger(gcd_math(v,vv));
            }
            case math_hcf: {
                long v = lisp->get_variable(v_val)->asInteger();
                long vv = lisp->get_variable(U"vaal")->asInteger();
                return lisp->provideInteger(hcf_math(v,vv));
            }
            case math_lgamma: {
                v = lisp->get_variable(v_val)->asNumber();
                v = lgamma(v);
                return lisp->provideNumber(v);
            }
            case math_log: {
                v = lisp->get_variable(v_val)->asNumber();
                v = log(v);
                return lisp->provideNumber(v);
            }
            case math_log10: {
                v = lisp->get_variable(v_val)->asNumber();
                v = log10(v);
                return lisp->provideNumber(v);
            }
            case math_log1p: {
                v = lisp->get_variable(v_val)->asNumber();
                v = log1p(v);
                return lisp->provideNumber(v);
            }
            case math_log2: {
                v = lisp->get_variable(v_val)->asNumber();
                v = log2(v);
                return lisp->provideNumber(v);
            }
            case math_logb: {
                v = lisp->get_variable(v_val)->asNumber();
                v = logb(v);
                return lisp->provideNumber(v);
            }
            case math_nearbyint: {
                v = lisp->get_variable(v_val)->asNumber();
                v = nearbyint(v);
                return lisp->provideNumber(v);
            }
            case math_rint: {
                v = lisp->get_variable(v_val)->asNumber();
                v = rint(v);
                return lisp->provideNumber(v);
            }
            case math_round: {
                v = lisp->get_variable(v_val)->asNumber();
                v = round(v);
                return lisp->provideNumber(v);
            }
            case math_sin: {
                v = lisp->get_variable(v_val)->asNumber();
                v = sin(v);
                return lisp->provideNumber(v);
            }
            case math_sinh: {
                v = lisp->get_variable(v_val)->asNumber();
                v = sinh(v);
                return lisp->provideNumber(v);
            }
            case math_sqrt: {
                v = lisp->get_variable(v_val)->asNumber();
                v = sqrt(v);
                return lisp->provideNumber(v);
            }
            case math_tan: {
                v = lisp->get_variable(v_val)->asNumber();
                v = tan(v);
                return lisp->provideNumber(v);
            }
            case math_tanh: {
                v = lisp->get_variable(v_val)->asNumber();
                v = tanh(v);
                return lisp->provideNumber(v);
            }
            case math_tgamma: {
                v = lisp->get_variable(v_val)->asNumber();
                v = tgamma(v);
                return lisp->provideNumber(v);
            }
            case math_trunc: {
                v = lisp->get_variable(v_val)->asNumber();
                v = trunc(v);
                return lisp->provideNumber(v);
            }
            case math_radian: {
                v = lisp->get_variable(v_val)->asNumber();
                v = M_PI*(v / 180);
                return lisp->provideNumber(v);
            }
            case math_degree: {
                v = lisp->get_variable(v_val)->asNumber();
                v = (v * 180) / M_PI;
                return lisp->provideNumber(v);
            }
        }
		return zero_;
    }
    
    //We use this instruction to return a description of the instruction
    //in effect, just do: (print getenv) to get this information
    wstring asString(LispE* lisp) {
        switch (m) {
            case math_fabs: {
                return L"compute the absolute value of a float type number";
            }
            case math_acos: {
                return L"compute the arc cosine";
            }
            case math_acosh: {
                return L"compute the hyperbolic arc cosine";
            }
            case math_asin: {
                return L"compute the arc sine";
            }
            case math_asinh: {
                return L"compute the hyperbolic sine arc";
            }
            case math_atan: {
                return L"compute the arc tangent";
            }
            case math_atanh: {
                return L"compute the hyperbolic arc tangent";
            }
            case math_cbrt: {
                return L"compute the cubic root";
            }
            case math_cos: {
                return L"compute the cosine";
            }
            case math_cosh: {
                return L"compute the hyperbolic cosine";
            }
            case math_erf: {
                return L"compute the error function";
            }
            case math_erfc: {
                return L"compute the complementary error function";
            }
            case math_exp: {
                return L"returns the high e to the required power";
            }
            case math_exp2: {
                return L"returns 2 high to the required power";
            }
            case math_expm1: {
                return L"Returns high e to the required power minus 1";
            }
            case math_floor: {
                return L"returns the nearest lower integer";
            }
            case math_gcd:
                return L"return Greater Common Divison";
            case math_hcf:
                return L"return Higher Common Factor";
            case math_lgamma: {
                return L"compute the natural logarithm of the absolute value of the gamma function";
            }
            case math_log: {
                return L"compute the natural logarithm (in base e)";
            }
            case math_log10: {
                return L"compute the decimal logarithm (base 10)";
            }
            case math_log1p: {
                return L"compute the natural logarithm (base e) of 1 plus the given number";
            }
            case math_log2: {
                return L"compute the binary logarithm (base 2)";
            }
            case math_logb: {
                return L"extracts the exponent of a number";
            }
            case math_nearbyint: {
                return L"returns the nearest integer using the current rounding method";
            }
            case math_rint: {
                return L"returns the nearest integer using current rounding method with exception if the result is different";
            }
            case math_round: {
                return L"returns the nearest integer, following the rounding rules";
            }
            case math_sin: {
                return L"compute the sine";
            }
            case math_sinh: {
                return L"compute the hyperbolic sine";
            }
            case math_sqrt: {
                return L"compute the square root";
            }
            case math_tan: {
                return L"compute the tangent";
            }
            case math_tanh: {
                return L"compute the hyperbolic tangent";
            }
            case math_tgamma: {
                return L"compute the gamma function";
            }
            case math_trunc: {
                return L"returns the nearest integer whose absolute value is smaller";
            }
            case math_radian: {
                return L"convert a value in 'degree' into 'radian'";
            }
            case math_degree: {
                return L"convert a value in 'radian' into 'degree'";
            }
        }
		return L"";
    }
};


//We are also going to implement the body of the call
void moduleMaths(LispE* lisp) {
    //We first create the body of the function
    lisp->extension("deflib fabs (val)", new Math(lisp, math_fabs));
    lisp->extension("deflib acos (val)", new Math(lisp, math_acos));
    lisp->extension("deflib acosh (val)", new Math(lisp, math_acosh));
    lisp->extension("deflib asin (val)", new Math(lisp, math_asin));
    lisp->extension("deflib asinh (val)", new Math(lisp, math_asinh));
    lisp->extension("deflib atan (val)", new Math(lisp, math_atan));
    lisp->extension("deflib atanh (val)", new Math(lisp, math_atanh));
    lisp->extension("deflib cbrt (val)", new Math(lisp, math_cbrt));
    lisp->extension("deflib ∛ (val)", new Math(lisp, math_cbrt));
    lisp->extension("deflib cos (val)", new Math(lisp, math_cos));
    lisp->extension("deflib cosh (val)", new Math(lisp, math_cosh));
    lisp->extension("deflib erf (val)", new Math(lisp, math_erf));
    lisp->extension("deflib erfc (val)", new Math(lisp, math_erfc));
    lisp->extension("deflib exp (val)", new Math(lisp, math_exp));
    lisp->extension("deflib exp2 (val)", new Math(lisp, math_exp2));
    lisp->extension("deflib expm1 (val)", new Math(lisp, math_expm1));
    lisp->extension("deflib floor (val)", new Math(lisp, math_floor));
    lisp->extension("deflib gcd (val vaal)", new Math(lisp, math_gcd));
    lisp->extension("deflib hcf (val vaal)", new Math(lisp, math_hcf));
    lisp->extension("deflib lgamma (val)", new Math(lisp, math_lgamma));
    lisp->extension("deflib log (val)", new Math(lisp, math_log));
    lisp->extension("deflib log10 (val)", new Math(lisp, math_log10));
    lisp->extension("deflib log1p (val)", new Math(lisp, math_log1p));
    lisp->extension("deflib log2 (val)", new Math(lisp, math_log2));
    lisp->extension("deflib logb (val)", new Math(lisp, math_logb));
    lisp->extension("deflib nearbyint (val)", new Math(lisp, math_nearbyint));
    lisp->extension("deflib rint (val)", new Math(lisp, math_rint));
    lisp->extension("deflib round (val)", new Math(lisp, math_round));
    lisp->extension("deflib sin (val)", new Math(lisp, math_sin));
    lisp->extension("deflib sinh (val)", new Math(lisp, math_sinh));
    lisp->extension("deflib sqrt (val)", new Math(lisp, math_sqrt));
    lisp->extension("deflib √ (val)", new Math(lisp, math_sqrt));
    lisp->extension("deflib tan (val)", new Math(lisp, math_tan));
    lisp->extension("deflib tanh (val)", new Math(lisp, math_tanh));
    lisp->extension("deflib tgamma (val)", new Math(lisp, math_tgamma));
    lisp->extension("deflib trunc (val)", new Math(lisp, math_trunc));
    lisp->extension("deflib radian (val)", new Math(lisp, math_radian));
    lisp->extension("deflib degree (val)", new Math(lisp, math_degree));

    u_ustring nom = U"_pi";
    Element* value = lisp->provideNumber(M_PI);
    lisp->recordingunique(value, lisp->encode(nom));
    nom = U"π";
    lisp->recordingunique(value, lisp->encode(nom));
    
    nom = U"_tau";
    value = lisp->provideNumber(2 * M_PI);
    lisp->recordingunique(value, lisp->encode(nom));
    nom = U"τ";
    lisp->recordingunique(value, lisp->encode(nom));
    
    nom = U"_e";
    value = lisp->provideNumber(M_E);
    lisp->recordingunique(value, lisp->encode(nom));
    nom = U"ℯ";
    lisp->recordingunique(value, lisp->encode(nom));
    
    nom = U"_phi";
    value = lisp->provideNumber(M_GOLDEN);
    lisp->recordingunique(value, lisp->encode(nom));

}
//------ Matrix operations ------------------------
union double32 {
public:
    
    uint32_t bits;
    float v;
    
    double32(float d) {
        v = d;
    }
};

union double64 {
public:
    
    uint64_t bits;
    double v;
    
    double64(double d) {
        v = d;
    }
};

//------------------------------------------------------------------------------------------

Matrice::Matrice(LispE* lisp, Matrice* m) {
    type = t_matrix;
    size_x = m->size_x;
    size_y = m->size_y;
    Numbers* l;
    for (long i = 0; i < size_x; i++) {
        l = lisp->provideNumbers();
        l->liste = ((Numbers*)m->liste[i])->liste;
        append(l);
    }
}

Matrice::Matrice(LispE* lisp, Matrice_float* m) {
    type = t_matrix;
    size_x = m->size_x;
    size_y = m->size_y;
    Numbers* l;
    for (long i = 0; i < size_x; i++) {
        l = lisp->provideNumbers();
        for (long j = 0; j < m->size_y; j++)
            l->liste.push_back(((Floats*)m->liste[i])->liste[j]);
        append(l);
    }
}

Matrice::Matrice(LispE* lisp, long x, long y, double n) {
    type = t_matrix;
    size_x = x;
    size_y = y;

    for (long i = 0; i < size_x; i++) {
        append(lisp->provideNumbers(size_y, n));
    }
}

void Matrice::build(LispE* lisp, Element* lst) {
    Numbers* l;
    long idx = 0;
    for (long x = 0; x < size_x; x++) {
        l = lisp->provideNumbers();
        append(l);
        for (long y = 0; y < size_y; y++) {
            if (idx == lst->size())
                idx = 0;
            l->liste.push_back(lst->index(idx++)->asNumber());
        }
    }
}

Matrice_float::Matrice_float(LispE* lisp, Matrice* m) {
    type = t_matrix_float;
    size_x = m->size_x;
    size_y = m->size_y;
    Floats* l;
    for (long i = 0; i < size_x; i++) {
        l = lisp->provideFloats();
        for (long j = 0; j < m->size_y; j++)
            l->liste.push_back(((Numbers*)m->liste[i])->liste[j]);
        append(l);
    }
}

Matrice_float::Matrice_float(LispE* lisp, Matrice_float* m) {
    type = t_matrix_float;
    size_x = m->size_x;
    size_y = m->size_y;
    Floats* l;
    for (long i = 0; i < size_x; i++) {
        l = lisp->provideFloats();
        l->liste = ((Floats*)m->liste[i])->liste;
        append(l);
    }
}


Matrice_float::Matrice_float(LispE* lisp, long x, long y, float n) {
    type = t_matrix_float;
    size_x = x;
    size_y = y;

    for (long i = 0; i < size_x; i++) {
        append(lisp->provideFloats(size_y, n));
    }
}

void Matrice_float::build(LispE* lisp, Element* lst) {
    Floats* l;
    long idx = 0;
    for (long x = 0; x < size_x; x++) {
        l = lisp->provideFloats();
        append(l);
        for (long y = 0; y < size_y; y++) {
            if (idx == lst->size())
                idx = 0;
            l->liste.push_back(lst->index(idx++)->asFloat());
        }
    }
}

void Tenseur::build(LispE* lisp, long isz, Element* res, double n) {
    if (isz == shape.size()-2) {
        Numbers* lst;
        for (long i = 0; i < shape[isz]; i++) {
            lst = lisp->provideNumbers(shape[isz+1], n);
            res->append(lst);
        }
    }
    else {
        List* lst;
        for (long i = 0; i < shape[isz]; i++) {
            lst = lisp->provideList();
            res->append(lst);
            build(lisp, isz+1, lst, n);
        }
    }
}

void Tenseur::build(LispE* lisp, long isz, Element* res, Element* lst, long& idx) {
    if (isz == shape.size()-2) {
        Numbers* l;
        long i,j;
        for (i = 0; i < shape[isz]; i++) {
            l = lisp->provideNumbers();
            res->append(l);
            for (j = 0; j < shape[isz+1]; j++) {
                if (idx == lst->size())
                    idx = 0;
                l->liste.push_back(lst->index(idx++)->asNumber());
            }
        }
    }
    else {
        List* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = lisp->provideList();
            res->append(l);
            build(lisp, isz+1, l, lst, idx);
        }
    }
}

void Tenseur::build(LispE* lisp, long isz, Element* res) {
    if (isz == shape.size()-2) {
        Numbers* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = lisp->provideNumbers();
            res->append(l);
            l->liste = ((Numbers*)liste[i])->liste;
        }
    }
    else {
        List* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = lisp->provideList();
            res->append(l);
            build(lisp, isz+1,l);
        }
    }
}

void Tenseur_float::build(LispE* lisp, long isz, Element* res, float n) {
    if (isz == shape.size()-2) {
        Floats* lst;
        for (long i = 0; i < shape[isz]; i++) {
            lst = lisp->provideFloats(shape[isz+1], n);
            res->append(lst);
        }
    }
    else {
        List* lst;
        for (long i = 0; i < shape[isz]; i++) {
            lst = lisp->provideList();
            res->append(lst);
            build(lisp, isz+1, lst, n);
        }
    }
}

void Tenseur_float::build(LispE* lisp, long isz, Element* res, Element* lst, long& idx) {
    if (isz == shape.size()-2) {
        Floats* l;
        long i,j;
        for (i = 0; i < shape[isz]; i++) {
            l = lisp->provideFloats();
            res->append(l);
            for (j = 0; j < shape[isz+1]; j++) {
                if (idx == lst->size())
                    idx = 0;
                l->liste.push_back(lst->index(idx++)->asFloat());
            }
        }
    }
    else {
        List* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = lisp->provideList();
            res->append(l);
            build(lisp, isz+1, l, lst, idx);
        }
    }
}

void Tenseur_float::build(LispE* lisp, long isz, Element* res) {
    if (isz == shape.size()-2) {
        Floats* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = lisp->provideFloats();
            res->append(l);
            l->liste = ((Floats*)liste[i])->liste;
        }
    }
    else {
        List* l;
        for (long i = 0; i < shape[isz]; i++) {
            l = lisp->provideList();
            res->append(l);
            build(lisp, isz+1,l);
        }
    }
}
//---------------------------------------------------------------------------------------------------


//LU decomposition
long LUDCMP(long n, vecte<long>& indexes, long& d, Matrice* m) {
    d = 1;
    double AMAX, DUM, thesum;
    long i, i_max = 0, j, k;
    vector<double> values;
    
    for (i = 0; i < n; i++)  {
        AMAX = 0.0;
        for (j = 0; j<n; j++)  {
            thesum = m->val(i, j);
            if (ABS(thesum) > AMAX)
                AMAX = ABS(thesum);
        }
        
        if (AMAX < TINY)
            return 1;
        values.push_back(1.0 / AMAX);
    } // i loop
    
    for (j = 0; j < n; j++)  {
        
        for (i = 0; i < j; i++)  {
            thesum = m->val(i, j);
            for (k = 0; k < i; k++)
            thesum = thesum - m->val(i, k)*m->val(k, j);
            m->set(i,j, thesum);
        } // i loop
        AMAX = 0.0;
        
        for (i = j; i < n; i++)  {
            thesum = m->val(i, j);
            for (k = 0; k < j; k++)
            thesum = thesum - m->val(i, k)*m->val(k, j);
            m->set(i,j, thesum);
            DUM = values[i] * ABS(thesum);
            if (DUM >= AMAX) {
                i_max = i;
                AMAX = DUM;
            }
        } // i loop
        
        if (j != i_max)  {
            for (k = 0; k < n; k++)  {
                DUM = m->val(i_max, k);
                m->set(i_max,k,m->val(j, k));
                m->set(j,k,DUM);
            } // k loop
            d = -d;
            values[i_max] = values[j];
        }
        
        indexes.at(j, i_max);
        
        if (ABS(m->val(j, j)) < TINY)
            m->set(j,j,TINY);
        
        if (j != n - 1)  {
            DUM = 1.0 / m->val(j, j);
            for (i = j + 1; i < n; i++) {
                m->mult(i,j, DUM);
            }
        }
    } // j loop
    
    return 0;
    
} // subroutine LUDCMP

void LUBKSB(long n, vecte<long>& indexes, vecte<double>& b_values, Matrice* m)  {
    double thesum;
    long  i, ii, j, ll;
    
    ii = -1;
    
    for (i = 0; i < n; i++)  {
        ll = indexes[i];
        thesum = b_values[ll];
        b_values.at(ll, b_values[i]);
        if (ii != -1) {
            for (j = ii; j < i; j++) {
                thesum = thesum - m->val(i, j)*b_values[j];
            }
        }
        else {
            if (thesum != 0.0)
                ii = i;
        }
        b_values.at(i, thesum);
    } // i loop
    
    for (i = n - 1; i >= 0; i--)  {
        thesum = b_values[i];
        if (i < n - 1)  {
            for (j = i + 1; j < n; j++) {
                thesum = thesum - m->val(i, j)*b_values[j];
            }
        }
        b_values.at(i, thesum / m->val(i, i));
    } // i loop
    
} // LUBKSB

double Matrice::determinant() {
    if (size_x == 2 && size_y == 2) {
        //then in that case
        return (val(0,0) * val(1,1) - val(1,0) * val(0,1));
    }

    if (size_x != size_y)
        throw new Error("Error: we can only apply 'determinant' to square matrices");
    
    long i;
    i = 0;
    double det = 0;
    for (long j = 0; j < size_x; j++) {
        if (val(i,j) == 0)
            continue;

        Matrice sub(size_x - 1, size_y - 1, 0.0);

        long pc = 0;
        long pr = 0;
        for (long r = 0; r < size_x; r++) {
            if (r == i)
                continue;
            pc = 0;
            for (long c = 0; c < size_y; c++) {
                if (c == j)
                    continue;
                sub.set(pr,pc, val(r,c));
                pc++;
            }
            pr++;
        }
        double sg = pow(-1, (i + j + 2));
        det += val(i,j) * sg*sub.determinant();
    }
    return det;
}

Element* Matrice::inversion(LispE* lisp) {
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'invert' to square matrices");

    //else Local decomposition
    Matrice m(this);
    
    
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
    
    Matrice* Y = new Matrice(lisp, size_x, size_x, 0.0);
    
    long i;
    //We create an identity matrix, which will contain the final result...
    for (i = 0; i < size_x; i++) {
        Y->set(i,i, 1);
    }
    
    vecte<double> temp(size_x);

    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, &m);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}

Element* Matrice::solve(LispE* lisp, Matrice* y) {
    if (size_x != size_y || y->size_x != y->size_y || size_x != y->size_x)
        throw new Error("Error: we can only apply 'solve' to square matrices of equal sizes");

    //else Local decomposition
    Matrice m(this);
        
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
        
    Matrice* Y = new Matrice(lisp, y);
    vecte<double> temp(size_x);
    long i;

    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, &m);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}

Element* Matrice::ludcmp(LispE* lisp) {
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'ludcmp' to square matrices");

    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, this);
    if (rc == 1) {
        return emptylist_;
    }
    Integers* lst = lisp->provideIntegers();
    lst->liste = indexes;
    return lst;
}

Element* Matrice::lubksb(LispE* lisp, Integers* idxs, Matrice* Y) {
    if (size_x != size_y || idxs->size() != size_x) {
        throw new Error("Error: we can only apply 'lubksb' to square matrices with the same number of indexes");
    }
    
    long i;
    if (Y == NULL) {
        Y = new Matrice(lisp, size_x, size_x, 0.0);
        //We create an identity matrix, which will contain the final result...
        for (i = 0; i < size_x; i++) {
            Y->set(i,i, 1);
        }
    }
    else {
        if (Y->size_x != size_x)
            throw new Error("Error: we can only apply 'lubksb' to square matrices of the same shape");
    }
    
    vecte<long> indexes(size_x);
    for (i = 0; i < size_x; i++) {
        indexes.push_back(idxs->liste[i]);
    }

    vecte<double> temp(size_x);

    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, this);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}

long LUDCMP(long n, vecte<long>& indexes, long& d, Matrice_float* m) {
    d = 1;
    float AMAX, DUM, thesum;
    long i, i_max = 0, j, k;
    vector<float> values;
    
    for (i = 0; i < n; i++)  {
        AMAX = 0.0;
        for (j = 0; j<n; j++)  {
            thesum = m->val(i, j);
            if (ABS(thesum) > AMAX)
                AMAX = ABS(thesum);
        }
        
        if (AMAX < TINY)
            return 1;
        values.push_back(1.0 / AMAX);
    } // i loop
    
    for (j = 0; j < n; j++)  {
        
        for (i = 0; i < j; i++)  {
            thesum = m->val(i, j);
            for (k = 0; k < i; k++)
            thesum = thesum - m->val(i, k)*m->val(k, j);
            m->set(i,j, thesum);
        } // i loop
        AMAX = 0.0;
        
        for (i = j; i < n; i++)  {
            thesum = m->val(i, j);
            for (k = 0; k < j; k++)
            thesum = thesum - m->val(i, k)*m->val(k, j);
            m->set(i,j, thesum);
            DUM = values[i] * ABS(thesum);
            if (DUM >= AMAX) {
                i_max = i;
                AMAX = DUM;
            }
        } // i loop
        
        if (j != i_max)  {
            for (k = 0; k < n; k++)  {
                DUM = m->val(i_max, k);
                m->set(i_max,k,m->val(j, k));
                m->set(j,k,DUM);
            } // k loop
            d = -d;
            values[i_max] = values[j];
        }
        
        indexes.at(j, i_max);
        
        if (ABS(m->val(j, j)) < TINY)
            m->set(j,j,TINY);
        
        if (j != n - 1)  {
            DUM = 1.0 / m->val(j, j);
            for (i = j + 1; i < n; i++) {
                m->mult(i,j, DUM);
            }
        }
    } // j loop
    
    return 0;
    
} // subroutine LUDCMP

void LUBKSB(long n, vecte<long>& indexes, vecte<float>& b_values, Matrice_float* m)  {
    float thesum;
    long  i, ii, j, ll;
    
    ii = -1;
    
    for (i = 0; i < n; i++)  {
        ll = indexes[i];
        thesum = b_values[ll];
        b_values.at(ll, b_values[i]);
        if (ii != -1) {
            for (j = ii; j < i; j++) {
                thesum = thesum - m->val(i, j)*b_values[j];
            }
        }
        else {
            if (thesum != 0.0)
                ii = i;
        }
        b_values.at(i, thesum);
    } // i loop
    
    for (i = n - 1; i >= 0; i--)  {
        thesum = b_values[i];
        if (i < n - 1)  {
            for (j = i + 1; j < n; j++) {
                thesum = thesum - m->val(i, j)*b_values[j];
            }
        }
        b_values.at(i, thesum / m->val(i, i));
    } // i loop
    
} // LUBKSB

float Matrice_float::determinant() {
    if (size_x == 2 && size_y == 2) {
        //then in that case
        return (val(0,0) * val(1,1) - val(1,0) * val(0,1));
    }

    if (size_x != size_y)
        throw new Error("Error: we can only apply 'determinant' to square matrices");
    
    long i;
    i = 0;
    float det = 0;
    for (long j = 0; j < size_x; j++) {
        if (val(i,j) == 0)
            continue;

        Matrice_float sub(size_x - 1, size_y - 1, 0.0);

        long pc = 0;
        long pr = 0;
        for (long r = 0; r < size_x; r++) {
            if (r == i)
                continue;
            pc = 0;
            for (long c = 0; c < size_y; c++) {
                if (c == j)
                    continue;
                sub.set(pr,pc, val(r,c));
                pc++;
            }
            pr++;
        }
        float sg = pow(-1, (i + j + 2));
        det += val(i,j) * sg*sub.determinant();
    }
    return det;
}

Element* Matrice_float::inversion(LispE* lisp) {
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'invert' to square matrices");

    //else Local decomposition
    Matrice_float m(this);
    
    
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
    
    Matrice_float* Y = new Matrice_float(lisp, size_x, size_x, 0.0);
    
    long i;
    //We create an identity matrix, which will contain the final result...
    for (i = 0; i < size_x; i++) {
        Y->set(i,i, 1);
    }
    
    vecte<float> temp(size_x);

    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, &m);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}

Element* Matrice_float::solve(LispE* lisp, Matrice_float* y) {
    if (size_x != size_y || y->size_x != y->size_y || size_x != y->size_x)
        throw new Error("Error: we can only apply 'solve' to square matrices of equal sizes");

    //else Local decomposition
    Matrice_float m(this);
        
    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, &m);
    if (rc == 1) {
        return emptylist_;
    }
        
    Matrice_float* Y = new Matrice_float(lisp, y);
    vecte<float> temp(size_x);
    long i;

    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, &m);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}

Element* Matrice_float::ludcmp(LispE* lisp) {
    if (size_x != size_y)
        throw new Error("Error: we can only apply 'ludcmp' to square matrices");

    vecte<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, this);
    if (rc == 1) {
        return emptylist_;
    }
    Integers* lst = lisp->provideIntegers();
    lst->liste = indexes;
    return lst;
}

Element* Matrice_float::lubksb(LispE* lisp, Integers* idxs, Matrice_float* Y) {
    if (size_x != size_y || idxs->size() != size_x) {
        throw new Error("Error: we can only apply 'lubksb' to square matrices with the same number of indexes");
    }
    
    long i;
    if (Y == NULL) {
        Y = new Matrice_float(lisp, size_x, size_x, 0.0);
        //We create an identity matrix, which will contain the final result...
        for (i = 0; i < size_x; i++) {
            Y->set(i,i, 1);
        }
    }
    else {
        if (Y->size_x != size_x)
            throw new Error("Error: we can only apply 'lubksb' to square matrices of the same shape");
    }
    
    vecte<long> indexes(size_x);
    for (i = 0; i < size_x; i++) {
        indexes.push_back(idxs->liste[i]);
    }

    vecte<float> temp(size_x);

    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, this);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}


//------------------------------------------------------------------------------------------
float Element::checkFloat(LispE* lisp) {
    wstring s = L"Error: cannot use this element in an arithmetic expression: '";
    s += asString(lisp);
    s += L"'";
    throw new Error(s);
}

double Element::checkNumber(LispE* lisp) {
    wstring s = L"Error: cannot use this element in an arithmetic expression: '";
    s += asString(lisp);
    s += L"'";
    throw new Error(s);
}

long Element::checkInteger(LispE* lisp) {
    wstring s = L"Error: cannot use this element in an arithmetic expression: '";
    s += asString(lisp);
    s += L"'";
    throw new Error(s);
}
//------------------------------------------------------------------------------------------
Element* Float::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            number += ((Float*)e)->number;
            return this;
        case t_number:
            number += ((Number*)e)->number;
            return this;
        case t_integer:
            number += ((Integer*)e)->integer;
            return this;
        default:
            return plus(lisp, e);
    }
}

Element* Float::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            number -= ((Float*)e)->number;
            return this;
        case t_number:
            number -= ((Number*)e)->number;
            return this;
        case t_integer:
            number -= ((Integer*)e)->integer;
            return this;
        default:
            return minus(lisp, e);
    }
}

Element* Float::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            number *= ((Float*)e)->number;
            return this;
        case t_number:
            number *= ((Number*)e)->number;
            return this;
        case t_integer:
            number *= ((Integer*)e)->integer;
            return this;
        default:
            return multiply(lisp, e);
    }
}

Element* Float::divide_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float: {
            double v = ((Float*)e)->number;
            if (!v)
                throw new Error("Error: division by zero");
            number /= v;
            return this;
        }
        case t_number: {
            double v = ((Number*)e)->number;
            if (!v)
                throw new Error("Error: division by zero");
            number /= v;
            return this;
        }
        case t_integer: {
            double v = ((Integer*)e)->integer;
            if (!v)
                throw new Error("Error: division by zero");
            number /= v;
            return this;
        }
        default:
            return divide(lisp, e);
    }
}

Element* Float::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->plus(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        number += e->checkFloat(lisp);
        return this;
    }
    return lisp->provideFloat(number+e->checkFloat(lisp));
}

Element* Float::minus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->minus(lisp, e);
    }
    if (status != s_constant) {
        number -= e->checkFloat(lisp);
        return this;
    }
    return lisp->provideFloat(number-e->checkFloat(lisp));
}

Element* Float::multiply(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->multiply(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        number *= e->checkFloat(lisp);
        return this;
    }
    return lisp->provideFloat(number*e->checkFloat(lisp));
}

Element* Float::divide(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->divide(lisp, e);
    }
    float v = e->checkFloat(lisp);
    if (!v)
        throw new Error("Error: division by zero");
    if (status != s_constant) {
        number /= v;
        return this;
    }
    return lisp->provideFloat(number/v);
}

Element* Float::mod(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->mod(lisp, e);
    }

    long v = e->checkInteger(lisp);
    if (!v)
        throw new Error("Error: division by zero");

    if (status != s_constant) {
        number = (long)number % v;
        return this;
    }
    return lisp->provideFloat((long)number%v);
}

Element* Float::power(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->power(lisp, e);
    }
    if (status != s_constant) {
        number = pow(number, e->checkFloat(lisp));
        return this;
    }
    return lisp->provideFloat(pow(number, e->checkFloat(lisp)));
}

Element* Float::bit_not(LispE* lisp)  {
    double32 d(number);
    d.bits = ~d.bits;
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideFloat(d.v);
}


Element* Float::bit_and_not(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_and_not(lisp, e);
    }
    double32 d(number);
    d.bits &= ~e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideFloat(d.v);
}

Element* Float::bit_and(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->bit_and(lisp, this);
        release();
        return n;
    }
    double32 d(number);
    d.bits &= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideFloat(d.v);
}


Element* Float::bit_or(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->bit_or(lisp, this);
        release();
        return n;
    }
    double32 d(number);
    d.bits |= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideFloat(d.v);
}

Element* Float::bit_xor(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->bit_xor(lisp, this);
        release();
        return n;
    }
    double32 d(number);
    d.bits ^= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideFloat(d.v);
}

Element* Float::leftshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->leftshift(lisp, e);
    }
    double32 d(number);
    d.bits <<= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideFloat(d.v);
}

Element* Float::rightshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->rightshift(lisp, e);
    }
    double32 d(number);
    d.bits >>= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideFloat(d.v);
}

Element* Number::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            number += ((Float*)e)->number;
            return this;
        case t_number:
            number += ((Number*)e)->number;
            return this;
        case t_integer:
            number += ((Integer*)e)->integer;
            return this;
        default:
            return plus(lisp, e);
    }
}

Element* Number::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            number -= ((Float*)e)->number;
            return this;
        case t_number:
            number -= ((Number*)e)->number;
            return this;
        case t_integer:
            number -= ((Integer*)e)->integer;
            return this;
        default:
            return minus(lisp, e);
    }
}

Element* Number::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float:
            number *= ((Float*)e)->number;
            return this;
        case t_number:
            number *= ((Number*)e)->number;
            return this;
        case t_integer:
            number *= ((Integer*)e)->integer;
            return this;
        default:
            return multiply(lisp, e);
    }
}

Element* Number::divide_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float: {
            double v = ((Float*)e)->number;
            if (!v)
                throw new Error("Error: division by zero");
            number /= v;
            return this;
        }
        case t_number: {
            double v = ((Number*)e)->number;
            if (!v)
                throw new Error("Error: division by zero");
            number /= v;
            return this;
        }
        case t_integer: {
            double v = ((Integer*)e)->integer;
            if (!v)
                throw new Error("Error: division by zero");
            number /= v;
            return this;
        }
        default:
            return divide(lisp, e);
    }
}

Element* Number::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->plus(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        number += e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(number+e->checkNumber(lisp));
}

Element* Number::minus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->minus(lisp, e);
    }
    if (status != s_constant) {
        number -= e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(number-e->checkNumber(lisp));
}

Element* Number::multiply(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->multiply(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        number *= e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(number*e->checkNumber(lisp));
}

Element* Number::divide(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->divide(lisp, e);
    }
    double v = e->checkNumber(lisp);
    if (!v)
        throw new Error("Error: division by zero");
    if (status != s_constant) {
        number /= v;
        return this;
    }
    return lisp->provideNumber(number/v);
}

Element* Number::mod(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->mod(lisp, e);
    }

    long v = e->checkInteger(lisp);
    if (!v)
        throw new Error("Error: division by zero");

    if (status != s_constant) {
        number = (long)number % v;
        return this;
    }
    return lisp->provideNumber((long)number%v);
}

Element* Number::power(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->power(lisp, e);
    }
    if (status != s_constant) {
        number = pow(number, e->checkNumber(lisp));
        return this;
    }
    return lisp->provideNumber(pow(number, e->checkNumber(lisp)));
}

Element* Number::bit_not(LispE* lisp)  {
    double64 d(number);
    d.bits = ~d.bits;
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}


Element* Number::bit_and_not(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_and_not(lisp, e);
    }
    double64 d(number);
    d.bits &= ~e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::bit_and(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->bit_and(lisp, this);
        release();
        return n;
    }
    double64 d(number);
    d.bits &= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}


Element* Number::bit_or(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->bit_or(lisp, this);
        release();
        return n;
    }
    double64 d(number);
    d.bits |= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::bit_xor(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->bit_xor(lisp, this);
        release();
        return n;
    }
    double64 d(number);
    d.bits ^= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::leftshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->leftshift(lisp, e);
    }
    double64 d(number);
    d.bits <<= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::rightshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->rightshift(lisp, e);
    }
    double64 d(number);
    d.bits >>= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Integer::plus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->plus(lisp, this);
        release();
        return n;
    }

    if (e->type == t_float) {
        float v = (float)integer + e->checkFloat(lisp);
        release();
        return lisp->provideFloat(v);
    }

    if (e->type == t_number) {
        double v = (double)integer + e->checkNumber(lisp);
        release();
        return lisp->provideNumber(v);
    }
    if (status != s_constant) {
        integer += e->checkInteger(lisp);
        return this;
    }
        
    return lisp->provideInteger(integer+e->checkInteger(lisp));
}

Element* Integer::minus(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->minus(lisp, e);
    }
    if (e->type == t_float) {
        float v = (float)integer - e->checkFloat(lisp);
        release();
        return lisp->provideFloat(v);
    }

    if (e->type == t_number) {
        double v = (double)integer - e->checkNumber(lisp);
        release();
        return lisp->provideNumber(v);
    }
    if (status != s_constant) {
        integer -= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer-e->checkInteger(lisp));
}

Element* Integer::multiply(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->multiply(lisp, this);
        release();
        return n;
    }
    if (e->type == t_float) {
        float v = (float)integer * e->checkFloat(lisp);
        release();
        return lisp->provideFloat(v);
    }

    if (e->type == t_number) {
        double v = (double)integer * e->checkNumber(lisp);
        release();
        return lisp->provideNumber(v);
    }
    if (status != s_constant) {
        integer *= e->asInteger();
        return this;
    }
    return lisp->provideInteger(integer*e->asInteger());
}

Element* Integer::divide_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_float: {
            float v =  ((Float*)e)->number;
            if (!v)
                throw new Error("Error: division by zero");
            release();
            return lisp->provideFloat(((float)integer)/v);
        }
        case t_number: {
            double v =  ((Number*)e)->number;
            if (!v)
                throw new Error("Error: division by zero");
            release();
            return lisp->provideNumber(((double)integer)/v);
        }
        case t_integer: {
            double v =  ((Integer*)e)->integer;
            if (!v)
                throw new Error("Error: division by zero");
            release();
            return lisp->provideNumber(((double)integer)/v);
        }
        default:
            return divide(lisp, e);
    }
}

Element* Integer::divide(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->divide(lisp, e);
    }
    double v =  e->checkNumber(lisp);
    if (!v)
        throw new Error("Error: division by zero");
    release();
    return lisp->provideNumber((double)integer/v);
}


Element* Integer::mod(LispE* lisp, Element* e) {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->mod(lisp, e);
    }
    long v =  e->checkInteger(lisp);
    if (!v)
        throw new Error("Error: division by zero");
    
    if (status != s_constant) {
        integer %= v;
        return this;
    }
    return lisp->provideInteger(integer%v);
}

Element* Integer::power(LispE* lisp, Element* e) {
    if (e->isList()) {
        Numbers* n = new Numbers(e->size(), integer);
        return n->power(lisp, e);
    }
    double v = pow((double)integer, e->checkNumber(lisp));
    release();
    return lisp->provideNumber(v);
}

Element* Integer::bit_not(LispE* lisp)  {
    if (status != s_constant) {
        integer = ~integer;
        return this;
    }
    return lisp->provideInteger(~integer);
}


Element* Integer::bit_and(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->bit_and(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        integer &= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer&e->checkInteger(lisp));
}

Element* Integer::bit_and_not(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->bit_and_not(lisp, e);
    }
    if (status != s_constant) {
        integer &= ~e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer&~e->checkInteger(lisp));
}

Element* Integer::bit_or(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->bit_or(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        integer |= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer|e->checkInteger(lisp));
}

Element* Integer::bit_xor(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->copyatom(1);
        n = n->bit_xor(lisp, this);
        release();
        return n;
    }
    if (status != s_constant) {
        integer ^= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer^e->checkInteger(lisp));
}


Element* Integer::leftshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->leftshift(lisp, e);
    }
    if (status != s_constant) {
        integer <<= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer<<e->checkInteger(lisp));
}


Element* Integer::rightshift(LispE* lisp, Element* e)  {
    if (e->isList()) {
        Element* n = e->newInstance(this);
        release();
        return n->rightshift(lisp, e);
    }
    if (status != s_constant) {
        integer >>= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer>>e->checkInteger(lisp));
}

Element* List::bit_not(LispE* l) {
    //Two cases either e is a number or it is a list...
    if (!status) {
        for (long i = 0; i < size(); i++) {
            replacing(i, liste[i]->bit_not(l));
        }
        return this;
    }
    List* lst = (List*)newInstance();
    for (long i = 0; i < size(); i++) {
        lst->append(liste[i]->bit_not(l));
    }
    return lst;
}


Element* List::bit_and(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->bit_and(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->bit_and(lisp, e));
    }
    return this;
}

Element* List::bit_and_not(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->bit_and_not(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->bit_and_not(lisp, e));
    }
    return this;
}


Element* List::bit_or(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->bit_or(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->bit_or(lisp, e));
    }
    return this;
}

Element* List::bit_xor(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->bit_xor(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->bit_xor(lisp, e));
    }
    return this;
}

Element* List::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->plus(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->plus(lisp, e));
    }
    return this;
}

Element* List::minus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->minus(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->minus(lisp, e));
    }
    return this;
}

Element* List::multiply(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->multiply(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->multiply(lisp, e));
    }
    return this;
}

Element* List::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->divide(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->divide(lisp, e));
    }
    return this;
}

Element* List::mod(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->mod(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->mod(lisp, e));
    }
    return this;
}

Element* List::power(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->power(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->power(lisp, e));
    }
    return this;
}

Element* List::leftshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->leftshift(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->leftshift(lisp, e));
    }
    return this;
}

Element* List::rightshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, liste[i]->rightshift(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, liste[i]->rightshift(lisp, e));
    }
    return this;
}

Element* Floats::bit_not(LispE* l) {
    //Two cases either e is a number or it is a list...
    if (!status) {
        for (long i = 0; i < size(); i++) {
            replacing(i, index(i)->bit_not(l));
        }
        return this;
    }
    double32 d(0);
    Floats* num = (Floats*)newInstance();
    for (long i = 0; i < size(); i++) {
        d.v = liste[i];
        d.bits = ~d.bits;
        num->liste.push_back(d.v);
    }
    return num;
}


Element* Floats::bit_and(LispE* lisp, Element* e) {
    if (e == NULL) {
        double32 r(0);
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits &= r.bits;
        }
        return lisp->provideFloat(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_and(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_and(lisp, e));
    }
    return this;
}

Element* Floats::bit_and_not(LispE* lisp, Element* e) {
    if (e == NULL) {
        double32 r(0);
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits &= ~r.bits;
        }
        return lisp->provideFloat(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and_not(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_and_not(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_and_not(lisp, e));
    }
    return this;
}

Element* Floats::bit_or(LispE* lisp, Element* e) {
    if (e == NULL) {
        double32 r(0);
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits |= r.bits;
        }
        return lisp->provideFloat(d.v);
    }

    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_or(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_or(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_or(lisp, e));
    }
    return this;
}

Element* Floats::bit_xor(LispE* lisp, Element* e) {
    if (e == NULL) {
        double32 r(0);
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits ^= r.bits;
        }
        return lisp->provideFloat(d.v);
    }

    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_xor(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_xor(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_xor(lisp, e));
    }
    return this;
}

Element* Floats::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_floats: {
            Floats* n = (Floats*)e;
            long szl = liste.size();
            long i = n->liste.size();

#ifdef INTELINTRINSICS
            if (szl >= 3 && i >= 3) {
                szl = lmin(szl, i);
                liste.padding((8 - (szl & 7)) & 7);
                liste.padding((8 - (szl & 7)) & 7);
                for (i = 0; i < szl; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_add_ps(_mm256_loadu_ps(&liste.vecteur[i]), _mm256_loadu_ps(&n->liste.vecteur[i])));
                }
                return this;
            }
#endif
            szl = lmin(szl, i);
            for (i = 0; i < szl; i++) {
                liste.vecteur[i] += n->liste[i];
            }
            return this;
        }
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] += ((Integers*)e)->liste[i];
            }
            return this;
        case t_numbers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] += ((Numbers*)e)->liste[i];
            }
            return this;
        case t_float:
        case t_number:
        case t_integer: {
            long szl = liste.size();
            float v = e->asFloat();
#ifdef INTELINTRINSICS
            if (szl >= 3) {
                liste.padding((8 - (szl & 7)) & 7);
                __m256 vb = {v, v, v, v, v, v, v, v};
                for (long i = 0; i < szl; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_add_ps(_mm256_loadu_ps(&liste.vecteur[i]), vb));
                }
                return this;
            }
#endif
            for (long i = 0; i < szl; i++) {
                liste.vecteur[i] += v;
            }
            return this;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
#ifdef INTELINTRINSICS
            long szl = liste.size();
            long sze = result->size_y;
            liste.padding((8 - (szl & 7)) & 7);
            long pade = (8 - (sze & 7)) & 7;
            long nb = lmin(szl, sze);

            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                n->liste.padding(pade);
                for (long i = 0; i < nb; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_add_ps(_mm256_loadu_ps(&liste.vecteur[i]), _mm256_loadu_ps(&n->liste.vecteur[i])));                }
            }
#else
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] += liste[i];
                }
            }
#endif
            release();
            return result;
        }
        default:
            return plus(lisp, e);
    }
}


Element* Floats::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_floats: {
            Floats* n = (Floats*)e;
            long szl = liste.size();
            long i = n->liste.size();

#ifdef INTELINTRINSICS
            if (szl >= 3 && i >= 3) {
                szl = lmin(szl, i);
                liste.padding((8 - (szl & 7)) & 7);
                liste.padding((8 - (szl & 7)) & 7);
                for (i = 0; i < szl; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_sub_ps(_mm256_loadu_ps(&liste.vecteur[i]), _mm256_loadu_ps(&n->liste.vecteur[i])));
                }
                return this;
            }
#endif
            szl = lmin(szl, i);
            for (i = 0; i < szl; i++) {
                liste.vecteur[i] -= n->liste.vecteur[i];
            }
            return this;
        }
        case t_numbers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] -= ((Numbers*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] -= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_float:
        case t_integer: {
            long szl = liste.size();
            float v = e->asFloat();
#ifdef INTELINTRINSICS
            if (szl >= 3) {
                liste.padding((8 - (szl & 7)) & 7);
                __m256 vb = {v, v, v, v, v, v, v, v};
                for (long i = 0; i < szl; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_sub_ps(_mm256_loadu_ps(&liste.vecteur[i]), vb));
                }
                return this;
            }
#endif
            for (long i = 0; i < szl; i++) {
                liste.vecteur[i] -= v;
            }
            return this;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] = liste[i] - n->liste.vecteur[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
#ifdef INTELINTRINSICS
            long szl = liste.size();
            long sze = result->size_y;
            liste.padding((8 - (szl & 7)) & 7);
            long pade = (8 - (sze & 7)) & 7;
            long nb = lmin(szl, sze);

            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                n->liste.padding(pade);
                for (long i = 0; i < nb; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_sub_ps(_mm256_loadu_ps(&liste.vecteur[i]), _mm256_loadu_ps(&n->liste.vecteur[i])));
                }
            }
#else
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] = liste[i] - n->liste.vecteur[i];
                }
            }
#endif
            release();
            return result;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Floats::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_floats: {
            Floats* n = (Floats*)e;
            long szl = liste.size();
            long i = n->liste.size();

#ifdef INTELINTRINSICS
            if (szl >= 3 && i >= 3) {
                szl = lmin(szl, i);
                liste.padding((8 - (szl & 7)) & 7);
                liste.padding((8 - (szl & 7)) & 7);
                for (i = 0; i < szl; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_mul_ps(_mm256_loadu_ps(&liste.vecteur[i]), _mm256_loadu_ps(&n->liste.vecteur[i])));
                }
                return this;
            }
#endif

            szl = lmin(szl, i);
            for (i = 0; i < szl; i++) {
                liste.vecteur[i] *= n->liste.vecteur[i];
            }
            return this;
        }
        case t_numbers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] *= ((Numbers*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] *= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_float:
        case t_integer: {
            long szl = liste.size();
            float v = e->asFloat();
#ifdef INTELINTRINSICS
            if (szl >= 3) {
                liste.padding((8 - (szl & 7)) & 7);
                __m256 vb = {v, v, v, v, v, v, v, v};
                for (long i = 0; i < szl; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_mul_ps(_mm256_loadu_ps(&liste.vecteur[i]), vb));
                }
                return this;
            }
#endif
            for (long i = 0; i < szl; i++) {
                liste.vecteur[i] *= v;
            }
            return this;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
#ifdef INTELINTRINSICS
            long szl = liste.size();
            long sze = result->size_y;
            liste.padding((8 - (szl & 7)) & 7);
            long pade = (8 - (sze & 7)) & 7;
            long nb = lmin(szl, sze);

            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                n->liste.padding(pade);
                for (long i = 0; i < nb; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_mul_ps(_mm256_loadu_ps(&liste.vecteur[i]), _mm256_loadu_ps(&n->liste.vecteur[i])));
                }
            }
#else
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] *= liste[i];
                }
            }
#endif
            release();
            return result;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Floats::divide_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_floats: {
            Floats* n = (Floats*)e;
            if (n->liste.check(0))
                throw new Error("Error: division by zero");

            long szl = liste.size();
            long i = n->liste.size();

#ifdef INTELINTRINSICS
            if (szl >= 3 && i >= 3) {
                szl = lmin(szl, i);
                liste.padding((8 - (szl & 7)) & 7);
                liste.padding((8 - (szl & 7)) & 7);
                for (i = 0; i < szl; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_div_ps(_mm256_loadu_ps(&liste.vecteur[i]), _mm256_loadu_ps(&n->liste.vecteur[i])));
                }
                return this;
            }
#endif
            szl = lmin(szl, i);
            for (i = 0; i < szl; i++)
                liste.vecteur[i] /= n->liste.vecteur[i];
            return this;
        }
        case t_numbers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Numbers*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste.vecteur[i] /= ((Numbers*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Integers*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste.vecteur[i] /= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_float:
        case t_integer: {
            float v = e->asFloat();
            if (!v)
                throw new Error("Error: division by zero");
            long szl = liste.size();
#ifdef INTELINTRINSICS
            if (szl >= 3) {
                liste.padding((8 - (szl & 7)) & 7, 1);
                __m256 vb = {v, v, v, v, v, v, v, v};
                for (long i = 0; i < szl; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_mul_ps(_mm256_loadu_ps(&liste.vecteur[i]), vb));
                }
                return this;
            }
#endif
            for (long i = 0; i < szl; i++) {
                liste.vecteur[i] /= v;
            }
            return this;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    if (n->liste.check(0)) {
                        result->release();
                        throw new Error("Error: division by zero");
                    }
                    n->liste.vecteur[i] = liste.vecteur[i] / n->liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;

#ifdef INTELINTRINSICS
            long szl = liste.size();
            long sze = result->size_y;
            liste.padding((8 - (szl & 7)) & 7, 1);
            long pade = (8 - (sze & 7)) & 7;
            long nb = lmin(szl, sze);
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                if (n->liste.check(0)) {
                    result->release();
                    throw new Error("Error: division by zero");
                }
                n->liste.padding(pade, 1);
                for (long i = 0; i < nb; i+= 8) {
                    _mm256_storeu_ps(&liste.vecteur[i], _mm256_mul_ps(_mm256_loadu_ps(&liste.vecteur[i]), _mm256_loadu_ps(&n->liste.vecteur[i])));
                }
            }
#else
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                if (n->liste.check(0)) {
                    result->release();
                    throw new Error("Error: division by zero");
                }
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] = liste.vecteur[i] / n->liste[i];
                }
            }
#endif
            release();
            return result;
        }
        default:
            return divide(lisp, e);
    }
}


Element* Floats::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d += liste[i];
        }
        return lisp->provideFloat(d);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->plus(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }

        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] += e->index(i)->asNumber();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] += e->asNumber();
    }
    return this;
}

Element* Floats::minus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d -= liste[i];
        }
        return lisp->provideFloat(d);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->minus(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] -= e->index(i)->asNumber();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] -= e->asNumber();
    }
    return this;
}

Element* Floats::multiply(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d *= liste[i];
        }
        return lisp->provideFloat(d);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->multiply(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] *= e->index(i)->asNumber();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] *= e->asNumber();
    }
    return this;
}

Element* Floats::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (!liste[i])
                throw new Error("Error: division by zero");
            d /= liste[i];
        }
        return lisp->provideFloat(d);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->divide(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->divide(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->divide(lisp, e));
    }
    return this;
}

Element* Floats::mod(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (liste[i] == 0)
                throw new Error("Error: division by zero");
            d %= (long)liste[i];
        }
        return lisp->provideFloat(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->mod(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->mod(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->mod(lisp, e));
    }
    return this;
}

Element* Floats::power(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d = pow(d,liste[i]);
        }
        return lisp->provideFloat(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->power(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->power(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->power(lisp, e));
    }
    return this;
}

Element* Floats::leftshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            d.bits <<= (long)liste[i];
        }
        return lisp->provideFloat(d.v);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->leftshift(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->leftshift(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->leftshift(lisp, e));
    }
    return this;
}

Element* Floats::rightshift(LispE* lisp, Element* e) {
    if (e == NULL) {
        double32 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            d.bits >>= (long)liste[i];
        }
        return lisp->provideFloat(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->rightshift(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->rightshift(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->rightshift(lisp, e));
    }
    return this;
}

Element* Numbers::bit_not(LispE* l) {
    //Two cases either e is a number or it is a list...
    if (!status) {
        for (long i = 0; i < size(); i++) {
            replacing(i, index(i)->bit_not(l));
        }
        return this;
    }
    double64 d(0);
    Numbers* num = (Numbers*)newInstance();
    for (long i = 0; i < size(); i++) {
        d.v = liste[i];
        d.bits = ~d.bits;
        num->liste.push_back(d.v);
    }
    return num;
}


Element* Numbers::bit_and(LispE* lisp, Element* e) {
    if (e == NULL) {
        double64 r(0);
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits &= r.bits;
        }
        return lisp->provideNumber(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_and(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_and(lisp, e));
    }
    return this;
}

Element* Numbers::bit_and_not(LispE* lisp, Element* e) {
    if (e == NULL) {
        double64 r(0);
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits &= ~r.bits;
        }
        return lisp->provideNumber(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and_not(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_and_not(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_and_not(lisp, e));
    }
    return this;
}

Element* Numbers::bit_or(LispE* lisp, Element* e) {
    if (e == NULL) {
        double64 r(0);
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits |= r.bits;
        }
        return lisp->provideNumber(d.v);
    }

    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_or(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_or(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_or(lisp, e));
    }
    return this;
}

Element* Numbers::bit_xor(LispE* lisp, Element* e) {
    if (e == NULL) {
        double64 r(0);
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            r.v = liste[i];
            d.bits ^= r.bits;
        }
        return lisp->provideNumber(d.v);
    }

    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_xor(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->bit_xor(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->bit_xor(lisp, e));
    }
    return this;
}

Element* Numbers::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            Numbers* n = (Numbers*)e;
            long szl = liste.size();
            long i = n->liste.size();

#ifdef INTELINTRINSICS
            if (szl >= 3 && i >= 3) {
                szl = lmin(szl, i);
                liste.padding((4 - (szl & 3)) & 3);
                liste.padding((4 - (szl & 3)) & 3);
                for (i = 0; i < szl; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_add_pd(_mm256_loadu_pd(&liste.vecteur[i]), _mm256_loadu_pd(&n->liste.vecteur[i])));
                }
                return this;
            }
#endif
            szl = lmin(szl, i);
            for (i = 0; i < szl; i++) {
                liste.vecteur[i] += n->liste[i];
            }
            return this;
        }
        case t_floats:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] += ((Floats*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] += ((Integers*)e)->liste[i];
            }
            return this;
        case t_float:
        case t_number:
        case t_integer: {
            long szl = liste.size();
            double v = e->asNumber();
#ifdef INTELINTRINSICS
            if (szl >= 3) {
                liste.padding((4 - (szl & 3)) & 3);
                __m256d vb = {v, v, v, v};
                for (long i = 0; i < szl; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_add_pd(_mm256_loadu_pd(&liste.vecteur[i]), vb));
                }
                return this;
            }
#endif
            for (long i = 0; i < szl; i++) {
                liste.vecteur[i] += v;
            }
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] += liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;

#ifdef INTELINTRINSICS
            long szl = liste.size();
            long sze = result->size_y;
            liste.padding((4 - (szl & 3)) & 3);
            long pade = (4 - (sze & 3)) & 3;
            long nb = lmin(szl, sze);

            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                n->liste.padding(pade);
                for (long i = 0; i < nb; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_add_pd(_mm256_loadu_pd(&liste.vecteur[i]), _mm256_loadu_pd(&n->liste.vecteur[i])));
                }
            }
#else
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] += liste[i];
                }
            }
#endif
            release();
            return result;
        }
        default:
            return plus(lisp, e);
    }
}


Element* Numbers::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            Numbers* n = (Numbers*)e;
            long szl = liste.size();
            long i = n->liste.size();

#ifdef INTELINTRINSICS
            if (szl >= 3 && i >= 3) {
                szl = lmin(szl, i);
                liste.padding((4 - (szl & 3)) & 3);
                liste.padding((4 - (szl & 3)) & 3);
                for (i = 0; i < szl; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_sub_pd(_mm256_loadu_pd(&liste.vecteur[i]), _mm256_loadu_pd(&n->liste.vecteur[i])));
                }
                return this;
            }
#endif
            szl = lmin(szl, i);
            for (i = 0; i < szl; i++) {
                liste.vecteur[i] -= n->liste.vecteur[i];
            }
            return this;
        }
        case t_floats:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] -= ((Floats*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] -= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_float:
        case t_integer: {
            long szl = liste.size();
            double v = e->asNumber();
#ifdef INTELINTRINSICS
            if (szl >= 3) {
                liste.padding((4 - (szl & 3)) & 3);
                __m256d vb = {v, v, v, v};
                for (long i = 0; i < szl; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_sub_pd(_mm256_loadu_pd(&liste.vecteur[i]), vb));
                }
                return this;
            }
#endif
            for (long i = 0; i < szl; i++) {
                liste.vecteur[i] -= v;
            }
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] = liste[i] - n->liste.vecteur[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;

#ifdef INTELINTRINSICS
            long szl = liste.size();
            long sze = result->size_y;
            liste.padding((4 - (szl & 3)) & 3);
            long pade = (4 - (sze & 3)) & 3;
            long nb = lmin(szl, sze);

            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                n->liste.padding(pade);
                for (long i = 0; i < nb; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_sub_pd(_mm256_loadu_pd(&liste.vecteur[i]), _mm256_loadu_pd(&n->liste.vecteur[i])));
                }
            }
#else
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] = liste[i] - n->liste.vecteur[i];
                }
            }
#endif
            release();
            return result;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Numbers::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            Numbers* n = (Numbers*)e;
            long szl = liste.size();
            long i = n->liste.size();

#ifdef INTELINTRINSICS
            if (szl >= 3 && i >= 3) {
                szl = lmin(szl, i);
                liste.padding((4 - (szl & 3)) & 3);
                liste.padding((4 - (szl & 3)) & 3);
                for (i = 0; i < szl; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_mul_pd(_mm256_loadu_pd(&liste.vecteur[i]), _mm256_loadu_pd(&n->liste.vecteur[i])));
                }
                return this;
            }
#endif

            szl = lmin(szl, i);
            for (i = 0; i < szl; i++) {
                liste.vecteur[i] *= n->liste.vecteur[i];
            }
            return this;
        }
        case t_floats:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] *= ((Floats*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] *= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_float:
        case t_integer: {
            long szl = liste.size();
            double v = e->asNumber();
#ifdef INTELINTRINSICS
            if (szl >= 3) {
                liste.padding((4 - (szl & 3)) & 3);
                __m256d vb = {v, v, v, v};
                for (long i = 0; i < szl; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_mul_pd(_mm256_loadu_pd(&liste.vecteur[i]), vb));
                }
                return this;
            }
#endif
            for (long i = 0; i < szl; i++) {
                liste.vecteur[i] *= v;
            }
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] *= liste[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;

#ifdef INTELINTRINSICS
            long szl = liste.size();
            long sze = result->size_y;
            liste.padding((4 - (szl & 3)) & 3);
            long pade = (4 - (sze & 3)) & 3;

            long nb = lmin(szl, sze);
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                n->liste.padding(pade);
                for (long i = 0; i < nb; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_mul_pd(_mm256_loadu_pd(&liste.vecteur[i]), _mm256_loadu_pd(&n->liste.vecteur[i])));
                }
            }
#else
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] *= liste[i];
                }
            }
#endif
            release();
            return result;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Numbers::divide_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            Numbers* n = (Numbers*)e;
            if (n->liste.check(0))
                throw new Error("Error: division by zero");

            long szl = liste.size();
            long i = n->liste.size();

#ifdef INTELINTRINSICS
            if (szl >= 3 && i >= 3) {
                szl = lmin(szl, i);
                liste.padding((4 - (szl & 3)) & 3);
                liste.padding((4 - (szl & 3)) & 3);
                for (i = 0; i < szl; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_div_pd(_mm256_loadu_pd(&liste.vecteur[i]), _mm256_loadu_pd(&n->liste.vecteur[i])));
                }
                return this;
            }
#endif
            szl = lmin(szl, i);
            for (i = 0; i < szl; i++)
                liste.vecteur[i] /= n->liste.vecteur[i];
            return this;
        }
        case t_floats:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Floats*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste.vecteur[i] /= ((Floats*)e)->liste[i];
            }
            return this;
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Integers*)e)->liste[i])
                    throw new Error("Error: division by zero");
                liste.vecteur[i] /= ((Integers*)e)->liste[i];
            }
            return this;
        case t_number:
        case t_float:
        case t_integer: {
            double v = e->asNumber();
            if (!v)
                throw new Error("Error: division by zero");
            long szl = liste.size();
#ifdef INTELINTRINSICS
            if (szl >= 3) {
                liste.padding((4 - (szl & 3)) & 3, 1);
                __m256d vb = {v, v, v, v};
                for (long i = 0; i < szl; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_div_pd(_mm256_loadu_pd(&liste.vecteur[i]), vb));
                }
                return this;
            }
#endif
            for (long i = 0; i < szl; i++) {
                liste.vecteur[i] /= v;
            }
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            Floats* n;
            for (long m = 0; m < result->size_x; m++) {
                n = (Floats*)result->index(m);
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    if (n->liste.check(0)) {
                        result->release();
                        throw new Error("Error: division by zero");
                    }
                    n->liste.vecteur[i] = liste[i] / n->liste.vecteur[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            Numbers* n;

#ifdef INTELINTRINSICS
            long szl = liste.size();
            long sze = result->size_y;
            liste.padding((4 - (szl & 3)) & 3, 1);
            
            long pade = (4 - (sze & 3)) & 3;

            long nb = lmin(szl, sze);
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                if (n->liste.check(0)) {
                    result->release();
                    throw new Error("Error: division by zero");
                }
                n->liste.padding(pade, 1);
                for (long i = 0; i < nb; i+= 4) {
                    _mm256_storeu_pd(&liste.vecteur[i], _mm256_div_pd(_mm256_loadu_pd(&liste.vecteur[i]), _mm256_loadu_pd(&n->liste.vecteur[i])));
                }
            }
#else
            for (long m = 0; m < result->size_x; m++) {
                n = (Numbers*)result->index(m);
                if (n->liste.check(0)) {
                    result->release();
                    throw new Error("Error: division by zero");
                }
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    n->liste.vecteur[i] = liste.vecteur[i] / n->liste[i];
                }
            }
#endif
            release();
            return result;
        }
        default:
            return divide(lisp, e);
    }
}


Element* Numbers::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d += liste[i];
        }
        return lisp->provideNumber(d);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->plus(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }

        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] += e->index(i)->asNumber();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] += e->asNumber();
    }
    return this;
}

Element* Numbers::minus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d -= liste[i];
        }
        return lisp->provideNumber(d);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->minus(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] -= e->index(i)->asNumber();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] -= e->asNumber();
    }
    return this;
}

Element* Numbers::multiply(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d *= liste[i];
        }
        return lisp->provideNumber(d);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->multiply(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] *= e->index(i)->asNumber();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] *= e->asNumber();
    }
    return this;
}

Element* Numbers::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (!liste[i])
                throw new Error("Error: division by zero");
            d /= liste[i];
        }
        return lisp->provideNumber(d);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->divide(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->divide(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->divide(lisp, e));
    }
    return this;
}

Element* Numbers::mod(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (liste[i] == 0)
                throw new Error("Error: division by zero");
            d %= (long)liste[i];
        }
        return lisp->provideNumber(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->mod(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->mod(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->mod(lisp, e));
    }
    return this;
}

Element* Numbers::power(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d = pow(d,liste[i]);
        }
        return lisp->provideNumber(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->power(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->power(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->power(lisp, e));
    }
    return this;
}

Element* Numbers::leftshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            d.bits <<= (long)liste[i];
        }
        return lisp->provideNumber(d.v);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->leftshift(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->leftshift(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->leftshift(lisp, e));
    }
    return this;
}

Element* Numbers::rightshift(LispE* lisp, Element* e) {
    if (e == NULL) {
        double64 d(liste[0]);
        for (long i = 1; i < size(); i++) {
            d.bits >>= (long)liste[i];
        }
        return lisp->provideNumber(d.v);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->rightshift(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->rightshift(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->rightshift(lisp, e));
    }
    return this;
}

Element* Integers::bit_not(LispE* l) {
    if (!status) {
        for (long i = 0; i < size(); i++) {
            liste.vecteur[i] = ~liste.vecteur[i];
        }
        return this;
    }
    Integers* num = (Integers*)newInstance();
    for (long i = 0; i < size(); i++)
        num->liste.push_back(~liste.vecteur[i]);
    return num;
}


Element* Integers::bit_and(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d &= liste.vecteur[i];
        }
        return lisp->provideInteger(d);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] &= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] &= e->asInteger();
    }
    return this;
}

Element* Integers::bit_and_not(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d &= ~liste.vecteur[i];
        }
        return lisp->provideInteger(d);
    }

    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_and_not(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] &= ~e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] &= ~e->asInteger();
    }
    return this;
}


Element* Integers::bit_or(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d |= liste.vecteur[i];
        }
        return lisp->provideInteger(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_or(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] |= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] |= e->asInteger();
    }
    return this;
}

Element* Integers::bit_xor(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d ^= liste.vecteur[i];
        }
        return lisp->provideInteger(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->bit_xor(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] ^= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] ^= e->asInteger();
    }
    return this;
}

Element* Integers::plus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] += ((Numbers*)e)->liste.vecteur[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] += ((Floats*)e)->liste.vecteur[i];
            }
            return this;
        }
        case t_integers: {
            Integers* n = (Integers*)e;
            long szl = liste.size();
            long i = n->liste.size();
            
#ifdef INTELINTRINSICS
            if (szl >= 3 && i >= 3) {
                //if a number can be divided by 4, then the last two bits should be 0
                //we then compute how many elements are missing to make a vector with a size divisible by 4
                szl = lmin(szl, i);
                liste.padding((4 - (szl & 3)) & 3);
                n->liste.padding((4 - (szl & 3)) & 3);
                for (i = 0; i < szl; i+= 4) {
                    _mm256_storeu_si256( (__m256i *)&liste.vecteur[i],
                                        _mm256_add_epi64(
                                                         _mm256_loadu_si256((const __m256i *)&liste.vecteur[i]),
                                                         _mm256_loadu_si256((const __m256i *)&n->liste.vecteur[i])
                                                         )
                                        );
                }
                return this;
            }
#endif
            szl = lmin(szl, i);
            for (i = 0; i < szl; i++) {
                liste.vecteur[i] -= n->liste.vecteur[i];
            }
            return this;
        }
        case t_number:
        case t_float:
        case t_integer: {
            long szl = liste.size();
            long v = e->asInteger();
#ifdef INTELINTRINSICS
            if (szl >= 3) {
                liste.padding((4 - (szl & 3)) & 3);
                __m256i vb = {v, v, v, v};
                for (long i = 0; i < szl; i+= 4) {
                    _mm256_storeu_si256( (__m256i *)&liste.vecteur[i],
                                        _mm256_add_epi64(
                                                         _mm256_loadu_si256((const __m256i *)&liste.vecteur[i]),
                                                         vb
                                                         )
                                        );
                }
                return this;
            }
#endif
            for (long i = 0; i < szl; i++) {
                liste.vecteur[i] += v;
            }
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Floats*)result->index(m))->liste.vecteur[i] += liste.vecteur[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Numbers*)result->index(m))->liste.vecteur[i] += liste.vecteur[i];
                }
            }
            release();
            return result;
        }
        default:
            return plus(lisp, e);
    }
}


Element* Integers::minus_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] -= ((Numbers*)e)->liste.vecteur[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] -= ((Floats*)e)->liste.vecteur[i];
            }
            return this;
        }
        case t_integers: {
            Integers* n = (Integers*)e;
            long szl = liste.size();
            long i = n->liste.size();
            
#ifdef INTELINTRINSICS
            if (szl >= 3 && i >= 3) {
                //if a number can be divided by 4, then the last two bits should be 0
                //we then compute how many elements are missing to make a vector with a size divisible by 4
                szl = lmin(szl, i);
                liste.padding((4 - (szl & 3)) & 3);
                n->liste.padding((4 - (szl & 3)) & 3);
                for (i = 0; i < szl; i+= 4) {
                    _mm256_storeu_si256( (__m256i *)&liste.vecteur[i],
                                        _mm256_sub_epi64(
                                                         _mm256_loadu_si256((const __m256i *)&liste.vecteur[i]),
                                                         _mm256_loadu_si256((const __m256i *)&n->liste.vecteur[i])
                                                         )
                                        );
                }
                return this;
            }
#endif
            szl = lmin(szl, i);
            for (i = 0; i < szl; i++) {
                liste.vecteur[i] -= n->liste.vecteur[i];
            }
            return this;
        }
        case t_number:
        case t_float:
        case t_integer: {
            long szl = liste.size();
            long v = e->asInteger();
#ifdef INTELINTRINSICS
            if (szl >= 3) {
                liste.padding((4 - (szl & 3)) & 3);
                __m256i vb = {v, v, v, v};
                for (long i = 0; i < szl; i+= 4) {
                    _mm256_storeu_si256( (__m256i *)&liste.vecteur[i],
                                        _mm256_sub_epi64(
                                                         _mm256_loadu_si256((const __m256i *)&liste.vecteur[i]),
                                                         vb
                                                         )
                                        );
                }
                return this;
            }
#endif
            for (long i = 0; i < szl; i++) {
                liste.vecteur[i] -= v;
            }
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            float v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Floats*)result->index(m))->liste.vecteur[i];
                    ((Floats*)result->index(m))->liste.vecteur[i] = liste.vecteur[i] - v;
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            double v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Numbers*)result->index(m))->liste.vecteur[i];
                    ((Numbers*)result->index(m))->liste.vecteur[i] = liste.vecteur[i] - v;
                }
            }
            release();
            return result;
        }
        default:
            return minus(lisp, e);
    }
}

Element* Integers::multiply_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] *= ((Numbers*)e)->liste.vecteur[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] *= ((Floats*)e)->liste.vecteur[i];
            }
            return this;
        }
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                liste.vecteur[i] *= ((Integers*)e)->liste.vecteur[i];
            }
            return this;
        case t_number:
        case t_float:
        case t_integer: {
            long v = e->asInteger();
            for (long i = 0; i < liste.size(); i++) {
                liste.vecteur[i] *= v;
            }
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Floats*)result->index(m))->liste.vecteur[i] *= liste.vecteur[i];
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    ((Numbers*)result->index(m))->liste.vecteur[i] *= liste.vecteur[i];
                }
            }
            release();
            return result;
        }
        default:
            return multiply(lisp, e);
    }
}

Element* Integers::divide_direct(LispE* lisp, Element* e) {
    switch (e->type) {
        case t_numbers: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Numbers*)e)->liste.vecteur[i])
                    throw new Error("Error: division by zero");
                liste.vecteur[i] /= ((Numbers*)e)->liste.vecteur[i];
            }
            return this;
        }
        case t_floats: {
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Floats*)e)->liste.vecteur[i])
                    throw new Error("Error: division by zero");
                liste.vecteur[i] /= ((Floats*)e)->liste.vecteur[i];
            }
            return this;
        }
        case t_integers:
            for (long i = 0; i < liste.size() && i < e->size(); i++) {
                if (!((Integers*)e)->liste.vecteur[i])
                    throw new Error("Error: division by zero");
                liste.vecteur[i] /= ((Integers*)e)->liste.vecteur[i];
            }
            return this;
        case t_number:
        case t_integer: {
            long v = e->asInteger();
            if (!v)
                throw new Error("Error: division by zero");
            for (long i = 0; i < liste.size(); i++) {
                liste.vecteur[i] /= v;
            }
            return this;
        }
        case t_matrix_float: {
            Matrice_float* result = new Matrice_float(lisp, (Matrice_float*)e);
            float v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Floats*)result->index(m))->liste.vecteur[i];
                    if (!v)
                        throw new Error("Error: division by zero");
                    ((Floats*)result->index(m))->liste.vecteur[i] = liste.vecteur[i] / v;
                }
            }
            release();
            return result;
        }
        case t_matrix: {
            Matrice* result = new Matrice(lisp, (Matrice*)e);
            double v;
            for (long m = 0; m < result->size_x; m++) {
                for (long i = 0; i < liste.size() && i < result->size_y; i++) {
                    v = ((Numbers*)result->index(m))->liste.vecteur[i];
                    if (!v)
                        throw new Error("Error: division by zero");
                    ((Numbers*)result->index(m))->liste.vecteur[i] = liste.vecteur[i] / v;
                }
            }
            release();
            return result;
        }
        default:
            return divide(lisp, e);
    }
}

Element* Integers::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d += liste.vecteur[i];
        }
        return lisp->provideInteger(d);
    }
    
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->plus(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] += e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] += e->asInteger();
    }
    return this;
}

Element* Integers::minus(LispE* lisp, Element* e) {
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d -= liste.vecteur[i];
        }
        return lisp->provideInteger(d);
    }
    
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->minus(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] -= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] -= e->asInteger();
    }
    return this;
}

Element* Integers::multiply(LispE* lisp, Element* e) {
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d *= liste.vecteur[i];
        }
        return lisp->provideInteger(d);
    }
    
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->multiply(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] *= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] *= e->asInteger();
    }
    return this;
}

Element* Integers::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (!liste.vecteur[i])
                throw new Error("Error: division by zero");
            d /= liste.vecteur[i];
        }
        return lisp->provideInteger(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->divide(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->divide(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->divide(lisp, e));
    }
    return this;
}

Element* Integers::mod(LispE* lisp, Element* e) {
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            if (!liste.vecteur[i])
                throw new Error("Error: division by zero");
            d %= liste.vecteur[i];
        }
        return lisp->provideInteger(d);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->mod(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->mod(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->mod(lisp, e));
    }
    return this;
}

Element* Integers::power(LispE* lisp, Element* e) {
    if (e == NULL) {
        double d = liste[0];
        for (long i = 1; i < size(); i++) {
            d = pow(d, (double)liste.vecteur[i]);
        }
        return lisp->provideInteger(d);
    }
    //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->power(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            replacing(i, index(i)->power(lisp, e->index(i)));
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        replacing(i, index(i)->power(lisp, e));
    }
    return this;
}

Element* Integers::leftshift(LispE* lisp, Element* e) {
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d <<= liste.vecteur[i];
        }
        return lisp->provideInteger(d);
    }
   //Two cases either e is a number or it is a list...
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->leftshift(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
       for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] <<= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] <<= e->asInteger();
    }
    return this;
}

Element* Integers::rightshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        long d = liste[0];
        for (long i = 1; i < size(); i++) {
            d >>= liste.vecteur[i];
        }
        return lisp->provideInteger(d);
    }
    if (e->isList()) {
        if (e->size() && e->index(0)->isList()) {
            Element* result = lisp->provideList();
            try {
                for (long i = 0; i < e->size() && i < size(); i++) {
                    result->append(index(i)->rightshift(lisp, e->index(i)));
                }
            }
            catch(Error* err) {
                result->release();
                throw err;
            }
            release();
            return result;
        }
        for (long i = 0; i < e->size() && i < size(); i++) {
            liste.vecteur[i] >>= e->index(i)->asInteger();
        }
        return this;
    }
    for (long i = 0; i < size(); i++) {
        liste.vecteur[i] >>= e->asInteger();
    }
    return this;
}

Element* Set_n::plus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double d = 0;
    if (e == NULL) {
        for (auto& a: ensemble) {
            d += a;
        }
        return lisp->provideNumber(d);
    }

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d = a + *nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            d = a + e->index(i)->asNumber();
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    double w = e->asNumber();
    for (auto& a: ensemble) {
        d = a + w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_n::minus(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double d = 0;
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else
                d -= a;
        }
        return lisp->provideNumber(d);
    }

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d = a - *nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            d = a - e->index(i)->asNumber();
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    double w = e->asNumber();
    for (auto& a: ensemble) {
        d = a - w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_n::multiply(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double d = 1;
    if (e == NULL) {
        for (auto& a: ensemble) {
            d *= a;
        }
        return lisp->provideNumber(d);
    }

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d = a * *nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            d = a * e->index(i)->asNumber();
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    double w = e->asNumber();
    for (auto& a: ensemble) {
        d = a * w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_n::divide(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double d = 0;
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else {
                if (!a)
                    throw new Error("Error: division by zero");
                d /= a;
            }
        }
        return lisp->provideNumber(d);
    }

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            if (!*nxt) {
                delete res;
                throw new Error("Error: division by zero");
            }

            d = a / *nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    double w;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            w = e->index(i)->asNumber();
            if (!w) {
                delete res;
                throw new Error("Error: division by zero");
            }
            d = a / w ;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    w = e->asNumber();
    if (!w) {
        delete res;
        throw new Error("Error: division by zero");
    }

    for (auto& a: ensemble) {
        d = a / w;
        res->add(d);
    }
    release();
    return res;
}


Element* Set_n::mod(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    if (e == NULL) {
        bool first = true;
        long d = 0;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else {
                if (!a)
                    throw new Error("Error: division by zero");
                d %= (long)a;
            }
        }
        return lisp->provideNumber(d);
    }

    double d;

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            if (!*nxt) {
                delete res;
                throw new Error("Error: division by zero");
            }

            d = (long)a % (long)*nxt;
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    long w;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            w = e->index(i)->asInteger();
            if (!w) {
                delete res;
                throw new Error("Error: division by zero");
            }
            d = (long)a % w ;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    w = e->asInteger();
    if (!w) {
        delete res;
        throw new Error("Error: division by zero");
    }

    for (auto& a: ensemble) {
        d = (long)a % w;
        res->add(d);
    }
    release();
    return res;
}

Element* Set_n::power(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    //Two cases either e is a number or it is a list...
    double d = 1;
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d = a;
                first = false;
            }
            else
                d = pow(d, a);
        }
        return lisp->provideNumber(d);
    }

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d = pow(a,*nxt);
            res->add(d);
            nxt++;
        }
        release();
        return res;
    }
    double w;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            w = e->index(i)->asNumber();
            d = pow(a, w) ;
            res->add(d);
            i++;
        }
        release();
        return res;
    }
    w = e->asNumber();
    if (!w) {
        delete res;
        throw new Error("Error: division by zero");
    }

    for (auto& a: ensemble) {
        d = pow(a, w);
        res->add(d);
    }
    release();
    return res;
}

Element* Set_n::bit_and_not(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    double64 r(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else {
                r.v = a;
                d.bits &= ~r.bits;
            }
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            r.v = *nxt;
            d.bits &= ~r.bits;
            res->add(d.v);
            nxt++;
        }
        release();
        return res;
    }

    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            r.v = e->index(i)->asNumber();
            d.v = a;
            d.bits &= ~r.bits;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    r.v = e->asNumber();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits &= ~r.bits;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::bit_and(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    double64 r(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else {
                r.v = a;
                d.bits &= r.bits;
            }
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            r.v = *nxt;
            d.bits &= r.bits;
            res->add(d.v);
            nxt++;
        }
        release();
        return res;
    }

    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            r.v = e->index(i)->asNumber();
            d.v = a;
            d.bits &= r.bits;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    r.v = e->asNumber();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits &= r.bits;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::bit_or(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    double64 r(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else {
                r.v = a;
                d.bits |= r.bits;
            }
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            r.v = *nxt;
            d.bits |= r.bits;
            res->add(d.v);
            nxt++;
        }
        release();
        return res;
    }

    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            r.v = e->index(i)->asNumber();
            d.v = a;
            d.bits |= r.bits;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    r.v = e->asNumber();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits |= r.bits;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::bit_xor(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    double64 r(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else {
                r.v = a;
                d.bits ^= r.bits;
            }
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            r.v = *nxt;
            d.bits ^= r.bits;
            res->add(d.v);
            nxt++;
        }
        release();
        return res;
    }

    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            r.v = e->index(i)->asNumber();
            d.v = a;
            d.bits ^= r.bits;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    r.v = e->asNumber();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits ^= r.bits;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::bit_not(LispE* lisp) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    Set_n* res = lisp->provideSet_n();

    for (auto& a: ensemble) {
        d.v = a;
        d.bits = ~d.bits;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::leftshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else
                d.bits <<= (long)a;
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            d.bits <<= (long)*nxt;
            res->add(d.v);
            nxt++;
        }
        release();
        return res;
    }
    long w;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            w = e->index(i)->asInteger();
            d.v = a;
            d.bits <<= w;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    w = e->asInteger();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits <<= w;
        res->add(d.v);
    }
    release();
    return res;
}

Element* Set_n::rightshift(LispE* lisp, Element* e) {
    //Two cases either e is a number or it is a list...
    double64 d(0);
    if (e == NULL) {
        bool first = true;
        for (auto& a: ensemble) {
            if (first) {
                d.v = a;
                first = false;
            }
            else
                d.bits >>= (long)a;
        }
        return lisp->provideNumber(d.v);
    }

    Set_n* res = lisp->provideSet_n();
    if (e->type == t_setn) {
        auto nxt = ((Set_n*)e)->ensemble.begin();
        for (auto& a : ensemble) {
            if (nxt == ((Set_n*)e)->ensemble.end()) {
                release();
                return res;
            }
            d.v = a;
            d.bits >>= (long)*nxt;
            res->add(d.v);
            nxt++;
        }
        release();
        return res;
    }
    long w;
    if (e->isList()) {
        long i = 0;
        for (auto& a : ensemble) {
            if (i == e->size()) {
                release();
                return res;
            }
            w = e->index(i)->asInteger();
            d.v = a;
            d.bits >>= w;
            res->add(d.v);
            i++;
        }
        release();
        return res;
    }
    w = e->asInteger();
    for (auto& a: ensemble) {
        d.v = a;
        d.bits >>= w;
        res->add(d.v);
    }
    release();
    return res;
}

//---------------------------------------------------------------------------
//------ Arithmetic Operations
//---------------------------------------------------------------------------


Element* List::evall_bitand(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->bit_and(lisp, NULL);
                first_element->release();
                return lst;
            }
            listsize = lst->size();
            if (listsize == 0) {
                lst->release();
                return zero_;
            }
            first_element = lst->index(0)->copyatom(1);
            for (i = 1; i < listsize; i++) {
                first_element = first_element->bit_and(lisp, lst->index(i));
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->bit_and(lisp, second_element);
                second_element->release();
                return first_element;
            }

            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_and(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_bitandnot(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->bit_and_not(lisp, NULL);
                first_element->release();
                return lst;
            }
            listsize = lst->size();
            if (listsize == 0) {
                lst->release();
                return zero_;
            }
            first_element = lst->index(0)->copyatom(1);
            for (i = 1; i < listsize; i++) {
                first_element = first_element->bit_and_not(lisp, lst->index(i));
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->bit_and_not(lisp, second_element);
                second_element->release();
                return first_element;
            }
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_and_not(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_bitor(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->bit_or(lisp, NULL);
                first_element->release();
                return lst;
            }
            listsize = lst->size();
            if (listsize == 0) {
                lst->release();
                return zero_;
            }
            first_element = lst->index(0)->copyatom(1);
            for (i = 1; i < listsize; i++) {
                first_element = first_element->bit_or(lisp, lst->index(i));
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->bit_or(lisp, second_element);
                second_element->release();
                return first_element;
            }
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_or(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }
    return first_element;
}

Element* List::evall_bitxor(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->bit_xor(lisp, NULL);
                first_element->release();
                return lst;
            }
            listsize = lst->size();
            if (listsize == 0) {
                lst->release();
                return zero_;
            }
            first_element = lst->index(0)->copyatom(1);
            for (i = 1; i < listsize; i++) {
                first_element = first_element->bit_xor(lisp, lst->index(i));
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->bit_xor(lisp, second_element);
                second_element->release();
                return first_element;
            }
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_xor(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }
    return first_element;
}

Element* List::evall_divide(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->divide(lisp, NULL);
                first_element->release();
                return lst;
            }
            listsize = lst->size();
            if (listsize == 0) {
                lst->release();
                return zero_;
            }
            first_element = lst->index(0)->copyatom(1);
            for (i = 1; i < listsize; i++) {
                first_element = first_element->divide_direct(lisp, lst->index(i));
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->divide_direct(lisp, second_element);
                second_element->release();
                return first_element;
            }
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->divide_direct(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }
    return first_element;
}

Element* List::evall_minus(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 ) {
            if (first_element->isList()) {
                lst = first_element;
                if (lst->type >= t_numbers && lst->type <= t_strings) {
                    lst = lst->minus(lisp, NULL);
                    first_element->release();
                    return lst;
                }
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    return zero_;
                }
                first_element = lst->index(0)->copyatom(1);
                for (i = 1; i < listsize; i++) {
                    first_element = first_element->minus_direct(lisp, lst->index(i));
                }
                lst->release();
            }
            else {
                second_element = first_element->invert_sign(lisp);
                if (first_element != second_element)
                    first_element->release();
                return second_element;
            }
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->minus_direct(lisp, second_element);
                second_element->release();
                return first_element;
            }
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->minus_direct(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_mod(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->mod(lisp, NULL);
                first_element->release();
                return lst;
            }
            listsize = lst->size();
            if (listsize == 0) {
                lst->release();
                return zero_;
            }
            first_element = lst->index(0)->copyatom(1);
            for (i = 1; i < listsize; i++) {
                first_element = first_element->mod(lisp, lst->index(i));
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->mod(lisp, second_element);
                second_element->release();
                return first_element;
            }
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->mod(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_multiply(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->multiply(lisp, NULL);
                first_element->release();
                return lst;
            }
            listsize = lst->size();
            if (listsize == 0) {
                lst->release();
                return zero_;
            }
            first_element = lst->index(0)->copyatom(1);
            for (i = 1; i < listsize; i++) {
                first_element = first_element->multiply_direct(lisp, lst->index(i));
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->multiply_direct(lisp, second_element);
                second_element->release();
                return first_element;
            }

            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->multiply_direct(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_plus(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->plus(lisp, NULL);
                first_element->release();
                return lst;
            }
            listsize = lst->size();
            if (listsize == 0) {
                lst->release();
                return zero_;
            }
            first_element = lst->index(0)->copyatom(1);
            for (i = 1; i < listsize; i++) {
                first_element = first_element->plus_direct(lisp, lst->index(i));
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->plus_direct(lisp, second_element);
                second_element->release();
                return first_element;
            }
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->plus_direct(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}


Element* List::evall_leftshift(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->leftshift(lisp, NULL);
                first_element->release();
                return lst;
            }
            listsize = lst->size();
            if (listsize == 0) {
                lst->release();
                return zero_;
            }
            first_element = lst->index(0)->copyatom(1);
            for (i = 1; i < listsize; i++) {
                first_element = first_element->leftshift(lisp, lst->index(i));
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->leftshift(lisp, second_element);
                second_element->release();
                return first_element;
            }
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->leftshift(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_rightshift(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->rightshift(lisp, NULL);
                first_element->release();
                return lst;
            }
            listsize = lst->size();
            if (listsize == 0) {
                lst->release();
                return zero_;
            }
            first_element = lst->index(0)->copyatom(1);
            for (i = 1; i < listsize; i++) {
                first_element = first_element->rightshift(lisp, lst->index(i));
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->rightshift(lisp, second_element);
                second_element->release();
                return first_element;
            }
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->rightshift(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List::evall_power(LispE* lisp) {
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;
        
    try {
        first_element = first_element->eval(lisp);
        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->power(lisp, NULL);
                first_element->release();
                return lst;
            }
            listsize = lst->size();
            if (listsize == 0) {
                lst->release();
                return zero_;
            }
            first_element = lst->index(0)->copyatom(1);
            for (i = 1; i < listsize; i++) {
                first_element = first_element->power(lisp, lst->index(i));
            }
            lst->release();
        }
        else {
            first_element = first_element->copyatom(1);
            if (listsize == 3) {
                second_element = liste[2]->eval(lisp);
                first_element = first_element->power(lisp, second_element);
                second_element->release();
                return first_element;
            }
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->power(lisp, second_element);
            }
            second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        first_element->release();
        second_element->release();
        throw err;
    }

    return first_element;
}

Element* List_power2::eval(LispE* lisp) {
    Element* first_element = liste[0];

    try {
        first_element = liste[1]->eval(lisp)->copyatom(1);
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return first_element->multiply_direct(lisp, first_element);
}


//-------------------------------------------------------------------------------

Element* List::evall_bitandequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;


    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->bit_and(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->bit_and(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_and(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}

Element* List::evall_bitandnotequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;


    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->bit_and_not(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->bit_and_not(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_and_not(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}

Element* List::evall_bitorequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;


    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->bit_or(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->bit_or(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_or(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}



Element* List::evall_bitxorequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;


    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->bit_xor(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->bit_xor(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->bit_xor(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}

Element* List::evall_divideequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;


    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->divide(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->divide_direct(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->divide_direct(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}

Element* List::evall_leftshiftequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;


    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->leftshift(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->leftshift(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->leftshift(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}

Element* List::evall_minusequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;


    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->minus(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->minus_direct(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->minus_direct(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}

Element* List::evall_modequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;


    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->mod(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->mod(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->mod(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}


Element* List::evall_multiplyequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;


    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->multiply(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->multiply_direct(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->multiply_direct(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}

Element* List::evall_plusequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;

    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->plus(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->plus_direct(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->plus_direct(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}

Element* List::evall_rightshiftequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;

    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->rightshift(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->rightshift(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->rightshift(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}

Element* List::evall_powerequal(LispE* lisp) {
    short label = liste[1]->label();
    if (label < l_final)
        throw new Error("Error: Missing variable");
    
    short listsize = liste.size();
    Element* lst = this;
    Element* first_element = liste[1];
    Element* second_element = null_;
    long i;

    try {
        first_element = first_element->eval(lisp)->copyatom(s_constant);

        if (listsize == 2 && first_element->isList()) {
            lst = first_element;
            if (lst->type >= t_numbers && lst->type <= t_strings) {
                lst = lst->power(lisp, NULL);
                first_element->release();
                first_element = lst;
            }
            else {
                listsize = lst->size();
                if (listsize == 0) {
                    lst->release();
                    first_element = zero_;
                }
                else {
                    first_element = lst->index(0)->copyatom(1);
                    for (i = 1; i < listsize; i++) {
                        first_element = first_element->power(lisp, lst->index(i));
                    }
                    lst->release();
                }
            }
        }
        else {
            for (i = 2; i < listsize; i++) {
                if (first_element != second_element)
                    _releasing(second_element);
                second_element = liste[i]->eval(lisp);
                first_element = first_element->power(lisp, second_element);
            }
            if (first_element != second_element)
                second_element->release();
        }
    }
    catch (Error* err) {
        if (lst != this)
            lst->release();
        if (first_element != second_element)
            second_element->release();
        first_element->release();
        throw err;
    }

    return lisp->recording_variable(first_element, label);
}

Element* List::evall_powerequal2(LispE* lisp) {
    Element* first_element = liste[0];
    short label;

    try {
        first_element = liste[1]->eval(lisp)->copyatom(s_constant);
        first_element = first_element->multiply_direct(lisp, first_element);
        label = liste[1]->label();
        if (label > l_final)
            return lisp->recording_variable(first_element, label);
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }

    return first_element;
}

//------------------------------------------------------------------------------------------

Element* List::evall_sum(LispE* lisp) {
    Element* first_element = liste[1];
    
    try {
        first_element = first_element->eval(lisp);
        switch (first_element->type) {
            case t_floats: {
                float v = 0;
                Floats* nb = (Floats*)first_element;
                long listsize = nb->size();
                for (long i = 0; i < listsize; i++) {
                    v += nb->liste[i];
                }
                first_element->release();
                return lisp->provideFloat(v);
            }
            case t_numbers: {
                double v = 0;
                Numbers* nb = (Numbers*)first_element;
                long listsize = nb->size();
                for (long i = 0; i < listsize; i++) {
                    v += nb->liste[i];
                }
                first_element->release();
                return lisp->provideNumber(v);
            }
            case t_integers: {
                long v = 0;
                Integers* nb = (Integers*)first_element;
                long listsize = nb->size();
                for (long i = 0; i < listsize; i++) {
                    v += nb->liste[i];
                }
                first_element->release();
                return lisp->provideInteger(v);
            }
            case t_strings: {
                u_ustring w;
                Strings* nb = (Strings*)first_element;
                long listsize = nb->size();
                for (long i = 0; i < listsize; i++) {
                    w += nb->liste[i];
                }
                first_element->release();
                return lisp->provideString(w);
            }
            case t_list: {
                double v = 0;
                List* lst = (List*)first_element;
                long listsize = lst->size();
                for (long i = 0; i < listsize; i++) {
                    v += lst->liste[i]->checkNumber(lisp);
                }
                first_element->release();
                return lisp->provideNumber(v);
            }
            case t_setn: {
                double v = 0;
                Set_n* lst = (Set_n*)first_element;
                for (auto& a: lst->ensemble)
                    v += a;
                first_element->release();
                return lisp->provideNumber(v);
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }
    throw new Error("Error: expecting a list as argument");
}

Element* List::evall_product(LispE* lisp) {
    Element* first_element = liste[1];
    try {
        first_element = first_element->eval(lisp);
        switch (first_element->type) {
            case t_floats: {
                float v = 1;
                Floats* nb = (Floats*)first_element;
                long listsize = nb->size();
                for (long i = 0; i < listsize; i++) {
                    v *= nb->liste[i];
                }
                first_element->release();
                return lisp->provideFloat(v);
            }
            case t_numbers: {
                double v = 1;
                Numbers* nb = (Numbers*)first_element;
                long listsize = nb->size();
                for (long i = 0; i < listsize; i++) {
                    v *= nb->liste[i];
                }
                first_element->release();
                return lisp->provideNumber(v);
            }
            case t_integers: {
                double v = 1;
                Integers* nb = (Integers*)first_element;
                long listsize = nb->size();
                for (long i = 0; i < listsize; i++) {
                    v *= nb->liste[i];
                }
                first_element->release();
                return lisp->provideInteger(v);
            }
            case t_list: {
                double v = 1;
                List* lst = (List*)first_element;
                long listsize = lst->size();
                for (long i = 0; i < listsize; i++)
                    v *= lst->liste[i]->checkNumber(lisp);
                first_element->release();
                return lisp->provideNumber(v);
            }
            case t_setn: {
                double v = 1;
                Set_n* lst = (Set_n*)first_element;
                for (auto& a: lst->ensemble)
                    v *= a;
                first_element->release();
                return lisp->provideNumber(v);
            }
        }
    }
    catch (Error* err) {
        first_element->release();
        throw err;
    }
    
    throw new Error("Error: expecting a list as argument");
}

//------------------------------------------------------------------------------------------

Element* List::evall_invert(LispE* lisp) {
    Element* element = null_;
    Element* Y = null_;
    Element* res;

    try {
        element = liste[1]->eval(lisp);
        if (element->type != t_matrix)
            throw new Error("Error: 'invert' can only be applied to matrices");

        if (liste.size() == 3) {
            Y = liste[2]->eval(lisp);
            if (Y->type != t_matrix)
                throw new Error("Error: 'solve' can only be applied to matrices");
            res = ((Matrice*)element)->solve(lisp, (Matrice*)Y);
            Y->release();
        }
        else
            res = ((Matrice*)element)->inversion(lisp);
        
        element->release();
    }
    catch (Error* err) {
        element->release();
        Y->release();
        throw err;
    }
    return res;
}

Element* List::evall_solve(LispE* lisp) {
    Element* element = null_;
    Element* Y = null_;
    Element* res;

    try {
        element = liste[1]->eval(lisp);
        Y = liste[2]->eval(lisp);
        if (element->type != t_matrix || Y->type != t_matrix)
            throw new Error("Error: solve can only be applied to matrices");
        res = ((Matrice*)element)->solve(lisp, (Matrice*)Y);
        Y->release();
        element->release();
    }
    catch (Error* err) {
        element->release();
        Y->release();
        throw err;
    }
    return res;
}


Element* List::evall_determinant(LispE* lisp) {
    Element* element = null_;
    double det = 0;
    
    try {
        element = liste[1]->eval(lisp);
        if (element->type != t_matrix)
            throw new Error("Error: We can only compute the determinant of a matrix");
        det = ((Matrice*)element)->determinant();
        element->release();
    }
    catch (Error* err) {
        element->release();
        throw err;
    }
    return lisp->provideNumber(det);
}

Element* List::evall_ludcmp(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    if (element->type != t_matrix) {
        element->release();
        throw new Error("Error: the first element should be a matrix");
    }
    Element* res = ((Matrice*)element)->ludcmp(lisp);
    element->release();
    return res;
}

Element* List::evall_lubksb(LispE* lisp) {
    Element* element = null_;
    Element* idxs = null_;
    Element* Y = NULL;
    
    try {
        element = liste[1]->eval(lisp);
        if (element->type != t_matrix)
            throw new Error("Error: the first element should be a matrix");
        idxs = liste[2]->eval(lisp);
        if (idxs->type != t_integers)
            throw new Error("Error: the second element should be an integers_ (a list of integers)");
        if (liste.size() == 4) {
            Y = liste[3]->eval(lisp);
            if (Y->type != t_matrix)
                throw new Error("Error: the last element should be a matrix");
        }
        Y = ((Matrice*)element)->lubksb(lisp, (Integers*)idxs, (Matrice*)Y);
        element->release();
        idxs->release();
    }
    catch (Error* err) {
        element->release();
        idxs->release();
        if (Y != NULL) {
            Y->release();
        }
        throw err;
    }
    return Y;
}

