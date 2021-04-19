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


static const double M_GOLDEN = 1.61803398874989484820458683436563811772030917980576286213544862270526046281890244970720720418939113748475;

//This math library is done in two steps

typedef enum {math_fabs,math_acos,math_acosh,math_asin,math_asinh,math_atan,math_atanh,math_cbrt,math_cos,math_cosh,math_erf,math_erfc,math_exp,math_exp2,math_expm1,math_floor,math_lgamma,math_log,math_log10,math_log1p,math_log2,math_logb,math_nearbyint,math_rint,math_round,math_sin,math_sinh,math_sqrt,math_tan,math_tanh,math_tgamma,math_trunc, math_radian, math_degree} math;

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
        wstring val = L"val";
        v_val = lisp->encode(val);
    }
    
    Element* eval(LispE* lisp) {
        //eval is either: command, setenv or getenv...
        double v;
        switch (m) {
            case math_fabs: {
                v = lisp->get(v_val)->asNumber();
                v = fabs(v);
                return lisp->provideNumber(v);
            }
            case math_acos: {
                v = lisp->get(v_val)->asNumber();
                v = acos(v);
                return lisp->provideNumber(v);
            }
            case math_acosh: {
                v = lisp->get(v_val)->asNumber();
                v = acosh(v);
                return lisp->provideNumber(v);
            }
            case math_asin: {
                v = lisp->get(v_val)->asNumber();
                v = asin(v);
                return lisp->provideNumber(v);
            }
            case math_asinh: {
                v = lisp->get(v_val)->asNumber();
                v = asinh(v);
                return lisp->provideNumber(v);
            }
            case math_atan: {
                v = lisp->get(v_val)->asNumber();
                v = atan(v);
                return lisp->provideNumber(v);
            }
            case math_atanh: {
                v = lisp->get(v_val)->asNumber();
                v = atanh(v);
                return lisp->provideNumber(v);
            }
            case math_cbrt: {
                v = lisp->get(v_val)->asNumber();
                v = cbrt(v);
                return lisp->provideNumber(v);
            }
            case math_cos: {
                v = lisp->get(v_val)->asNumber();
                v = cos(v);
                return lisp->provideNumber(v);
            }
            case math_cosh: {
                v = lisp->get(v_val)->asNumber();
                v = cosh(v);
                return lisp->provideNumber(v);
            }
            case math_erf: {
                v = lisp->get(v_val)->asNumber();
                v = erf(v);
                return lisp->provideNumber(v);
            }
            case math_erfc: {
                v = lisp->get(v_val)->asNumber();
                v = erfc(v);
                return lisp->provideNumber(v);
            }
            case math_exp: {
                v = lisp->get(v_val)->asNumber();
                v = exp(v);
                return lisp->provideNumber(v);
            }
            case math_exp2: {
                v = lisp->get(v_val)->asNumber();
                v = exp2(v);
                return lisp->provideNumber(v);
            }
            case math_expm1: {
                v = lisp->get(v_val)->asNumber();
                v = expm1(v);
                return lisp->provideNumber(v);
            }
            case math_floor: {
                v = lisp->get(v_val)->asNumber();
                v = floor(v);
                return lisp->provideNumber(v);
            }
            case math_lgamma: {
                v = lisp->get(v_val)->asNumber();
                v = lgamma(v);
                return lisp->provideNumber(v);
            }
            case math_log: {
                v = lisp->get(v_val)->asNumber();
                v = log(v);
                return lisp->provideNumber(v);
            }
            case math_log10: {
                v = lisp->get(v_val)->asNumber();
                v = log10(v);
                return lisp->provideNumber(v);
            }
            case math_log1p: {
                v = lisp->get(v_val)->asNumber();
                v = log1p(v);
                return lisp->provideNumber(v);
            }
            case math_log2: {
                v = lisp->get(v_val)->asNumber();
                v = log2(v);
                return lisp->provideNumber(v);
            }
            case math_logb: {
                v = lisp->get(v_val)->asNumber();
                v = logb(v);
                return lisp->provideNumber(v);
            }
            case math_nearbyint: {
                v = lisp->get(v_val)->asNumber();
                v = nearbyint(v);
                return lisp->provideNumber(v);
            }
            case math_rint: {
                v = lisp->get(v_val)->asNumber();
                v = rint(v);
                return lisp->provideNumber(v);
            }
            case math_round: {
                v = lisp->get(v_val)->asNumber();
                v = round(v);
                return lisp->provideNumber(v);
            }
            case math_sin: {
                v = lisp->get(v_val)->asNumber();
                v = sin(v);
                return lisp->provideNumber(v);
            }
            case math_sinh: {
                v = lisp->get(v_val)->asNumber();
                v = sinh(v);
                return lisp->provideNumber(v);
            }
            case math_sqrt: {
                v = lisp->get(v_val)->asNumber();
                v = sqrt(v);
                return lisp->provideNumber(v);
            }
            case math_tan: {
                v = lisp->get(v_val)->asNumber();
                v = tan(v);
                return lisp->provideNumber(v);
            }
            case math_tanh: {
                v = lisp->get(v_val)->asNumber();
                v = tanh(v);
                return lisp->provideNumber(v);
            }
            case math_tgamma: {
                v = lisp->get(v_val)->asNumber();
                v = tgamma(v);
                return lisp->provideNumber(v);
            }
            case math_trunc: {
                v = lisp->get(v_val)->asNumber();
                v = trunc(v);
                return lisp->provideNumber(v);
            }
            case math_radian: {
                v = lisp->get(v_val)->asNumber();
                v = M_PI*(v / 180);
                return lisp->provideNumber(v);
            }
            case math_degree: {
                v = lisp->get(v_val)->asNumber();
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

    wstring nom = L"_pi";
    Element* pi = lisp->provideNumber(M_PI);
    lisp->recordingunique(pi, lisp->encode(nom));
    nom = L"π";
    lisp->recordingunique(pi, lisp->encode(nom));
    
    nom = L"_tau";
    pi = lisp->provideNumber(2 * M_PI);
    lisp->recordingunique(pi, lisp->encode(nom));
    nom = L"τ";
    lisp->recordingunique(pi, lisp->encode(nom));
    
    nom = L"_e";
    pi = lisp->provideNumber(M_E);
    lisp->recordingunique(pi, lisp->encode(nom));
    nom = L"ℯ";
    lisp->recordingunique(pi, lisp->encode(nom));
    
    nom = L"_phi";
    pi = lisp->provideNumber(M_GOLDEN);
    lisp->recordingunique(pi, lisp->encode(nom));
    nom = L"φ";
    lisp->recordingunique(pi, lisp->encode(nom));

}
//------ Matrix operations ------------------------

//LU decomposition
long LUDCMP(long n, VECTE<long>& indexes, long& d, Matrice& m) {    
    d = 1;
    double AMAX, DUM, thesum;
    long i, i_max = 0, j, k;
    vector<double> values;
    
    for (i = 0; i < n; i++)  {
        AMAX = 0.0;
        for (j = 0; j<n; j++)  {
            thesum = m.val(i, j);
            if (ABS(thesum) > AMAX)
                AMAX = ABS(thesum);
        }
        
        if (AMAX < TINY)
            return 1;
        values.push_back(1.0 / AMAX);
    } // i loop
    
    for (j = 0; j < n; j++)  {
        
        for (i = 0; i < j; i++)  {
            thesum = m.val(i, j);
            for (k = 0; k < i; k++)
            thesum = thesum - m.val(i, k)*m.val(k, j);
            m.set(i,j, thesum);
        } // i loop
        AMAX = 0.0;
        
        for (i = j; i < n; i++)  {
            thesum = m.val(i, j);
            for (k = 0; k < j; k++)
            thesum = thesum - m.val(i, k)*m.val(k, j);
            m.set(i,j, thesum);
            DUM = values[i] * ABS(thesum);
            if (DUM >= AMAX) {
                i_max = i;
                AMAX = DUM;
            }
        } // i loop
        
        if (j != i_max)  {
            for (k = 0; k < n; k++)  {
                DUM = m.val(i_max, k);
                m.set(i_max,k,m.val(j, k));
                m.set(j,k,DUM);
            } // k loop
            d = -d;
            values[i_max] = values[j];
        }
        
        indexes.at(j, i_max);
        
        if (ABS(m.val(j, j)) < TINY)
            m.set(j,j,TINY);
        
        if (j != n - 1)  {
            DUM = 1.0 / m.val(j, j);
            for (i = j + 1; i < n; i++) {
                m.mult(i,j, DUM);
            }
        }
    } // j loop
    
    return 0;
    
} // subroutine LUDCMP

void LUBKSB(long n, VECTE<long>& indexes, VECTE<double>& b_values, Matrice& m)  {
    double thesum;
    long  i, ii, j, ll;
    
    ii = -1;
    
    for (i = 0; i < n; i++)  {
        ll = indexes[i];
        thesum = b_values[ll];
        b_values.at(ll, b_values[i]);
        if (ii != -1) {
            for (j = ii; j < i; j++) {
                thesum = thesum - m.val(i, j)*b_values[j];
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
                thesum = thesum - m.val(i, j)*b_values[j];
            }
        }
        b_values.at(i, thesum / m.val(i, i));
    } // i loop
    
} // LUBKSB

double Matrice::determinant() {
    if (size_x == 2 && size_y == 2) {
        //then in that case
        return (val(0,0) * val(1,1) - val(1,0) * val(0,1));
    }


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
    //else Local decomposition
    Matrice m(this);
    
    
    VECTE<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, m);
    if (rc == 1) {
        return emptylist_;
    }
    
    
    Matrice* Y = new Matrice(size_x, size_y, 0.0);
    
    VECTE<double> temp(size_x);
    long i;
    //We create an identity matrix, which will contain the final result...
    for (i = 0; i < size_x; i++) {
        Y->set(i,i, 1);
    }
    
    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, m);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}

Element* Matrice::solve(LispE* lisp, Matrice* y) {
    //else Local decomposition
    Matrice m(this);
        
    VECTE<long> indexes(size_x);
    long id;
    //call LU decomposition
    long rc = LUDCMP(size_x, indexes, id, m);
    if (rc == 1) {
        return emptylist_;
    }
        
    Matrice* Y = new Matrice(y);
    VECTE<double> temp(size_x);
    long i;

    for (long j = 0; j < size_x; j++) {
        for (i = 0; i < size_x; i++) {
            temp.at(i, Y->val(i, j));
        }
        LUBKSB(size_x, indexes, temp, m);
        for (i = 0; i < size_x; i++) {
            Y->set(i,j,temp[i]);
        }
    }
    return Y;
}
