/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  systeme.cxx
//
//


/*
 This is an example of how to extend LispE with new features.
 This system adds system features to the language
 */

#ifdef WIN32
#define NOMINMAX
#include "Windows.h"
#define _USE_MATH_DEFINES
#include <math.h>
#include <time.h>
#endif

#include "lispe.h"
#include "elements.h"
#include "tools.h"

#include <random>
#include <algorithm>

//This 'random' library is done in two steps

typedef enum {rnd_random, rnd_shuffle, rnd_random_choice,rnd_uniform,rnd_bernoulli_distribution,rnd_binomial_distribution,rnd_negative_binomial_distribution,rnd_geometric_distribution,rnd_poisson_distribution,rnd_exponential_distribution,rnd_gamma_distribution,rnd_weibull_distribution,rnd_extreme_value_distribution,rnd_normal_distribution,rnd_lognormal_distribution,rnd_chi_squared_distribution,rnd_cauchy_distribution,rnd_fisher_distribution,rnd_student_distribution,rnd_discrete_distribution,rnd_piecewise_constant_distribution,rnd_piecewise_linear_distribution} aleatoire;


/*
 First of all we create a new Element derivation
 The "eval" and "asString" methods are then implemented.
 This extension exports a number of methods to return random distributions
 */

#ifdef WIN32
long unaryfunc(double v) {
    return v;
}
#endif

double localrandom(long mx) {
    static bool lisperestrandom = true;
    
    if (!mx)
        mx = 1;
    static unsigned long x = 123456789;
    static unsigned long y = 362436069;
    static unsigned long z = 521288629;
    static long w = time(0);
    unsigned long t;
    if (lisperestrandom) {
        w = time(0);
        lisperestrandom = false;
    }
    
    t = x ^ (x << 11);
    x = y; y = z; z = w;
    w = w ^ (w >> 19) ^ (t ^ (t >> 8));
    return abs(w%mx);
}

class Aleatoire : public Element {
public:
    aleatoire rnd;
    int16_t v_nb;
    int16_t v_alpha;
    int16_t v_beta;
    int16_t v_liste;
    int16_t v_initial;
    int16_t v_inter;
    
    Aleatoire(LispE* lisp, aleatoire r) : rnd(r), Element(l_lib) {
        u_ustring nom(U"nb");
        v_nb = lisp->encode(nom);
        nom = U"alpha";
        v_alpha = lisp->encode(nom);
        nom = U"beta";
        v_beta = lisp->encode(nom);
        nom = U"liste";
        v_liste = lisp->encode(nom);
        nom = U"initial";
        v_initial = lisp->encode(nom);
        nom = U"inter";
        v_inter = lisp->encode(nom);

    }

    Element* Proc_random_choice(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        long initial = lisp->get_variable(v_initial)->asInteger();
        
        Element* valuevect = lisp->get_variable(v_liste);
        
        if (!valuevect->isList())
            throw new Error("Error: The second parameter must be a list");
        
        long i;
        size_t sz = valuevect->size();
        
        
#ifdef WIN32
        std::discrete_distribution<long> d(sz, 0, 100, unaryfunc);
#else
        double val = initial / sz;
        vector<double> vect;
        for (i = 0; i < sz; i++)
            vect.push_back(val);
        
        std::discrete_distribution<long> d(vect.begin(), vect.end());
#endif
        if (nb == 1) {
            i = d(gen);
            return valuevect->value_on_index(lisp, i);
        }
        
        long v;
        switch (valuevect->type) {
            case  t_shorts: {
                Shorts* liste = new Shorts();
                int16_t vlispe;
                for (i = 0; i < nb; i++) {
                    v = d(gen);
                    vlispe = ((Shorts*)valuevect)->liste[v];
                    liste->liste.push_back(vlispe);
                }
                return liste;
            }
            case t_floats: {
                Floats* liste = lisp->provideFloats();
                float vlispe;
                for (i = 0; i < nb; i++) {
                    v = d(gen);
                    vlispe = ((Floats*)valuevect)->liste[v];
                    liste->liste.push_back(vlispe);
                }
                return liste;
            }
            case  t_integers: {
                Integers* liste = lisp->provideIntegers();
                long vlispe;
                for (i = 0; i < nb; i++) {
                    v = d(gen);
                    vlispe = ((Integers*)valuevect)->liste[v];
                    liste->liste.push_back(vlispe);
                }
                return liste;
            }
            case t_numbers: {
                Numbers* liste = lisp->provideNumbers();
                double vlispe;
                for (i = 0; i < nb; i++) {
                    v = d(gen);
                    vlispe = ((Numbers*)valuevect)->liste[v];
                    liste->liste.push_back(vlispe);
                }
                return liste;
            }
            default: {
                List* liste = lisp->provideList();
                
                Element* vlispe;
                for (i = 0; i < nb; i++) {
                    v = d(gen);
                    vlispe = valuevect->value_on_index(lisp, v);
                    liste->append(vlispe);
                }
                return liste;
            }
        }
    }
    
    Element* Proc_uniform(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        double beta = lisp->get_variable(v_beta)->asNumber();
        
        
        std::uniform_real_distribution<double> d(alpha, beta);
        if (nb == 1)
            return lisp->provideNumber((double)d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    Element* Proc_bernoulli_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        
        std::bernoulli_distribution d(alpha);
        bool v;
        if (nb == 1) {
            v = d(gen);
            return booleans_[v];
        }
        if (booleans_[0] == zero_) {
            Integers* iv = lisp->provideIntegers();
            for (long i = 0; i < nb; i++) {
                iv->liste.push_back(d(gen));
            }
            return iv;
        }
        
        List* iv = lisp->provideList();
                
        for (long i = 0; i < nb; i++) {
            v = d(gen);
            iv->append(booleans_[v]);
        }
        return iv;
    }
    
    Element* Proc_binomial_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        long alpha = lisp->get_variable(v_alpha)->asInteger();
        double beta = lisp->get_variable(v_beta)->asNumber();
        
        std::binomial_distribution<long> dis(alpha, beta);
        if (nb == 1)
            return lisp->provideInteger((long)dis(gen));

        
        Integers* iv = lisp->provideIntegers();
        
        for (long i = 0; i < nb; i++) {
            iv->liste.push_back(dis(gen));
        }
        return iv;
    }
    
    Element* Proc_negative_binomial_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        
        long alpha = lisp->get_variable(v_alpha)->asInteger();
        double beta = lisp->get_variable(v_beta)->asNumber();
        
        std::negative_binomial_distribution<long> d(alpha, beta);
        if (nb == 1)
            return lisp->provideInteger(d(gen));
        
        Integers* iv = lisp->provideIntegers();
        
        for (long i = 0; i < nb; i++) {
            iv->liste.push_back(d(gen));
        }

        return iv;
    }
    
    Element* Proc_normal_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        double beta = lisp->get_variable(v_beta)->asNumber();
        
        std::normal_distribution<double> d(alpha, beta);
        if (nb == 1)
            return lisp->provideNumber(d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    Element* Proc_discrete_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        Element* tvect = lisp->get_variable(v_liste);
        
        vector<double> vect;
        
        long i;
        for (i = 0; i < tvect->size(); i++)
            vect.push_back(tvect->index(i)->asNumber());
        
        #ifdef WIN32
            std::discrete_distribution<long> d;
            d._Par._Pvec = vect;
            d._Par._Init();
        #else
            std::discrete_distribution<long> d(vect.begin(), vect.end());
        #endif
        
        if (nb == 1) {
            i = d(gen);
            return tvect->value_on_index(lisp,i);
        }
        Element* iv;
        switch(tvect->type) {
            case t_shorts:
                iv = new Shorts();
                break;
            case t_integers:
                iv = lisp->provideIntegers();
                break;
            case t_floats:
                iv = lisp->provideFloats();
                break;
            default:
                iv = lisp->provideNumbers();
        }
        
        long idx;
        for (long i = 0; i < nb; i++) {
            idx = d(gen);
            iv->append(tvect->index(idx));
        }
        return iv;
    }
    
    
    Element* Proc_piecewise_constant_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        Element* tvect = lisp->get_variable(v_liste);
        Element* tinter = lisp->get_variable(v_inter);
        
        vector<double> vect;
        vector<double> inter;
        
        long i;
        for (i = 0; i < tvect->size(); i++)
            vect.push_back(tvect->index(i)->asNumber());
        
        for (i = 0; i < tinter->size(); i++)
            vect.push_back(tinter->index(i)->asNumber());
        
        std::piecewise_constant_distribution<double> d(vect.begin(), vect.end(), inter.begin());
        if (nb == 1)
            return lisp->provideNumber(d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    Element* Proc_piecewise_linear_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        Element* tvect = lisp->get_variable(v_liste);
        Element* tinter = lisp->get_variable(v_inter);
        
        vector<double> vect;
        vector<double> inter;
        
        long i;
        for (i = 0; i < tvect->size(); i++)
            vect.push_back(tvect->index(i)->asNumber());
        
        for (i = 0; i < tinter->size(); i++)
            vect.push_back(tinter->index(i)->asNumber());
        
        std::piecewise_linear_distribution<double> d(vect.begin(), vect.end(), inter.begin());
       
        if (nb == 1)
            return lisp->provideNumber(d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    
    Element* Proc_lognormal_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        double beta = lisp->get_variable(v_beta)->asNumber();
        
        std::lognormal_distribution<double> d(alpha, beta);
        
        if (nb == 1)
            return lisp->provideNumber(d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    Element* Proc_geometric_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        
        std::geometric_distribution<long> d(alpha);
        if (nb == 1)
            return lisp->provideInteger(d(gen));
        
        Integers* iv = lisp->provideIntegers();
        
        for (long i = 0; i < nb; i++) {
            iv->liste.push_back(d(gen));
        }
        return iv;
    }
    
    Element* Proc_cauchy_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        double beta = lisp->get_variable(v_beta)->asNumber();
        
        std::cauchy_distribution<double> d(alpha, beta);
        
        if (nb == 1)
            return lisp->provideNumber(d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    Element* Proc_fisher_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        double beta = lisp->get_variable(v_beta)->asNumber();
        
        std::fisher_f_distribution<double> d(alpha, beta);
        if (nb == 1)
            return lisp->provideNumber(d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    Element* Proc_student_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        
        std::student_t_distribution<double> d(alpha);
        if (nb == 1)
            return lisp->provideNumber((double)d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    Element* Proc_extreme_value_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        double beta = lisp->get_variable(v_beta)->asNumber();
        
        std::extreme_value_distribution<double> d(alpha, beta);
        if (nb == 1)
            return lisp->provideNumber((double)d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    
    Element* Proc_poisson_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        
        
        std::poisson_distribution<long> d(alpha);
        if (nb == 1)
            return lisp->provideInteger(d(gen));
        
        Integers* iv = lisp->provideIntegers();
        
        for (long i = 0; i < nb; i++) {
            iv->liste.push_back(d(gen));
        }
        return iv;
    }
    
    Element* Proc_exponential_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        
        std::exponential_distribution<double> d(alpha);
        if (nb == 1)
            return lisp->provideNumber(d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    Element* Proc_gamma_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        double beta = lisp->get_variable(v_beta)->asNumber();
        
        std::gamma_distribution<double> d(alpha, beta);
        if (nb == 1)
            return lisp->provideNumber(d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    Element* Proc_weibull_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        double beta = lisp->get_variable(v_beta)->asNumber();
        
        std::weibull_distribution<double> d(alpha, beta);
        if (nb == 1)
            return lisp->provideNumber((double)d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    
    Element* Proc_chi_squared_distribution(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        long nb = lisp->get_variable(v_nb)->asInteger();
        double alpha = lisp->get_variable(v_alpha)->asNumber();
        
        std::chi_squared_distribution<double> d(alpha);
        if (nb == 1)
            return lisp->provideNumber((double)d(gen));
        
        Numbers* fv = lisp->provideNumbers();
        
        for (long i = 0; i < nb; i++) {
            fv->liste.push_back(d(gen));
        }
        return fv;
    }
    
    Element* Proc_shuffle(LispE* lisp) {
        static std::random_device rd;  //Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
        
        Element* l = lisp->get_variable(v_liste);
        if (!l->isList())
            throw new Error("Error: the 'shuffle' argument must be a list");
        
        switch (l->type) {
            case t_shorts: {
                Shorts* li = (Shorts*)l;
                long sz = li->size();
                for (long i = 0; i < sz; i++)
                    li->swap(i, (gen()%sz));
                return l;
            }
            case t_integers: {
                Integers* li = (Integers*)l;
                long sz = li->size();
                for (long i = 0; i < sz; i++)
                    li->swap(i, (gen()%sz));
                return l;
            }
            case t_floats: {
                Floats* li = (Floats*)l;
                long sz = li->size();
                for (long i = 0; i < sz; i++)
                    li->swap(i, (gen()%sz));
                return l;
            }
            case t_numbers: {
                Numbers* li = (Numbers*)l;
                long sz = li->size();
                for (long i = 0; i < sz; i++)
                    li->swap(i, (gen()%sz));
                return l;
            }
            default: {
                List* ll = (List*)l;
                
                long sz = ll->liste.size();
                long h = ll->liste.home;
                for (long i = h; i < ll->liste.item->last; i++) {
                    ll->liste.item->swap(i, (gen()%sz) + h);
                }
                return l;
            }
        }
    }
    
    Element* eval(LispE* lisp) {
        //eval is either: command, setenv or getenv...
        switch (rnd) {
            case rnd_random: {
                long nb = lisp->get_variable(v_nb)->asInteger();
                return lisp->provideInteger(localrandom(nb));
            }
            case rnd_shuffle:
                return Proc_shuffle(lisp);
            case rnd_random_choice:
                return Proc_random_choice(lisp);
            case rnd_discrete_distribution:
                return Proc_discrete_distribution(lisp);
            case rnd_piecewise_constant_distribution:
                return Proc_piecewise_constant_distribution(lisp);
            case rnd_piecewise_linear_distribution:
                return Proc_piecewise_linear_distribution(lisp);
            case rnd_lognormal_distribution:
                return Proc_lognormal_distribution(lisp);
            case rnd_geometric_distribution:
                return Proc_geometric_distribution(lisp);
            case rnd_cauchy_distribution:
                return Proc_cauchy_distribution(lisp);
            case rnd_fisher_distribution:
                return Proc_fisher_distribution(lisp);
            case rnd_student_distribution:
                return Proc_student_distribution(lisp);
            case rnd_extreme_value_distribution:
                return Proc_extreme_value_distribution(lisp);
            case rnd_poisson_distribution:
                return Proc_poisson_distribution(lisp);
            case rnd_exponential_distribution:
                return Proc_exponential_distribution(lisp);
            case rnd_gamma_distribution:
                return Proc_gamma_distribution(lisp);
            case rnd_weibull_distribution:
                return Proc_weibull_distribution(lisp);
            case rnd_chi_squared_distribution:
                return Proc_chi_squared_distribution(lisp);
            case rnd_uniform:
                return Proc_uniform(lisp);
            case rnd_bernoulli_distribution:
                return Proc_bernoulli_distribution(lisp);
            case rnd_binomial_distribution:
                return Proc_binomial_distribution(lisp);
            case rnd_negative_binomial_distribution:
                return Proc_negative_binomial_distribution(lisp);
            case rnd_normal_distribution:
                return Proc_normal_distribution(lisp);
        }
        return null_;
    }
    
    //We use this instruction to return a description of the instruction
    //in effect, just do: (print getenv) to get this information
    wstring asString(LispE* lisp) {
        switch (rnd) {
            case rnd_random:
                return L"returns a random value between 0 and nb";
            case rnd_shuffle:
                return L"Randomly mixes the values of a list";
            case rnd_random_choice:
                return L"returns a list of nb random values among the elements in 'list', and (initial/size list) > 1";
            case rnd_uniform:
                return L"Uniform discrete distribution (alpha = 0 beta = 1) ";
            case rnd_bernoulli_distribution:
                return L"Bernoulli distribution (alpha = 0.5)";
            case rnd_binomial_distribution:
                return L"Binomial distribution (alpha = 1 beta = 0.5)";
            case rnd_geometric_distribution:
                return L"Geometric distribution (alpha = 0.5)";
            case rnd_negative_binomial_distribution:
                return L"Negative binomial distribution (alpha = 1 beta = 0.5)";
            case rnd_poisson_distribution:
                return L"Poisson distribution (alpha = 1)";
            case rnd_exponential_distribution:
                return L"Exponential distribution";
            case rnd_gamma_distribution:
                return L"Gamma distribution";
            case rnd_weibull_distribution:
                return L"Weibull distribution (alpha = 1 beta = 1)";
            case rnd_extreme_value_distribution:
                return L"Extreme Value distribution (alpha = 0 beta = 1)";
            case rnd_normal_distribution:
                return L"Normal distribution (alpha = 0 beta = 1)";
            case rnd_lognormal_distribution:
                return L"Lognormal distribution (alpha = 0 beta = 1)";
            case rnd_chi_squared_distribution:
                return L"Chi-squared distribution ";
            case rnd_cauchy_distribution:
                return L"Cauchy distribution (alpha = 0 beta = 1)";
            case rnd_fisher_distribution:
                return L"Fisher F-distribution (alpha = 1 beta = 1)";
            case rnd_student_distribution:
                return L"Student T-Distribution (alpha = 1)";
            case rnd_discrete_distribution:
                return L"Discrete distribution";
            case rnd_piecewise_constant_distribution:
                return L"Piecewise constant distribution";
            case rnd_piecewise_linear_distribution:
                return L"Piecewise linear distribution";
        }
		return L"";
    }
};


//We are also going to implement the body of the call
void moduleAleatoire(LispE* lisp) {
    //We first create the body of the function
    //body is now a list to which we add an instance of our new class
    lisp->extension("deflib random (nb)", new Aleatoire(lisp, rnd_random));
    lisp->extension("deflib shuffle (liste)", new Aleatoire(lisp, rnd_shuffle));
    lisp->extension("deflib random_choice (nb liste initial)", new Aleatoire(lisp, rnd_random_choice));
    lisp->extension("deflib uniform_distribution (nb (alpha 0) (beta 1))", new Aleatoire(lisp, rnd_uniform));
    lisp->extension("deflib bernoulli_distribution (nb (alpha 0.5))", new Aleatoire(lisp, rnd_bernoulli_distribution));
    lisp->extension("deflib binomial_distribution (nb (alpha 1) (beta 0.5))", new Aleatoire(lisp, rnd_binomial_distribution));
    lisp->extension("deflib negative_binomial_distribution (nb (alpha 1) (beta 0.5))", new Aleatoire(lisp, rnd_negative_binomial_distribution));
    lisp->extension("deflib geometric_distribution (nb (alpha 0.5))", new Aleatoire(lisp, rnd_geometric_distribution));
    lisp->extension("deflib poisson_distribution (nb (alpha 1))", new Aleatoire(lisp, rnd_poisson_distribution));
    lisp->extension("deflib exponential_distribution (nb alpha)", new Aleatoire(lisp, rnd_exponential_distribution));
    lisp->extension("deflib gamma_distribution (nb alpha beta)", new Aleatoire(lisp, rnd_gamma_distribution));
    lisp->extension("deflib weibull_distribution (nb (alpha 1) (beta 1))", new Aleatoire(lisp, rnd_weibull_distribution));
    lisp->extension("deflib extreme_value_distribution (nb (alpha 0) (beta 1))", new Aleatoire(lisp, rnd_extreme_value_distribution));
    lisp->extension("deflib normal_distribution (nb (alpha 0) (beta 1))", new Aleatoire(lisp, rnd_normal_distribution));
    lisp->extension("deflib lognormal_distribution (nb (alpha 0) (beta 1))", new Aleatoire(lisp, rnd_lognormal_distribution));
    lisp->extension("deflib chi_squared_distribution (nb alpha)", new Aleatoire(lisp, rnd_chi_squared_distribution));
    lisp->extension("deflib cauchy_distribution (nb (alpha 0) (beta 1))", new Aleatoire(lisp, rnd_cauchy_distribution));
    lisp->extension("deflib fisher_distribution (nb (alpha 1) (beta 1))", new Aleatoire(lisp, rnd_fisher_distribution));
    lisp->extension("deflib student_distribution (nb (alpha 1))", new Aleatoire(lisp, rnd_student_distribution));
    lisp->extension("deflib discrete_distribution (nb liste)", new Aleatoire(lisp, rnd_discrete_distribution));
    lisp->extension("deflib piecewise_constant_distribution (nb liste inter)", new Aleatoire(lisp, rnd_piecewise_constant_distribution));
    lisp->extension("deflib piecewise_linear_distribution (nb liste inter)", new Aleatoire(lisp, rnd_piecewise_linear_distribution));
    
}
