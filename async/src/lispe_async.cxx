/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_async.cxx


/*
 This the template for new 'extensions' in lispe
 */

#include "lispe.h"
#include "listes.h"
#include <future>

#ifdef WIN32
#if (_MSC_VER >= 1900)
#pragma comment(lib, "legacy_stdio_definitions.lib")
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif
#endif

static int16_t type_async_run;
static int16_t type_async;
static int16_t type_async_element;

class LispE_async_element : public Element {
public:
    LispE local_lisp;
    Element* code;
    List* call;
    u_ustring name;

    LispE_async_element(LispE* lisp, u_ustring& n, Element* c) : Element(type_async_element), local_lisp(lisp) {
        name = U"(async '" + n + U")";
        code = c;
        code->increment();
        Element* function = code->index(0);
        bool use_list = !lisp->delegation->check_straight(function->label());
        if (use_list)
            call = lisp->provideList();
        else
            call = lisp->delegation->straight_eval[function->label()]->cloning();
        call->append(function);
        for (long i = 1; i < code->size(); i++)
            call->append(null_);
        call->increment();
    }

    ~LispE_async_element() {
        call->force_release();
        code->decrement();
    }
    
    u_ustring asUString(LispE* lisp) {
        return name;
    }

    void initialize(LispE* lisp) {
        Element* e = NULL;
        try {
            for (long i = 1; i < code->size(); i++) {
                e = code->index(i)->eval(lisp);
                call->set_in(lisp, new Quoted(e->fullcopy()), i);
                e->release();
            }
        }
        catch(Error* err) {
            for (long i = 1; i < call->size(); i++) {
                call->set_in(lisp, null_, i);
            }
            throw err;
        }
    }

    void clean(LispE* lisp) {
        for (long i = 1; i < call->size(); i++) {
            call->set_in(lisp, null_, i);
        }
    }

    Element* eval(LispE* lisp) {        
        Element* e = null_;
        try {
            e = call->eval(&local_lisp);
        }
        catch(Error* err) {
            return err;
        }

        local_lisp.create_no_pool_element = true;
        Element* v = e->fullcopy();
        local_lisp.create_no_pool_element = false;
        e->release();
        return v;
    }

};

class LispE_async : public Element {
public:
    

    LispE_async() : Element(type_async) {}

    Element* eval(LispE* lisp) {
        //(async name (call a1 a2 a3)
        Element* code = lisp->get_variable("call");
        if (!code->size())
            throw new Error("Error: expecting a list with at least one element");
        u_ustring name  = lisp->get_variable("name")->asUString(lisp);
        return new LispE_async_element(lisp, name, code);
    }

    u_ustring asUString() {
        return U"Returns an 'async' element";
    }

};

Element* evaluation(LispE* lisp, Element* code) {    
    return code->eval(lisp);
}

class LispE_async_run : public Element {
public:

    LispE_async_run() : Element(type_async_run) {}

    Element* eval(LispE* lisp) {
        Element* codes = lisp->get_variable("calls");
        if (!codes->isList()) {
            if (codes->type == type_async_element)
                return codes->eval(lisp);
            throw new Error("Error: Expecting a list");
        }
        vector<std::future<Element*> > futurs;
        Element* e = NULL;
        try {
            for (long i = 0; i < codes->size(); i++) {
                e = codes->index(i);
                if (e->type != type_async_element)
                    throw new Error("Only 'async' elements can be evaluated here");
                
                ((LispE_async_element*)e)->initialize(lisp);
                futurs.push_back(std::async(std::launch::async, evaluation, lisp, e));
            }
        }
        catch(Error* err) {
            for (long i = 0; i < codes->size(); i++) {
                e = codes->index(i);
                if (e->type != type_async_element)
                    break;
                ((LispE_async_element*)e)->clean(lisp);
            }
            throw err;
        }
        
        List* l = lisp->provideList();
        for (auto& e : futurs) {
            l->append(e.get());
        }

        for (long i = 0; i < codes->size(); i++) {
            ((LispE_async_element*)e)->clean(lisp);
        }
        return l;
    }

    u_ustring asUString() {
        return U"Execute an 'async' element or a list of 'async' elements";
    }

};

extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {

    type_async_run = lisp->provideAtom(U"l_async_run_")->label();
    type_async = lisp->provideAtom(U"l_async_")->label();
    type_async_element = lisp->provideAtom(U"t_async_element_")->label();

    //We first create the body of the function
    lisp->extension("deflib async(name call)", new LispE_async());
    lisp->extension("deflib async_run(calls)", new LispE_async_run());
    return true;
}
}


