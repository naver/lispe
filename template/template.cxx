/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_%1.cxx


/*
 This the template for new 'extensions' in lispe
 */

#include "lispe.h"
#include "lispe_%1.h"

#ifdef WIN32
#if (_MSC_VER >= 1900)
#pragma comment(lib, "legacy_stdio_definitions.lib")
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif
#endif

Element* Lispe_%1::eval(LispE* lisp) {
    //The name defined in the extension is not insignificant, it is used to retrieve our arguments.
    Element* argument = lisp->get_variable((L"cmd");
    return argument;
}

//We use this instruction to return a description of the instruction
//Indeed, just do: (print %1_example) to get this information
wstring Lispe_%1::asString(LispE* lisp) {
    return L"This is an example";
}

u_ustring Lispe_%1::asUString(LispE* lisp) {
    return U"This is an example";
}

float Lispe_%1::asFloat() {
    return 0;
}
                                           
double Lispe_%1::asNumber() {
    return 0;
}

long Lispe_%1::asInteger() {
    return 0;
}

bool Lispe_%1::Boolean() {
    return true;
}

//The default version should return 'this'.
//We give the example below, to show
//chow are manage numbers and strings,
//but if your object is very big, the counter mechanism
// reference is more than sufficient. It is better to send back 'this'.
Element* Lispe_%1::copying(bool duplicate) {
    if (status == s_destructible || !duplicate)
        return this;
    return new Lispe_%1;
}

extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {
    //We first create the body of the function
    Element* body = lisp->extension("deflib %1_example (cmd)", new Lispe_%1());
    if (body->isError()) {
        std::cerr << body->toString(lisp) << endl;
        return false;
    }
    return true;
}
}

