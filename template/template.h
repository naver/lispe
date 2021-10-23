/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_%1.cxx
//

#ifndef lispe_%1_h
#define lispe_%1_h


class Lispe_%1 : public Element {
public:
    Lispe_%1() : Element(l_lib) {}

    Element* eval(LispE* lisp);

    //We use this instruction to return a description of the instruction
    //Indeed, just do: (print getenv) to get this information
    wstring asString(LispE* lisp);
    u_ustring asUString(LispE* lisp);

    double asNumber();
    long asInteger();
    float asFloat();
    bool Boolean();

    Element* copying(bool duplicate = true);
};

#endif

