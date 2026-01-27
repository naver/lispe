/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_tiktoken.cxx
//

#ifndef lispe_tiktoken_h
#define lispe_tiktoken_h

enum corebpe_action {corebpe_create, corebpe_decode, corebpe_encode, corebpe_special_encode, corebpe_vocab_size, corebpe_set_space, corebpe_add_meta, corebpe_remove_meta, corebpe_list_meta, corebpe_decode_bytes, corebpe_decode_single_token_bytes, corebpe_token_byte_values};

class Lispe_tiktoken : public Element {
public:
    corebpe_action action;

    Lispe_tiktoken(corebpe_action a) : Element(l_lib) {
        action = a;
    }

    Element* eval(LispE* lisp);
    Element* set_space(LispE* lisp, std::string);

    //We use this instruction to return a description of the instruction
    //Indeed, just do: (print getenv) to get this information
    wstring asString(LispE* lisp);
};

#endif


