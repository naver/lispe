/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  segmentation.h
//
//

#ifndef segmentation_h
#define segmentation_h


#include <ostream>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include "elements.h"

using std::string;
using std::vector;

#define uchar unsigned char

class Tokenizer {
public:
    vector<u_ustring> tokens;
    vector<double> numbers;
    vector<lisp_code> types;
    vector<long> lines;
    vector<long> positions;
    binHash<Element*> defun_functions;
    
    long current;
    bool asList;

    Tokenizer() {
        asList = false;
    }
    
    void clear() {
        asList = false;
        current = -1;
        tokens.clear();
        numbers.clear();
        types.clear();
        lines.clear();
        positions.clear();
    }
    
    void append(uchar car, lisp_code t, long l, long posbeg, long posend) {
        string token;
        token = car;
        u_ustring segment;
        s_utf8_to_unicode(segment, USTR(token), token.size());
        tokens.push_back(segment);
        numbers.push_back(0);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

    void append(string& token, lisp_code t, long l, long posbeg, long posend) {
        u_ustring segment;
        s_utf8_to_unicode(segment, USTR(token), token.size());
        tokens.push_back(segment);
        numbers.push_back(0);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

    void append(double valeur, string& token, lisp_code t, long l, long posbeg, long posend) {
        u_ustring segment;
        s_utf8_to_unicode(segment, USTR(token), token.size());
        tokens.push_back(segment);
        numbers.push_back(valeur);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

};


#endif /* segmentation_h */
