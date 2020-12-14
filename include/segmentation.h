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
    vector<string> tokens;
    vector<double> numbers;
    vector<lisp_code> types;
    vector<long> lines;
    vector<long> positions;

    void clear() {
        tokens.clear();
        numbers.clear();
        types.clear();
        lines.clear();
        positions.clear();
    }
    
    void append(uchar segment, lisp_code t, long l, long posbeg, long posend) {
        string str;
        str = segment;
        tokens.push_back(str);
        numbers.push_back(0);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

    void append(string segment, lisp_code t, long l, long posbeg, long posend) {
        tokens.push_back(segment);
        numbers.push_back(0);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

    void append(double valeur, string segment, lisp_code t, long l, long posbeg, long posend) {
        tokens.push_back(segment);
        numbers.push_back(valeur);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

};


#endif /* segmentation_h */
