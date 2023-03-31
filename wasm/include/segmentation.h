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
    vector<long> integers;
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
        integers.clear();
        types.clear();
        lines.clear();
        positions.clear();
    }

    
    void append(u_ustring& segment, lisp_code t, long l, long b, long e) {
        tokens.push_back(segment);
        numbers.push_back(0);
        integers.push_back(0);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(b);
        positions.push_back(e);
    }

    void append(double valeur, u_ustring& segment, lisp_code t, long l, long b, long e) {
        tokens.push_back(segment);
        numbers.push_back(valeur);
        integers.push_back(0);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(b);
        positions.push_back(e);
    }

    void append(long valeur, u_ustring& segment, lisp_code t, long l, long b, long e) {
        tokens.push_back(segment);
        numbers.push_back(0);
        integers.push_back(valeur);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(b);
        positions.push_back(e);
    }

};
#endif /* segmentation_h */