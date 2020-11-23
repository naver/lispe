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

using std::string;
using std::vector;

#define uchar unsigned char

typedef enum {e_token, e_opening_parenthesis, e_closing_parenthesis,
    e_operator, e_quote, e_string, e_emptystring, e_digits, e_cadr,
    e_opening_brace, e_closing_brace, e_colon,
    e_character, e_error_brace, e_error_parenthesis, e_error_string, e_no_error,
} e_type;

class Tokenizer {
public:
    vector<string> tokens;
    vector<double> numbers;
    vector<e_type> types;
    vector<long> lines;
    vector<long> positions;

    void clear() {
        tokens.clear();
        numbers.clear();
        types.clear();
        lines.clear();
        positions.clear();
    }
    
    void append(uchar segment, e_type t, long l, long posbeg, long posend) {
        string str;
        str = segment;
        tokens.push_back(str);
        numbers.push_back(0);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

    void append(string segment, e_type t, long l, long posbeg, long posend) {
        tokens.push_back(segment);
        numbers.push_back(0);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

    void append(double valeur, string segment, e_type t, long l, long posbeg, long posend) {
        tokens.push_back(segment);
        numbers.push_back(valeur);
        types.push_back(t);
        lines.push_back(l);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

};


#endif /* segmentation_h */
