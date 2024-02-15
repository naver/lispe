#ifndef tokenize_h
#define tokenize_h


typedef enum {
    jt_emptystring = 0, jt_word = 1, jt_keyword = 2, jt_number = 3, 
    jt_string = 4, jt_method = 5, jt_comment = 6, jt_finalcomment = 7, jt_longstring = 8,
    jt_quote = 9, jt_o_parenthesis = 10, jt_c_parenthesis = 11, jt_bracket = 12, jt_quote_list = 13, 
    jt_o_brace = 14, jt_c_brace = 15, jt_colon = 16, jt_cadar = 17
} jag_code;


class Segmentingtype {
public:
    vector<std::string> strings;
    vector<jag_code> types;
    vector<long> positions;

    void clear() {
        types.clear();
        positions.clear();
        strings.clear();
    }

    long size() {
        return types.size();
    }

    void append(jag_code t, long posbeg, long posend) {
        types.push_back(t);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

    void append(std::string& s, jag_code t, long posbeg, long posend) {
        strings.push_back(s);
        types.push_back(t);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }
};


#endif