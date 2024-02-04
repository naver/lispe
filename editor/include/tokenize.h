#ifndef tokenize_h
#define tokenize_h


typedef enum {
    jt_emptystring = 0, jt_word = 1, jt_keyword = 2, jt_number = 3, 
    jt_string = 4, jt_method = 5, jt_comment = 6, jt_finalcomment = 7, jt_longstring = 8,
    jt_colon = 9, jt_opening_p = 10, jt_closing_p = 11, jt_opening_bk = 12, jt_closing_bk = 13, jt_opening_br = 14, jt_closing_br = 15
} jag_code;


class Segmentingtype {
public:
    vector<std::string> strings;
    vector<double> numbers;
    vector<jag_code> types;
    vector<long> positions;

    void clear() {
        types.clear();
        positions.clear();
        strings.clear();
        numbers.clear();
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
        numbers.push_back(0);
        types.push_back(t);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

    void append(double d, jag_code t, long posbeg, long posend) {
        strings.push_back("");
        numbers.push_back(d);
        types.push_back(t);
        positions.push_back(posbeg);
        positions.push_back(posend);
    }

    void insertclosing(long pos) {
        strings.insert(strings.begin()+pos, ")");
        numbers.insert(numbers.begin()+pos, 0);
        types.insert(types.begin()+pos, jt_closing_p);        
        positions.insert(positions.begin()+pos, 0);
        positions.insert(positions.begin()+pos, 0);
    }
};


#endif