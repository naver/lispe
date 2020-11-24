/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//automaton.h

#ifndef automaton_h
#define automaton_h
#include "longmap.h"
#include "tools.h"
#include "conversion.h"

//----------------------------------------------------------------------------------------------------------------------------
const uchar xfarcend = 1;
const uchar xflast = 2;
const uchar xfepsilonlower = 4;
const uchar xfepsilonupper = 8;
const uchar xfmark = 16;
const uchar xfdelete = 32;
const uchar xfsorted = 64;
const uchar xflongsize = 128;

#define hashincrement 1


union octet2 {
    uint16_t v;
    char b[2];
    
    octet2() {
        v = 0;
    }
};

union octet4 {
    uint32_t v;
    char b[4];
    octet4() {
        v = 0;
    }
};

union octet8 {
    uint64_t v;
    char b[8];
    octet8() {
        v = 0;
    }
};


template <class Z> class arc_map {
public:
    
    Z* table;
    uint32_t* indexes;
    short sz, nb;
    
    arc_map<Z>() {
        sz = hashincrement;
        table = new Z[sz];
        memset(table, NULL, sz*sizeof(Z));
        indexes = new uint32_t[sz];
        memset(indexes, 0, sz*4);
        nb = 0;
    }
    
    ~arc_map<Z>() {
        delete[] table;
        delete[] indexes;
    }
    
    void redimension(short sze) {
        if (sz >= sze)
            return;
        delete[] table;
        delete[] indexes;
        sz = sze;
        table = new Z[sz];
        memset(table, NULL, sz*sizeof(Z));
        indexes = new uint32_t[sz];
        memset(indexes, 0, sz*4);
    }
    
    void resize(short sze) {
        if (sz >= sze)
            return;
        
        Z* ntable = new Z[sze];
        memset(ntable, NULL, sze*sizeof(Z));
        uint32_t* nindexes = new uint32_t[sze];
        memset(nindexes, 0, sze*4);
        
        memcpy(ntable, table, sz*sizeof(Z));
        memcpy(nindexes, indexes, sz*4);
        delete[] table;
        delete[] indexes;
        table = ntable;
        indexes = nindexes;
        sz = sze;
    }
    
    bool remove(uint32_t r) {
        for (long i = 0; i < nb; i++) {
            if (indexes[i] == r) {
                for (; i < nb - 1; i++) {
                    indexes[i] = indexes[i + 1];
                    table[i] = table[i + 1];
                }
                nb--;
                indexes[nb] = 0;
                table[nb] = NULL;
                return true;
            }
        }
        return false;
    }
    
    bool find(uint32_t r, short& last) {
        for (long i = 0; i < nb; i++) {
            if (indexes[i] == r) {
                last = i;
                return true;
            }
        }
        last = -1;
        return false;
    }
    
    bool find(uint32_t r) {
        for (long i = 0; i < nb; i++) {
            if (indexes[i] == r) {
                return true;
            }
        }
        return false;
    }
    
    Z& found(long i) {
        return table[i];
    }
    
    uint32_t code(long i) {
        return (indexes[i] >> 16);
    }
    
    uint32_t character(long i) {
        return (indexes[i] & 0xFFFF);
    }
    
    bool checkup(uint32_t r, long& i) {
        uint32_t v;
        for (++i; i < nb; i++) {
            v = indexes[i] & 0xFFFF;
            if (v <= 1 || v == r)
                return true;
            if (v > r)
                return false;
        }
        return false;
    }
    
    bool checkdown(uint32_t r, long& i) {
        uint32_t v;
        for (++i; i < nb; i++) {
            v = indexes[i] >> 16;
            if (v <= 1 || v == r)
                return true;
        }
        return false;
    }
    
    void add(uint32_t r, Z v) {
        if (v == NULL)
            return;
        if (nb < sz) {
            indexes[nb] = r;
            table[nb] = v;
            nb++;
            return;
        }
        
        Z* ntable = new Z[sz + hashincrement];
        uint32_t* nindexes = new uint32_t[sz + hashincrement];
        memset(ntable, NULL, (sz + hashincrement)*sizeof(Z));
        memset(nindexes, 0, (sz + hashincrement)*4);
        memcpy(ntable, table, sz*sizeof(Z));
        memcpy(nindexes, indexes, sz*4);
        sz += hashincrement;
        delete[] table;
        delete[] indexes;
        table = ntable;
        indexes = nindexes;
        indexes[nb] = r;
        table[nb] = v;
        nb++;
        return;
    }
    
    Z& get(uint32_t r, short last) {
        if (last != -1 && indexes[last] == r)
            return table[last];
        
        for (long i = 0; i < nb; i++) {
            if (indexes[i] == r)
                return table[i];
        }
        
        if (nb < sz) {
            indexes[nb] = r;
            return table[nb++];
        }
        
        Z* ntable = new Z[sz + hashincrement];
        uint32_t* nindexes = new uint32_t[sz + hashincrement];
        memset(ntable, NULL, (sz + hashincrement)*sizeof(Z));
        memset(nindexes, 0, (sz + hashincrement)*4);
        memcpy(ntable, table, sz*sizeof(Z));
        memcpy(nindexes, indexes, sz*4);
        sz += hashincrement;
        delete[] table;
        delete[] indexes;
        table = ntable;
        indexes = nindexes;
        indexes[nb] = r;
        return table[nb++];
    }
    
    Z& operator [](uint32_t r) {
        for (long i = 0; i < nb; i++) {
            if (indexes[i] == r)
                return table[i];
        }
        
        if (nb < sz) {
            indexes[nb] = r;
            return table[nb++];
        }
        
        Z* ntable = new Z[sz + hashincrement];
        uint32_t* nindexes = new uint32_t[sz + hashincrement];
        memset(ntable, NULL, (sz + hashincrement)*sizeof(Z));
        memset(nindexes, 0, (sz + hashincrement)*4);
        memcpy(ntable, table, sz*sizeof(Z));
        memcpy(nindexes, indexes, sz*4);
        sz += hashincrement;
        delete[] table;
        delete[] indexes;
        table = ntable;
        indexes = nindexes;
        indexes[nb] = r;
        return table[nb++];
    }
    
    long size() {
        return nb;
    }
};

class charRead {
public:
    
    agnostring w;
    vector<long> cends;
    vector<long> bends;
    
    char buff[100];
    
    
    long bbegin;
    long cbegin;
    long bend;
    long cend;
    short encoding_table;
    
    bool addoffsets;
    bool addfeatures;
    
    charRead() {
        addfeatures = false;
        addoffsets = false;
        bbegin = bend = 0;
        cbegin = cend = 0;
    }
    
    
    void init(agnostring& w)  {
        cends.clear();
        bends.clear();
        bbegin = bend = w.bytepos;
        cbegin = cend = w.charpos;
    }
    
    void eset(agnostring& w) {
        cends.push_back(w.charpos);
        bends.push_back(w.bytepos);
        
        if (cend < w.charpos) {
            cend = w.charpos;
            bend = w.bytepos;
        }
    }
    
    void eset(long b, long c) {
        bend = b;
        cend = c;
    }
    
    string extract(agnostring& w) {
        return w.substr(bbegin, bend - bbegin);
    }
    
    virtual string offsets() {
        sprintf_s(buff, 100, "+%ld+%ld+%ld+%ld", bbegin, bend, cbegin, cend);
        return buff;
    }
    
    virtual long theoffsets(char* str) {
        sprintf_s(str, 100, "\t+%ld+%ld+%ld+%ld", bbegin, bend, cbegin, cend);
        return(strlen(str));
    }
    
    virtual TRANSCHAR nextcode() {
        return 0;
    }
    
    virtual bool begin() {
        return false;
    }
    
    virtual bool end() {
        return false;
    }
    
    virtual void reset() {}
    void setpos(long b, long c) {
        w.setpos(b, c);
    }
    
    void switchcurrent() {
        w.switchcurrent();
    }

    void replacecurrent(TRANSCHAR c) {
        w.replacecurrent(c);
    }
    

    string extract() {
        return w.substr(bbegin, bend - bbegin);
    }
    
    void getpos(long& b , long& c) {
        b=w.bytepos;
        c=w.charpos;
    }
    
    long bytepos() {
        return w.bytepos;
    }
    
    long charpos() {
        return w.charpos;
    }
    
    virtual long beginbyte() {
        return w.bytepos;
    }
    
    virtual long beginchar() {
        return w.charpos;
    }
    
    
    virtual void offset(long& bb, long& be, long& cb, long& ce) {
        bb = bbegin;
        be = bend;
        cb = cbegin;
        ce = cend;
    }
    
};

class charReadString : public charRead {
public:
    
    charReadString(string& s) {
        w = s;
    }
    
    charReadString(unsigned char* s) {
        w = s;
    }
    
    bool begin() {
        w.begin();
        init(w);
        return true;
    }
    
    virtual TRANSCHAR nextcode() {
        if (encoding_table == 1)
            return w.nextcode();
        
        return c_latin_table_to_unicode(encoding_table, w.nextcode());
    }
    
    virtual bool end() {
        return w.end();
    }
    
};


class charReadFile : public charRead {
public:
    
    FILE* afile;
    long bbase, cbase;
    
    charReadFile(FILE* f) {
        afile = f;
        bbase = 0;
        cbase = 0;
    }
    
    void readline() {
        w = "";
        unsigned char c;
        bool non_spaces = false;
        while (!feof(afile)) {
            c = fgetc(afile);
            w += c;
            if (c <= 32) {
                //we do not want a line containing only spaces
                if (non_spaces && (c == 10 || c == 13))
                    break;
            }
            else
                non_spaces = true;
        }
    }
    
    bool begin() {
        readline();
        w.begin();
        init(w);
        return true;
    }
    
	TRANSCHAR nextcode() {
        if (w.end()) {
            char c[10];
            c[0] = fgetc(afile);
            long nb = c_detect_utf8((unsigned char*)c);
            if (nb) {
                nb = fread(c + 1, 1, nb, afile);
                c[nb + 1] = 0;
            }
            else
                c[1] = 0;
            
            w += c;
        }
        
        if (encoding_table == 1)
            return w.nextcode();
        
        return c_latin_table_to_unicode(encoding_table, w.nextcode());
    }
    
    void reset() {
        if (w.end()) {
            bbase += w.size();
            cbase += w.sizec();
            readline();
            w.begin();
            init(w);
        }
    }
    
    bool end() {
        if (w.end())
            return feof(afile);
        return false;
    }
    
    void offset(long& bb, long& be, long& cb, long& ce) {
        bb = bbase + bbegin;
        be = bbase + bend;
        cb = cbase + cbegin;
        ce = cbase + cend;
    }
    
    long beginbyte() {
        return (bbase + w.bytepos);
    }
    
    long beginchar() {
        return (cbase + w.charpos);
    }
    
    string offsets() {
        sprintf_s(buff, 100, "+%ld+%ld+%ld+%ld", bbase + bbegin, bbase + bend, cbase + cbegin, cbase + cend);
        return buff;
    }
    
};

//----------------------------------------------------------------------------------------------------------------------------
class DoubleSideAutomaton;
class FstCompanion;
bool compileAutomaton(DoubleSideAutomaton& a, string intrans, string outtrans, short latintable, bool norm);

class State {
public:
    unsigned char status;
    arc_map<State*> arcs;
    uint32_t id;
    bool mark;
    
    State() {
        id = 0;
        status = 0;
        mark=false;
    }
    
    State(DoubleSideAutomaton& a);
    
	State(unicodestring& w, unicodestring& lf, long posw, long posl, DoubleSideAutomaton& a);
    
    bool isend() {
        if ((status&xfarcend)==xfarcend)
            return true;
        return false;
    }
    
    void mergein(State* start, DoubleSideAutomaton& a, vector<State*>& marks);
    void merging(State* start, DoubleSideAutomaton& a);
    
	void add(unicodestring& w, unicodestring& lf, long posw, long posl, DoubleSideAutomaton& a);
    void regulars(DoubleSideAutomaton& a);
    bool rgx(DoubleSideAutomaton& a, vector<wstring>& vs, long& i, vector<uint32_t>& indexes);
    bool parsergx(DoubleSideAutomaton& a, agnostring& e, vector<uint32_t>& indexes);
    State* parse(DoubleSideAutomaton& a, vector<wstring>& vs, vector<uchar>& types, long i, vector<uint32_t>& indexes, State* common);
    bool parse(DoubleSideAutomaton& a, agnostring& e, vector<uint32_t>& indexes);
    bool parse(DoubleSideAutomaton& a, wstring& e, vector<uint32_t>& indexes);
    bool loadtext(string name, DoubleSideAutomaton& a);
    bool factorize(DoubleSideAutomaton& a, long first=1);
    bool addmap(hmap<string, string>& lexicon, DoubleSideAutomaton& a);
    bool load(string name, DoubleSideAutomaton& a);
    void loadarcs(ifstream& dump, hmap<uint32_t, uint32_t>&, DoubleSideAutomaton& a);
    bool compile(string name, DoubleSideAutomaton& a);
    void savearc(ofstream& dump, hmap<uint32_t, uint32_t>&);
    bool up(FstCompanion* fst, long, long threshold, short flags);
    bool down(vector<unsigned short>& w, long, FstCompanion* f, char lemma);
    bool finals(FstCompanion* f, long threshold);
    
    bool process(charRead& w, bool punct, FstCompanion* f,long threshold, short flags);
    bool editdistance(charRead& w, bool punct, FstCompanion* f,long threshold, short flags);
    void sorting();
};

class State_Vectorized : public State {
public:
    
    binlong_hash<vector<long> > arcsv;
    
    State_Vectorized(DoubleSideAutomaton& a) : arcsv(false), State(a) {}
    
    void shuffle() {
        uint32_t u;
        arcsv.clear();
        for (long i = 0; i < arcs.nb; i++) {
            u = arcs.indexes[i] & 0xFFFF;
            arcsv[u].push_back(i);
        }
    }
    bool vprocess(charRead& w, FstCompanion* f, long threshold, short flags);
};

struct Gates {
public:
    hmap<string, ushort> codes;
    basebin_hash<uchar> actions;
    basebin_hash<ushort> attributes;
    basebin_hash<ushort> values;
    
    bool isgate(ushort c) {
        return attributes.check(c);
    }
    
    void add(string r, ushort c) {
        string sub = r.substr(3,r.size()-4);
        string val;
        long ps = sub.find('.');
        if (ps != -1) {
            val=sub.substr(ps+1,sub.size()-ps);
            if (codes.find(val) == codes.end())
                codes[val] = codes.size()+1;
            values[c] = codes[val];
            sub=sub.substr(0,ps);
        }
        if (codes.find(sub) == codes.end())
            codes[sub] = codes.size()+1;
        attributes[c] = codes[sub];
        switch (r[1]) {
            case 'C':
                actions[c] = 'C';
                break;
            case 'D':
                actions[c] = 'D';
                break;
            case 'P':
                actions[c] = 'P';
                break;
            case 'R':
                actions[c] = 'R';
                break;
            case 'S':
                actions[c] = 'S';
                break;
            case 'U':
                actions[c] = 'U';
                break;
        }
    }
};

class DoubleSideAutomaton {
public:
    
    vector<State*> garbage;
    hmap<string, unsigned short> alphabet;
	hmap<TRANSCHAR, State*> features;
    hmap<wstring, TRANSCHAR> multis;
    vector<wstring> sortedmultis;
    
    hash_bin<unsigned short, string> ialphabet;
    basebin_hash<unsigned short> encoding;
    basebin_hash<unsigned short> decoding;
    basebin_hash<unsigned short> featurecode;

    State_Vectorized start;
    Gates* gates;
    long encoding_table;
    bool finalize;
    bool normalize;
    
    DoubleSideAutomaton() : start(*this), ialphabet(false) {
        normalize = false;
        finalize = false;
        encoding_table = 1;
        ialphabet[0] = "";
        index(0xFFFE); //the "any" character, a weird hack with the hope that it won't disrupt the whole architecture
        index(9);
        index(10);
        index(13);
        index(32);
        index(160);
        index(0x202F);
        gates = NULL;
    }
    
    ~DoubleSideAutomaton() {
        //we cannot delete the first element, which is defined here...
        for (long i = 1; i < garbage.size(); i++) {
            if (garbage[i] != NULL)
                delete garbage[i];
        }
        if (gates != NULL)
            delete gates;
    }
    
    void clearmarks() {
        for (long i = 0; i < garbage.size(); i++) {
            if (garbage[i] != NULL) {
                garbage[i]->mark=false;
                garbage[i]->status &= ~xfmark;
                garbage[i]->status &= ~xfdelete;
            }
        }
    }
    
    void clearmarks(long d, long f) {
        for (long i = d; i <= f; i++) {
            if (garbage[i] != NULL)
                garbage[i]->mark=false;
        }
    }
    
    void clearmarksfrom(long d) {
        if (d == 1)
            d = 0;
        for (long i = d; i < garbage.size(); i++) {
            if (garbage[i] != NULL)
                garbage[i]->mark=false;
        }
    }
    
    void Clear() {
        for (long i = 1; i < garbage.size(); i++) {
            if (garbage[i] != NULL)
                delete garbage[i];
        }
        alphabet.clear();
        ialphabet.clear();
        features.clear();
        encoding.clear();
        decoding.clear();
        garbage.clear();
    }
    
    void fillencoding(bool add);
    
    State* addfeature(uint32_t p, State* a = NULL) {
        try {
            return features.at(p);
        }
        catch(const std::out_of_range& oor) {
            if (a == NULL)
                a = new State(*this);
            features[p] = a;
            a->status |= xfarcend;
            return a;
        }
    }
    
    void regulars() {
        start.regulars(*this);
    }
    
    bool up(wstring& w, vector<string>& res, long threshold, short flags);
    
    bool down(wstring& w, vector<string>& res, char);
    
    long index(string s) {
        try {
            return alphabet.at(s);
        }
        catch(const std::out_of_range& oor) {
            unsigned short fpos = 1 + alphabet.size();
            alphabet[s] = fpos;
            ialphabet[fpos] = s;
            return fpos;
        }
    }
    
    long index(TRANSCHAR c) {
        return index(c_unicode_to_utf8(c));
    }
    
	long code(TRANSCHAR c) {
        return encoding.search(c);
    }
    
    bool load(string name) {
        return start.load(name, *this);
    }
    
    bool loadtext(string name) {
        return start.loadtext(name, *this);
    }
    
    bool addmap(hmap<string, string>& lexicon) {
        return start.addmap(lexicon, *this);
    }
    
    bool compile(string name, string res) {
        if (loadtext(name) == false)
            return false;
        return start.compile(res, *this);
    }
    
    bool store(string name) {
        return start.compile(name, *this);
    }
    
    bool process(charRead& w, vector<string>& res, bool option, long threshold, short flags);
    
    void merge(DoubleSideAutomaton* aut);
    
    void factorize(long last) {
        start.factorize(*this, last);
    }
    
    void sorting();
};

class FstCompanion {
public:
    unicodestring w;
    basebin_hash<short> gates;
    char s[4096];
    char f[2048];
    DoubleSideAutomaton* a;
    vector<string>* res;
    long previous, pos, i, sz, fprevious, fpos;
    long threshold;
    bool addfeatures;
    bool gate;
    
    FstCompanion() {
        threshold = 0;
        res = NULL;
        a = NULL;
        s[0] = 0;
        pos = 0;
        previous = 0;
        addfeatures = false;
        gate = false;
    }
    
    void set(wstring& ww, DoubleSideAutomaton* aa, vector<string>& rr) {
        a = aa;
        w = ww;
        res = &rr;
        sz = w.size();
        s[0] = 0;
        pos = 0;
        previous = 0;
        fpos = 0;
        fprevious = 0;
        gates.clear();
        gate = false;
        if (a->gates != NULL)
            gate = true;
    }
    
    void displayarcs(State* tf, long beg=0) {
        uint32_t prev;
        for (long ii = beg; ii < tf->arcs.nb; ii++) {
            prev = tf->arcs.code(ii);
            if (!prev) {
                prev = tf->arcs.character(ii);
                if (!prev)
                    cerr<<"EPS ";
                else
                    cerr<<a->ialphabet[prev]<<" ";
            }
            else {
                cerr<<a->ialphabet[prev]<<" ";
            }
        }
        cerr<<endl<<"---------------"<<endl;
    }
    
    void displayarc(State* tf, long ii) {
        uint32_t prev = tf->arcs.code(ii);
        if (!prev) {
            prev = tf->arcs.character(ii);
            if (!prev)
                cerr<<"EPS ";
            else
                cerr<<a->ialphabet[prev];
        }
        else {
            cerr<<a->ialphabet[prev];
        }
        cerr<<endl<<"---------------"<<endl;
    }
    
    void set(DoubleSideAutomaton* aa, vector<string>& rr, bool addf, long th) {
        threshold = th;
        a = aa;
        res = &rr;
        sz = w.size();
        s[0] = 0;
        f[0] = 0;
		pos = 0;
        previous = 0;
        fprevious = 0;
        fpos = 0;
        addfeatures = addf;
    }
    
    inline long addforparse(ushort c) {
        string& r = a->ialphabet[c];
        i = 0;
        //we are adding things to the feature list or it is a feature
        if (r[0] == '\t' || fpos) {
            fprevious = fpos + 1;
            if (!addfeatures) {
                //We add a pseudo feature, in order to avoid pushing unrelated things to the lemma form.
                fpos= 1;
                return -fprevious;
            }
            
            for (i=1;i<r.size(); i++)
                f[fpos++] = r[i];
            f[fpos]=0;
            return -fprevious;
        }
        
        previous = pos;
        for (;i<r.size(); i++)
            s[pos++] = r[i];
        s[pos] = 0;
        return previous;
    }
    
    inline void resetfeatures(long p) {
        if (p < 0) {
            fpos = -p-1;
            f[fpos] = 0;
        }
        else {
            pos = p;
            s[pos] = 0;
        }
    }
    
    inline char checkgate(ushort cd, ushort val) {
        if (gates.check(cd)) {
            if (!val)
                return 2;
            
            short v = gates.get(cd);
            if (v<0) {
                v = -v;
                if (val == v)
                    return 1;
                return 0;
            }
            if (val == v)
                return 0;
            return 1;
        }
        return -1;
    }
    
    inline char isgate(ushort c) {
        if (!gate)
            return 0;
        
        if (a->gates->isgate(c)) {
            short attribute = a->gates->attributes.get(c);
            short val = 0;
            char ret;
            if (a->gates->values.check(c))
                val = a->gates->values.get(c);
            
            switch (a->gates->actions.get(c)) {
                case 'C':
                    gates.erase(attribute);
                    return 1;
                case 'D':
                    ret = checkgate(attribute, val);
                    if (ret == 2 || !ret)
                        return 2;
                    return 1;
                case 'P':
                    gates[attribute] = val;
                    return 3;
                case 'N':
                    gates[attribute] = -val;
                    return 3;
                case 'R':
                    ret = checkgate(attribute, val);
                    if (ret == 2 || !ret)
                        return 1;
                    return 2;
                case 'U': {
                    ret = checkgate(attribute, val);
                    if (!ret)
                        return 1;

                    if (ret == -1) {
                        gates[attribute] = val;
                        return 3;
                    }
                    
                    return 2;
                }
            }
        }
        return 0;
    }
    
    inline void resetgates(ushort c, uchar flag) {
        if (flag == 3)
            gates.erase(a->gates->attributes.get(c));
    }
    
    
    inline bool isfeature(long c) {
        return a->featurecode.check(c);
    }
    
    inline long addalphabet(ushort c) {
        string& r = a->ialphabet[c];
        i = 0;
        if (r[0] == '\t') {
            //We keep only one tab
            if (strchr(s,'\t'))
                i = 1;
        }
        
        previous = pos;
        for (;i<r.size(); i++)
            s[pos++] = r[i];
        s[pos] = 0;
        return previous;
    }

    inline long addfeature(ushort c) {
        string& r = a->ialphabet[c];
        fprevious = fpos;
        for (i = 1;i<r.size(); i++)
            f[fpos++] = r[i];
        f[fpos] = 0;
        return fprevious;
    }
    

	inline long add(TRANSCHAR r) {
        previous = pos;
        s[pos++] = (char)r;
        s[pos] = 0;
        return previous;
    }
    
    
    inline void reset(long p) {
        pos = p;
        s[pos] = 0;
    }

    inline void resetfeature(long p) {
        fpos = p;
        f[fpos] = 0;
    }
    

    inline void storingfeatures(long dst) {
        res->push_back(s);
        if (addfeatures) {
            long rep = fpos;
            char c = f[fpos];
            if (threshold) {
                dst = threshold - dst;
                f[rep++] = '+';
                f[rep++] = 'd';
                f[rep++] = 's';
                f[rep++] = 't';
                f[rep++] = '_';
                char buff[10];
                sprintf_s(buff,10,"%ld",dst);
                for (int i = 0; i < strlen(buff); i++)
                    f[rep++] = buff[i];
                f[rep++] = 0;
            }
            res->push_back(f);
            f[fpos] = c;
        }
    }
    
    inline void storingfull() {
        res->push_back(s);
        if (!fpos)
            return;

        res->back() += '\t';
        res->back() += f;
    }
    
    inline void storing() {
        res->push_back(s);
    }
    
    inline bool empty() {
        return (!pos);
    }
};

#endif

