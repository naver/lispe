/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//automaton.cxx

//----------------------------------------------------------------------------------
//------------ DoubleSideAutomaton Section -----------------------------------------------
//----------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "automaton.h"
#include "longmap.h"
#include "x_node.h"

#ifdef INTELINTRINSICS
#ifdef WIN32
#include <intrin.h>
#else
#include <x86intrin.h>
#endif
#endif

ostream* flot_erreur = &cerr;

#define action_first 1
#define action_change 2
#define action_delete 4
#define action_insert 8
#define action_switch 16
#define action_nocase 32
#define action_repetition 64
#define action_vowel 128 //we can only replace a vowel with another vowel
#define action_surface 256 //we can only replace a vowel with another vowel
#define action_longest_match 512 //we can only replace a vowel with another vowel

const short action_suball = action_change | action_insert | action_vowel;
const short action_fullall = action_change | action_insert | action_vowel | action_nocase;

#define isaction(a) (flags & a) == a
#define isnotaction(a) (flags & a) == 0

//----------------------------------------------------------------------------------

static FstCompanion Companion;

//----------------------------------------------------------------------------------
static void replaceonechar(wstring& buffer, TRANSCHAR rep, long pos, long ssz) {
	long sz = buffer.size();
	buffer[pos++] = rep;
	while (pos + ssz <= sz) {
		buffer[pos] = buffer[pos + ssz - 1];
		pos++;
	}
	sz -= ssz - 1;
    buffer.resize(sz);
}

bool compileautomaton(string intrans, string outtrans, short latintable, bool norm) {
	DoubleSideAutomaton a;
	if (!valid_latin_table(latintable))
		return false;

	a.encoding_table = latintable;
	a.normalize = norm;
	return a.compile(intrans, outtrans);
}

bool compileAutomaton(DoubleSideAutomaton& a, string intrans, string outtrans, short latintable, bool norm) {
	if (!valid_latin_table(latintable))
		return false;

	a.encoding_table = latintable;
	a.normalize = norm;
	return a.compile(intrans, outtrans);
}

State::State(DoubleSideAutomaton& a) {
	id = a.garbage.size();
	a.garbage.push_back(this);
	status = 0;
    mark=false;
}

State::State(unicodestring& w, unicodestring& lf, long posw, long posl, DoubleSideAutomaton& a) {
    mark=false;
	id = a.garbage.size();
	a.garbage.push_back(this);
	status = 0;
	add(w, lf, posw, posl, a);
}

void State::loadarcs(ifstream& dump, hmap<uint32_t, uint32_t>& allarcs, DoubleSideAutomaton& a) {
	char buff[2];
	dump.read(buff, 1);
	status = buff[0];

	if (status & xflast)
		return;

    octet2 v2;
    if ((status & xflongsize) == xflongsize) {
        dump.read(v2.b, 2);
        arcs.nb = v2.v;
        status &= ~xflongsize;
    }
    else {
        dump.read(buff, 1);
        arcs.nb = (uchar)buff[0];
    }

	arcs.redimension(arcs.nb + 1);

	octet4 v4;

	static unsigned short vx = 1 << 14;
	static unsigned short vxx = 1 << 15;
	long val, vbuff;
	for (long i = 0; i < arcs.nb; i++) {
		dump.read(v2.b, 2);
		if ((v2.v & vxx) == vxx) {
			v2.v &= ~vxx;
			arcs.indexes[i] = allarcs[v2.v];
			dump.read(v4.b, 4);
			arcs.table[i] = a.garbage[v4.v];
		}
		else {
			if ((v2.v & vx) == vx) {
				v2.v &= ~vx;
				arcs.indexes[i] = allarcs[v2.v];
				dump.read(buff, 1);
				dump.read(v2.b, 2);
				val = v2.v;
				val <<= 8;
				vbuff = (uchar)buff[0];
				val |= vbuff;
				arcs.table[i] = a.garbage[val];
			}
			else {
				arcs.indexes[i] = allarcs[v2.v];
				dump.read(v2.b, 2);
				arcs.table[i] = a.garbage[v2.v];
			}
		}
	}
}

bool State::load(string name, DoubleSideAutomaton& a) {
	ifstream dump(STR(name), std::ios::in|std::ios::binary);
	if (dump.fail())
		return false;
	octet2 v;

	//we read the signature+the table encoding... 24,9,16
	char buff[2048];
	dump.read(buff, 5);
	if (buff[0] != 24 || buff[1] != 9 || buff[2] != 16)
		return false;

	a.encoding_table = buff[3];
	a.normalize = buff[4];

	//the size
	dump.read(v.b, 2);
	long sz = v.v;
	uint32_t i;
    string sconv;
	for (i = 1; i <= sz; i++) {
		dump.read(v.b, 2); //the string size
        if (!v.v)
            sconv = (wchar_t)1;
        else {
            dump.read(buff, v.v);
            buff[v.v] = 0;
            sconv = conversion_latin_table_to_utf8(a.encoding_table, (uchar*)buff, v.v);
        }
		a.ialphabet[i] = sconv;
		a.alphabet[sconv] = i;
        if (sconv.size() > 1) {
            if (sconv[0]=='@' && sconv[sconv.size()-1]=='@') {
                if (a.gates == NULL)
                    a.gates = new Gates;
                a.gates->add(sconv, i);
            }
        }
	}

    a.ialphabet[0] = "";

	a.fillencoding(true);
	octet4 vv;
	dump.read(vv.b, 4);
	//We read the number of elements...
	State* trans;

	long nbarcs = vv.v;
	for (i = 1; i < nbarcs; i++)
		trans = new State(a);

	//Nb arc labels
	dump.read(v.b, 2);
	hmap<uint32_t, uint32_t> allarcs;
	for (i = 0; i < v.v; i++) {
		dump.read(vv.b, 4);
		allarcs[i] = vv.v;
	}
    
	for (i = 0; i < nbarcs; i++) {
		if ((i % 5000) == 0)
			*flot_erreur << "<";
		a.garbage[i]->loadarcs(dump, allarcs, a);
	}
    
	*flot_erreur << endl;
    if (a.gates != NULL) {
        State* ar;
        uint32_t code;
        uint32_t chr;
        for (i = 0; i < nbarcs; i++) {
            ar = a.garbage[i];
            for (long ii = 0; ii < ar->arcs.nb; ii++) {
                code = ar->arcs.code(ii);
                chr = ar->arcs.character(ii);
                if (a.gates->isgate(chr) && a.gates->isgate(code)) {
                    ar->status |= xfepsilonlower;
                    ar->arcs.indexes[ii] = code << 16;
                }
            }
        }
    }
    
    sorting();
	return true;
}

void State::savearc(ofstream& dump, hmap<uint32_t, uint32_t>& allarcs) {
	//we check first if all indexes are identities
	char buff[] = { 0, 0, 0 };

	if (arcs.nb == 0) {
		buff[0] = status | xflast;
		dump.write(buff, 1);
		return;
	}

	octet2 v2;
	octet4 v4;

    if (arcs.nb > 254) {
        buff[0] = status | xflongsize;
        dump.write(buff, 1);
        v2.v = arcs.nb;
        dump.write(v2.b, 2);
    }
    else {
        buff[0] = status; buff[1] = (uchar)arcs.nb;
        dump.write(buff, 2);
    }
    
    buff[0] = 0; buff[1] = 0;
	uint32_t idx;
	for (long i = 0; i < arcs.nb; i++) {
		idx = arcs.table[i]->id;
		v2.v = allarcs[arcs.indexes[i]];
		if ((idx & 0xFFFF) == idx) {
			dump.write(v2.b, 2);
			v2.v = idx;
			dump.write(v2.b, 2);
		}
		else {
			if ((idx & 0xFFFFFF) == idx) {
				buff[0] = idx & 0xFF;
				v2.v |= (1 << 14);
				dump.write(v2.b, 2);
				dump.write(buff, 1);
				v2.v = idx >> 8;
				dump.write(v2.b, 2);
			}
			else {
				v2.v |= (1 << 15);
				dump.write(v2.b, 2);
				v4.v = idx;
				dump.write(v4.b, 4);
			}
		}
	}
}

//we suppose our automaton without any loops... A pure lexicon
bool State::compile(string name, DoubleSideAutomaton& a) {
	map<unsigned short, string> sorted;

	hash_bin<unsigned short, string>::iterator it;
	for (it = a.ialphabet.begin(); it != a.ialphabet.end(); it++) {
		if (it->first != 0)
			sorted[it->first] = it->second;
	}

	ofstream dump(STR(name), ios::binary);
	
	//We write the signature at the beginning of the file
	char buff[5] = { 24, 9, 16, 1, 0 };

	if (a.encoding_table != 1)
		buff[3] = a.encoding_table;
	buff[4] = a.normalize;

	dump.write(buff, 5);

	octet2 v;
	v.v = sorted.size();
	dump.write(v.b, 2);

	for (auto& its : sorted) {
		v.v = its.second.size();
		dump.write(v.b, 2);
		dump.write(its.second.c_str(), its.second.size());
	}

	//traverse(dump);
	//We reindex to skip the NULL...
	uint32_t idx = 0;
	long i;
	for (i = 0; i < a.garbage.size(); i++) {
		if (a.garbage[i] != NULL)
			a.garbage[i]->id = idx++;
	}

	octet4 vv;
	vv.v = idx;
	dump.write(vv.b, 4);

	long total = 0;
	hmap<uint32_t, uint32_t> allarcs;
	vector<uint32_t> varcs;
	uint32_t e;
	for (i = 0; i < a.garbage.size(); i++)  {
		if (a.garbage[i] == NULL)
			continue;

		for (idx = 0; idx < a.garbage[i]->arcs.nb; idx++) {
			e = a.garbage[i]->arcs.indexes[idx];
			if (allarcs.find(e) == allarcs.end())  {
				allarcs[e] = varcs.size();
				varcs.push_back(e);
			}
			total++;
		}
	}

	v.v = varcs.size();
	dump.write(v.b, 2);
	for (i = 0; i < varcs.size(); i++) {
		vv.v = varcs[i];
		dump.write(vv.b, 4);
	}

	for (i = 0; i < a.garbage.size(); i++) {
		if (a.garbage[i] != NULL)
			a.garbage[i]->savearc(dump, allarcs);
	}

	return true;

}
//-----------------------------------------------------------
//We add a pair of characters to the automaton...
//We conflate the character code of the lower and upper sides on one single 32 bits value...
void State::add(unicodestring& w, unicodestring& lf, long posw, long posl, DoubleSideAutomaton& a) {
	TRANSCHAR cw;
    short last;

	if (posw < w.size()) {
		cw = w[posw++];
		cw = a.index(cw);
		//we merge the bits of the two characters as a key
		if (posl < lf.size()) {
			TRANSCHAR  clf = lf[posl++];
			//the last character is the feature structure, which we do not want to reinterpret
			if (posl < lf.size())
				clf = a.index(clf);
			//the upper side is stored on the upper bits
			cw |= clf << 16;
		}
		else
			status |= xfepsilonupper;

		if (!arcs.find(cw,last))
			arcs[cw] = new State(w, lf, posw, posl, a);
		else
			arcs.get(cw,last)->add(w, lf, posw, posl, a);
		return;
	}


    if (posl < lf.size()) {
		cw = lf[posl++];
		//the last character is the feature structure, which we do not want to reinterpret
		if (posl < lf.size())
			cw = a.index((uint32_t)cw);
		cw <<= 16;
		if (!arcs.find(cw, last)) {
			if (a.features.find(cw) != a.features.end())
				arcs[cw] = a.features[cw];
			else
				arcs[cw] = new State(w, lf, posw, posl, a);
		}
		else
			arcs.get(cw,last)->add(w, lf, posw, posl, a);
		status |= xfepsilonlower;
	}
    else
        status |= xfarcend;
}

bool State::loadtext(string name, DoubleSideAutomaton& a) {
	ifstream f(name.c_str(), ios::in | ios::binary);
	if (f.fail())
		return false;

	agnostring w;
	agnostring l;
	unicodestring wstr;
	unicodestring lstr;
#ifdef WSTRING_IS_UTF16
	wstring str;
#endif


	string feats;
	long compte = 0;
	long fpos = 0;

	while (!f.eof()) {
		getline(f, w);
		getline(f, l);
		w = w.trim();
		l = l.trim();
		if (w.size() && l.size()) {
			fpos = l.rfind("\t", l.size());
			if (fpos != string::npos) {
				feats = l.substr(fpos + 1, l.size() - fpos);
				l = l.substr(0, fpos);
				fpos = a.index(feats);
				a.addfeature(fpos << 16);

#ifdef WSTRING_IS_UTF16
				str = w.latintounicode(a.encoding_table);
				wstr = str;
				
				str = l.latintounicode(a.encoding_table);
				str += (wchar_t)fpos;
				lstr = str;
#else
				wstr = w.latintounicode(a.encoding_table);
				lstr = l.latintounicode(a.encoding_table);
				lstr += (wchar_t)fpos;
#endif
				add(wstr, lstr, 0, 0, a);
				compte++;
				if ((compte % 100000) == 0)
					*flot_erreur << "<";
			}
		}
	}

	return factorize(a);
}



bool State::addmap(hmap<string, string>& lexicon, DoubleSideAutomaton& a) {

	agnostring w;
	agnostring l;
	unicodestring wstr;
	unicodestring lstr;

#ifdef WSTRING_IS_UTF16
	wstring str;
#endif

	string feats;
	long compte = 0;
	long fpos = 0;

	for (auto& it : lexicon) {
		w = it.first;
		l = it.second;
		if (w.size() && l.size()) {
			fpos = l.rfind("\t", l.size());
			if (fpos != string::npos) {
				feats = l.substr(fpos, l.size() - fpos);
				l = l.substr(0, fpos);
				fpos = a.index(feats);
				a.addfeature(fpos << 16);

#ifdef WSTRING_IS_UTF16
				str = w.latintounicode(a.encoding_table);
				wstr = str;
				str = l.latintounicode(a.encoding_table);
				str += (wchar_t)fpos;
				lstr = str;
#else
				wstr = w.latintounicode(a.encoding_table);
				lstr = l.latintounicode(a.encoding_table);
				lstr += (wchar_t)fpos;
#endif
				add(wstr, lstr, 0, 0, a);
				compte++;
				if ((compte % 10000) == 0)
					*flot_erreur << "<";
			}
		}
	}

	return factorize(a);
}
//----------------------------------------------------------------------------------------
static bool comparenodes(State* x1, State* x2) {
    //All arcs should be in both elements
    if (x1 == NULL || x2 == NULL)
        return false;
    
    if (x1 == x2)
        return true;
    
    if (x1->status != x2->status)
        return false;
    
    if (x1->arcs.nb != x2->arcs.nb)
        return false;
    
    long i;
    for (i = 0; i < x1->arcs.nb; i++) {
        if (x2->arcs.find(x1->arcs.indexes[i]) == false)
            return false;
    }
    
    for (i = 0; i < x1->arcs.nb; i++) {
        if (!comparenodes(x1->arcs.table[i], x2->arcs[x1->arcs.indexes[i]]))
            return false;
    }
    
    x2->status |= xfdelete;
    
    return true;
}

static void fstsize(long& idx, State* xf) {
    if (xf == NULL)
        return;
    
    if (xf->mark)
        return;
    
    xf->mark=true;
    
    idx += xf->arcs.nb;
    for (long i = 0; i < xf->arcs.nb; i++)
        fstsize(idx, xf->arcs.table[i]);
    
    xf->mark = false;
}

//simplify compares lists of arcs gathered according to their size or the ids of the next arcs.
static void simplify(map<long, hmap<long, vector<State*> > >& pile, binlong_hash<long>& toberemoved, bool top) {
    long i, n;
    State* xe;
    State* xf;
    long sz;
    long I=0;

    *flot_erreur << pile.size() << " blocs" <<endl;

    for (auto& its : pile) {
        *flot_erreur << ".";
        I++;
        if ((I%10) == 0)
            *flot_erreur << I << "/" << pile.size();
        
        for (auto& it : its.second) {
            
            vector<State*>& v = it.second;
            sz = v.size();
            
            if (sz > 1) {
                for (i = 0; i < sz; i++) {
                    xf = v[i];
                    
                    if (toberemoved.check(xf->id))
                        continue;
                    
                    for (n = i + 1; n < sz; n++) {
                        xe = v[n];
                        if (toberemoved.check(xe->id)) {
                            continue;
                        }
                        
                        if (comparenodes(xf, xe))
                            toberemoved[xe->id] = xf->id;
                    }
                }
            }
        }
    }
}

bool State::factorize(DoubleSideAutomaton& a, long first) {
    a.clearmarks();
	*flot_erreur << endl << a.garbage.size() << " arcs" << endl;
	long i, n, nb;
	State* xf;
	map<long, hmap<long, vector<State*> > > pile;
    for (i = first; i < a.garbage.size(); i++) {
		xf = a.garbage[i];
		if (xf == NULL)
			continue;
        for (n = 0; n < xf->arcs.nb; n++) {
            if (xf->arcs.table[n]->arcs.nb) {
                nb = 0;
                fstsize(nb, xf->arcs.table[n]);
                pile[nb][xf->arcs.indexes[n]].push_back(xf->arcs.table[n]);
            }
        }
	}

	binlong_hash<long> toberemoved(true, a.garbage.size()>>4);
    
    *flot_erreur << endl;
	simplify(pile, toberemoved, true);
	*flot_erreur << endl;


    binuint64 filter;
    unsigned long qj;
    long j;
    for (i = 0; i < toberemoved.tsize; i++) {
        if (toberemoved.table[i] != NULL) {
            j=0;
            filter = toberemoved.indexes[i];
            while (filter) {
#ifdef INTELINTRINSICS
                if (!(filter & 1)) {
                    if (!(filter & 0x00000000FFFFFFFF)) {
                        filter >>= 32;
                        j += 32;
                    }
					bitscanforward(qj, (uint32_t)(filter & 0x00000000FFFFFFFF));
                    filter >>= qj;
                    j += qj;
                }
#else
                if (!(filter & 1)) {
                    while (!(filter & 65535)) {
                        filter >>= 16;
                        j+=16;
                    }
                    while (!(filter & 255)) {
                        filter >>= 8;
                        j+=8;
                    }
                    while (!(filter & 15)) {
                        filter >>= 4;
                        j+=4;
                    }
                    while (!(filter & 1)) {
                        filter >>= 1;
                        j++;
                    }
                }
#endif
                n = toberemoved.table[i][j];
                a.garbage[n]->status |= xfmark;

                filter >>= 1;
                j++;
            }
        }
    }
    
	a.start.status |= xfmark;

	State* xe;
	//We have identified with simplify the arcs, which are similar. For each of these arcs, we have kept one as
	//the replacement. This procedure replaces each of these arcs with their replacement.
	//The arcs that we want to keep are marked
	for (i = 0; i < a.garbage.size(); i++) {
		xf = a.garbage[i];
		if (xf == NULL)
			continue;

		if (toberemoved.check(xf->id))
			continue;

		for (n = 0; n < xf->arcs.nb; n++) {
			xe = xf->arcs.table[n];
			if (toberemoved.check(xe->id))
				xf->arcs.table[n] = a.garbage[toberemoved[xe->id]];
			else
				xe->status |= xfmark;
		}
	}

	//we then delete the unmarked arcs
	n = 0;
	for (i = 0; i < a.garbage.size(); i++) {
		xf = a.garbage[i];
		if (xf == NULL)
			continue;

		if (xf->status & xfmark) {
			xf->status &= ~xfmark;
			n++;
		}
		else {
			delete xf;
			a.garbage[i] = NULL;
		}
	}

    a.sorting();
	*flot_erreur << n << " factorized arcs remain" << endl;

	return true;
}

//-----------------------------------------------------------
class x_wdoubleautomaton : public x_wreading {
public:
    
    x_wdoubleautomaton() {
        juststack=true;
    }
    
    void setrules() {
        rules.push_back("%r=#");
        rules.push_back("%s=#");
        
        rules.push_back("${%a %d %H #. #, #;}+%+=25");
        rules.push_back("${%a %d %H #. #, #;}+*=26");
        rules.push_back("${%a %d %H #. #, #;}+=27");

        rules.push_back("!%d+=30");
        
        rules.push_back("?%+=40"); //the any character...
        rules.push_back("?*=41");
        rules.push_back("?=42");

        rules.push_back("%% %+=4");
        rules.push_back("%% %*=5");
        rules.push_back("%% %=6");

        rules.push_back("%%%.%+=4");
        rules.push_back("%%%.*=5");
        rules.push_back("%%%.=6");
        
        rules.push_back("{=7");
        rules.push_back("}%+=8");
        rules.push_back("}*=9");
        rules.push_back("}=10");
        
        rules.push_back("[=11");
        rules.push_back("]%+=12");
        rules.push_back("]*=13");
        rules.push_back("]=14");
        
        rules.push_back("(=15");
        rules.push_back(")=16");
        
        rules.push_back("%d%-%d%+=20");
        rules.push_back("%a%-%a%+=20");
        rules.push_back("%d%-%d*=21");
        rules.push_back("%a%-%a*=21");
        rules.push_back("%d%-%d=22");
        rules.push_back("%a%-%a=22");
        rules.push_back("%.%+=1");
        rules.push_back("%.*=2");
        rules.push_back("%.=3");
    }
    
    void tokenize(wstring& thestr, vector<wstring>& vstack, vector<uchar>& vtypes) {
        //only stack is necessary
        apply(thestr,false, &vstack, &vtypes);
    }
    
};

//-----------------------------------------------------------

static bool comp(uint32_t s1, uint32_t s2) {
    uint32_t a1 =  s1 & 0xFFFF;
    uint32_t a2 =  s2 & 0xFFFF;
    
    if (a1 < a2)
        return true;
    
    return false;
}

//we sort arcs according to their index
void DoubleSideAutomaton::sorting() {
    vector<uint32_t> ids;
    hmap<uint32_t, State*> table;
    State* a;
    long j;
    for (long i = 0; i < garbage.size(); i++) {
        a = garbage[i];
        if (a != NULL && a->arcs.nb > 1 && !(a->status & xfsorted)) {
            ids.clear();
            table.clear();
            for (j = 0; j < a->arcs.nb; j++) {
                ids.push_back(a->arcs.indexes[j]);
                table[a->arcs.indexes[j]] = a->arcs.table[j];
            }
            sort(ids.begin(), ids.end(), comp);
            for (j = 0; j < a->arcs.nb; j++) {
                a->arcs.indexes[j] = ids[j];
                a->arcs.table[j] = table[ids[j]];
            }
            a->status |= xfsorted;
        }
    }
}

void State::sorting() {
    if (arcs.nb <= 1)
        return;
    
    vector<uint32_t> ids;
    hmap<uint32_t, State*> table;
    long j;
    
    for (j = 0; j < arcs.nb; j++) {
        ids.push_back(arcs.indexes[j]);
        table[arcs.indexes[j]] = arcs.table[j];
    }
    
    sort(ids.begin(), ids.end(), comp);
    for (j = 0; j < arcs.nb; j++) {
        arcs.indexes[j] = ids[j];
        arcs.table[j] = table[ids[j]];
    }
    status |= xfsorted;
}

//-----------------------------------------------------------
State* addcommon(DoubleSideAutomaton& a,long sz, long i, State** common) {
    if (*common==NULL || (i+1)<sz)
        return new State(a);

    State* c=*common;
    *common=NULL;
    return c;
}

State* State::parse(DoubleSideAutomaton& a, vector<wstring>& vs, vector<uchar>& types, long I, vector<uint32_t>& indexes,State* common) {
    if (I == vs.size())
        return this;
    
    long i=I;
    long j;
    long counter=0;
    vector<wstring> vlocal;
    vector<uchar> tlocal;
    uint32_t cw;
    State* sub = NULL;
    State* next = NULL;
    State* theend;
    long subpos=-1;
    uchar localtype=types[I];
    
    switch(localtype) {
        case 1: //char+
        case 2: //char*
        case 3: //character...
            cw = (uint32_t)a.index(vs[i][0]);
            cw |= cw << 16;
            //common is the final state to which the last arc should link (common can be NULL)
            //If we are dealing with the last character, we can use it... However, since for a "*", we need a
            //last state to jump over, we cannot use common in this case (a "*")
            if (types[i]==2)
                sub=new State(a); //we cannot use common here, we will create a new state later
            else
                sub = addcommon(a,vs.size(),i,&common);
            arcs.add(cw, sub);
            if (types[i]!=3) {
                sub->arcs.add(cw,sub);//we add a loop...
                if (types[i]==2) { //a star, we need an epsilon
                    //We can use common here
                    next=addcommon(a,vs.size(),i,&common);
                    sub->arcs.add(0,next);
                    arcs.add(0,next);
                    theend=next->parse(a,vs,types,i+1,indexes,common);
                    if (theend->isend())
                        status |= xfarcend;
                    return theend;
                }
            }
            return sub->parse(a,vs,types,i+1,indexes,common);
        case 4: //%x+
        case 5: //%x*
        case 6: //one character escaped...
            cw = (uint32_t)a.index(vs[i][1]);
            cw |= cw << 16;
            //common is the final state to which the last arc should link (common can be NULL)
            //If we are dealing with the last character, we can use it... However, since for a "*", we need a
            //last state to jump over, we cannot use common in this case (a "*")
            if (types[i]==5)
                sub=new State(a); //we cannot use common here, we will create a new state later
            else
                sub = addcommon(a,vs.size(),i,&common);
            arcs.add(cw, sub);
            if (types[i]!=6) {
                sub->arcs.add(cw,sub);//we add a loop...
                if (types[i]==5) { //a star, we need an epsilon
                    //We can use common here
                    next=addcommon(a,vs.size(),i,&common);
                    sub->arcs.add(0,next);
                    arcs.add(0,next);
                    theend=next->parse(a,vs,types,i+1,indexes,common);
                    if (theend->isend())
                        status |= xfarcend;
                    return theend;
                }
            }
            return sub->parse(a,vs,types,i+1,indexes,common);
        case 7: {//a sequence introduced with {}
            //we find its closing structure
            counter=1;
            i++;
            bool stop=false;
            uchar thetype, mint, maxt;
            //This the common end
            State* commonend=new State(a);
            
            while (i < vs.size() && !stop) {
                switch (types[i]) {
                    case 8:
                    case 9:
                    case 10:
                        stop=true;
                        break;
                    case 7:
                    case 11:
                    case 15:
                        counter=1;
                        vlocal.clear();
                        tlocal.clear();
                        thetype=types[i];
                        mint=thetype+1;
                        if (thetype==15)
                            maxt=mint;
                        else
                            maxt=thetype+3;
                        i++;
                        while (i<vs.size()) {
                            if (types[i]==thetype)
                                counter++;
                            else {
                                if (types[i]>=mint && types[i]<=maxt) {
                                    counter--;
                                    if (!counter)
                                        break;
                                }
                            }
                            vlocal.push_back(vs[i]);
                            tlocal.push_back(types[i]);
                            i++;
                        }
                        
                        //We could not find the closing parenthesis
                        if (counter)
                            return NULL;
                        
                        subpos = a.garbage.size();
                        sub = new State(a);
                        theend=sub->parse(a,vlocal,tlocal,0,indexes, commonend);
                        if (theend==NULL)
                            return NULL;
                        
                        //we copy our arcs...
                        for (j = 0; j < sub->arcs.nb; j++) {
                            cw = sub->arcs.indexes[j];
                            arcs.add(cw, sub->arcs.table[j]);
                        }
                        a.garbage[subpos] = NULL;
                        delete sub;
                        break;
                    default:
                        vlocal.clear();
                        tlocal.clear();
                        vlocal.push_back(vs[i]);
                        tlocal.push_back(types[i]);
                        subpos = a.garbage.size();
                        sub = new State(a);
                        theend=sub->parse(a,vlocal,tlocal,0,indexes,commonend);
                        if (theend==NULL)
                            return NULL;
                        //we copy our arcs...
                        arcs.resize(arcs.nb + sub->arcs.nb + 1);
                        for (j = 0; j < sub->arcs.nb; j++) {
                            cw = sub->arcs.indexes[j];
                            arcs.add(cw, sub->arcs.table[j]);
                        }
                        a.garbage[subpos] = NULL;
                        delete sub;
                }
                i++;
            }
            if (!stop)
                return NULL;
            
            if (types[i-1]!=10) {//a loop
                for (j = 0; j < arcs.nb; j++) {
                    cw = arcs.indexes[j];
                    commonend->arcs.add(cw, arcs.table[j]);
                }
                
                if (types[i-1]==9) {
                    arcs.add(0,commonend); //we jump to the common end...
                    theend= commonend->parse(a,vs,types,i,indexes,common);
                    if (theend->isend())
                        status |= xfarcend;
                    return theend;
                }
            }
            return commonend->parse(a,vs,types,i,indexes,common);
        }
        case 11: {//a sequence introduced with []
            //we find its closing structure
            counter=1;
            i++;
            while (i < vs.size()) {
                if (types[i]==11)
                    counter++;
                else {
                    if (types[i]==12 || types[i]==13 || types[i]==14) {
                        counter--;
                        if (!counter)
                            break;
                    }
                }
                vlocal.push_back(vs[i]);
                tlocal.push_back(types[i]);
                i++;
            }
            //We could not find the closing parenthesis
            if (counter)
                return NULL;
            
            subpos = a.garbage.size();
            sub = new State(a);
            if (types[i]==13) //we cannot use common here...
                theend=sub->parse(a,vlocal,tlocal,0,indexes, NULL);
            else
                theend=sub->parse(a,vlocal,tlocal,0,indexes, common);
            
            if (theend==NULL)
                return NULL;
            
            //we copy our arcs...
            for (j = 0; j < sub->arcs.nb; j++) {
                cw = sub->arcs.indexes[j];
                arcs.add(cw, sub->arcs.table[j]);
            }
            a.garbage[subpos] = NULL;
            delete sub;
            if (types[i]!=14) {
                //we have loops
                for (j = 0; j < arcs.nb; j++) {
                    cw = arcs.indexes[j];
                    theend->arcs.add(cw,arcs.table[j]);
                }
                if (types[i]==13) {//a star, we have an epsilon arc to the end
                    sub = addcommon(a,vs.size(),i,&common); //we need a common end, which can be common itself...
                    theend->arcs.add(0,sub);
                    arcs.add(0,sub);
                    theend=sub->parse(a,vs,types,i+1,indexes, common);
                    if (theend->isend())
                        status |= xfarcend;
                    return theend;
                }
            }
            return theend->parse(a,vs,types,i+1,indexes, common);
        }
        case 15: {//an optional sequence
            //we find its closing structure
            counter=1;
            i++;
            while (i < vs.size()) {
                if (types[i]==15)
                    counter++;
                else {
                    if (types[i]==16) {
                        counter--;
                        if (!counter)
                            break;
                    }
                }
                
                vlocal.push_back(vs[i]);
                tlocal.push_back(types[i]);
                i++;
            }
            //We could not find the closing parenthesis
            if (counter)
                return NULL;
            
            subpos = a.garbage.size();
            sub = new State(a);
            theend=sub->parse(a,vlocal,tlocal,0,indexes, NULL);
            if (theend==NULL)
                return NULL;
            
            //we copy our arcs...
            for (j = 0; j < sub->arcs.nb; j++) {
                cw = sub->arcs.indexes[j];
                arcs.add(cw, sub->arcs.table[j]);
            }
            a.garbage[subpos] = NULL;
            delete sub;
            
            //We need to jump over this end, to avoid taking into account local arcs...
            sub = addcommon(a,vs.size(),i,&common); //we need a common end, which can be common itself...
            theend->arcs.add(0,sub);
            arcs.add(0,sub);
            
            theend=sub->parse(a,vs,types,i+1,indexes,common);
            if (theend->isend())
                status |= xfarcend;
            return theend;
        }
        case 20: //a x-x+
        case 21: //a x-x*
        case 22: {//a x-x
            wstring w=vs[i];
            if (w[0]>w[2])
                return NULL;
            //common is the final state to which the last arc should link (common can be NULL)
            //If we are dealing with the last character, we can use it... However, since for a "*", we need a
            //last state to jump over, we cannot use common in this case (a "*")
            if (types[i]==21)
                sub=new State(a);
            else
                sub = addcommon(a,vs.size(),i,&common);
            
            for (j=w[0];j<=w[2];j++) {
                cw = a.index(j);
                cw |= cw << 16;
                arcs[cw]=sub;
                if (types[i]!=22) // we add a loop
                    sub->arcs.add(cw,sub);
            }
            
            if (types[i]==21) { //a star, we need a common exit state
                //here eventually we can use common...
                next=addcommon(a,vs.size(),i,&common);
                arcs.add(0,next);
                sub->arcs.add(0,next);
                theend=next->parse(a,vs,types,i+1,indexes,common);
                if (theend->isend())
                    status |= xfarcend;
                return theend;
            }
            return sub->parse(a,vs,types,i+1,indexes,common);
        }
        case 25: //a $xxx+
        case 26: //a $xxx*
        case 27: {//a $xxx
            sub=this; //Here we need to chain each character within the automaton with the previous one...
            uint32_t cwfirst=0;
            long sz=vs[i].size();
            for (j=1;j<sz;j++) {
                //common is the final state to which the last arc should link (common can be NULL)
                //If we are dealing with the last character, we can use it... However, since for a "*", we need a
                //last state to jump over, we cannot use common in this case (a "*")
                if ((j+1)==sz && types[i]!=26) //then we can use the common
                    next = addcommon(a,vs.size(),i,&common);
                else
                    next=new State(a);
                
                cw = a.index((uint32_t)vs[i][j]);
                cw |= cw << 16;
                if (j==1)
                    cwfirst=cw;
                sub->arcs[cw]=next;
                sub=next;
            }
            
            if (types[i]!=27) {//a loop
                //First we loop between our next and our current state
                sub->arcs.add(cwfirst,arcs[cwfirst]);
                if (types[i]==26) { //a star, we need a common exit state
                    next = addcommon(a,vs.size(),i,&common);
                    arcs.add(0,next);
                    sub->arcs.add(0,next);
                    theend=next->parse(a,vs,types,i+1,indexes,common);
                    if (theend->isend())
                        status |= xfarcend;
                    return theend;
                }
            }
            return sub->parse(a,vs,types,i+1,indexes, common);
        }
        case 30: {//the index...
            j=convertinteger(vs[i].substr(1,vs[i].size()));
            cw = indexes[j-1];
            cw <<= 16;
            sub = addcommon(a,vs.size(),i,&common);
            sub->status|=xfarcend;
            arcs.add(cw, sub);
            status |= xfepsilonlower | xfarcend;
            return parse(a, vs, types, i+1, indexes,common);
        }
        case 40: //?+
        case 41: //?*
        case 42: //?...
            cw = a.index((uint32_t)0xFFFE);
            cw |= cw << 16;
            //common is the final state to which the last arc should link (common can be NULL)
            //If we are dealing with the last character, we can use it... However, since for a "*", we need a
            //last state to jump over, we cannot use common in this case (a "*")
            if (types[i]==2)
                sub=new State(a); //we cannot use common here, we will create a new state later
            else
                sub = addcommon(a,vs.size(),i,&common);
            arcs.add(cw, sub);
            if (types[i]!=42) {
                sub->arcs.add(cw,sub);//we add a loop...
                if (types[i]==41) { //a star, we need an epsilon
                    //We can use common here
                    next=addcommon(a,vs.size(),i,&common);
                    sub->arcs.add(0,next);
                    arcs.add(0,next);
                    theend=next->parse(a,vs,types,i+1,indexes,common);
                    if (theend->isend())
                        status |= xfarcend;
                    return theend;
                }
            }
            return sub->parse(a,vs,types,i+1,indexes,common);
    }
    return NULL;
}


bool State::parse(DoubleSideAutomaton& a, agnostring& expression, vector<uint32_t>& indexes) {
    static x_wdoubleautomaton xr;
    vector<wstring> vs;
    vector<uchar> types;
    wstring w=expression.utf8tounicode();

    xr.tokenize(w,vs,types);
    
	long i = 0;
    
    State* xf = new State(a);
	if (xf->parse(a, vs, types, i, indexes, NULL)==NULL)
		return false;

	status |= xf->status;

    vector<State*> marked;
    mergein(xf,a,marked);
    a.finalize = false;

    for (i = 0; i < marked.size(); i++)
        marked[i]->mark = false;

    
	return true;
}

bool State::parse(DoubleSideAutomaton& a, wstring& w, vector<uint32_t>& indexes) {
    static x_wdoubleautomaton xr;
    vector<wstring> vs;
    vector<uchar> types;
    
    xr.tokenize(w,vs,types);
    
    long i = 0;
    
    State* xf = new State(a);
    if (xf->parse(a, vs, types, i, indexes, NULL)==NULL)
        return false;
        
    vector<State*> marked;
    mergein(xf,a,marked);
    a.finalize = false;
    
    for (i = 0; i < marked.size(); i++)
        marked[i]->mark = false;

    return true;
}

void State::mergein(State* xf, DoubleSideAutomaton& a, vector<State*>& marked) {
    if (mark)
        return;

    mark=true;
    marked.push_back(this);
    
    short last;
    vector<long> unknown;
    long i;
    uint32_t u;

    status |= xf->status;

    for (i = 0; i < xf->arcs.nb; i++) {
        u = xf->arcs.indexes[i];
        if (arcs.find(u, last)) {
            arcs.get(u, last)->mergein(xf->arcs.table[i], a, marked);
        }
        else
            unknown.push_back(i);
    }

    last = unknown.size();
    if (last) {
        arcs.resize(arcs.nb + last + 1);
            //we are at the limit where the indexes are no longer common...
        for (long j = 0; j < last; j++) {
            i = unknown[j];
            //We then copy them...
            arcs.add(xf->arcs.indexes[i], xf->arcs.table[i]);
        }
        sorting();
    }    
}

void State::merging(State* xf, DoubleSideAutomaton& a) {
    if (xf->mark)
        return;
    xf->mark=true;
    xf->status |= xfdelete;
    
    bool found=true;
    short last;
    long i=0;
    vector<long> absents;
    for (; i < xf->arcs.nb; i++) {
        if (arcs.find(xf->arcs.indexes[i], last))
            arcs.get(xf->arcs.indexes[i], last)->merging(xf->arcs.table[i],a);
        else {
            absents.push_back(i);
            found=false;
        }
    }
    
    //we are at the limit where the indexes are no longer common...
    if (!found) {
        State* xe;
        //We then copy them...
        long u;
        for (i=0; i< absents.size(); i++) {
            u = absents[i];
            xe = xf->arcs.table[u];
            arcs.add(xf->arcs.indexes[u], xe);
        }
    }
}


void DoubleSideAutomaton::merge(DoubleSideAutomaton* a) {
    if (a==NULL)
        return;
    //The first step is normalize the alphabet of a...
    hmap<unsigned short, unsigned short> conversion;
    //We check each code to see if it appears in a
    bool diff=false;
    for (auto& it: alphabet) {
        if (a->alphabet.find(it.first) != a->alphabet.end()) {
            //This character in our automaton occurs in "a" alphabet...
            conversion[a->alphabet[it.first]] = it.second;
            if (a->alphabet[it.first] != it.second) {//if the code is different, we keep it
                diff=true;
            }
        }
    }

    for (auto& it: a->alphabet) {
        if (alphabet.find(it.first) == alphabet.end()) {
            if (ialphabet.check(it.second)) {
                //This is already a code in the current automaton, we need to create a new one
                long code = alphabet.size() + 1;
                alphabet[it.first] = code;
                ialphabet[code] = it.first;
                conversion[it.second] = code;
            }
            else {
                alphabet[it.first] = it.second;
                ialphabet[it.second] = it.first;
                conversion[it.second] = it.second;
            }
        }
    }
    
    if (diff) {
        //we need to keep the other conversions
        
        uint32_t lower;
        uint32_t upper;
        //We need know to adapt the arcs in a to this new alphabet..
        State* x;
        for (long i=0; i<a->garbage.size(); i++) {
            x = a->garbage[i];
            for (long j=0; j< x->arcs.sz; j++) {
                upper = x->arcs.code(j);
                lower = x->arcs.character(j);
                lower = conversion[lower];
                upper = conversion[upper];
                upper <<= 16;
                upper |= lower;
                x->arcs.indexes[j] = upper;
            }
        }
    }

    for (auto& u : a->multis) {
        if (multis.find(u.first) == multis.end()) {
            multis[u.first] = u.second;
            sortedmultis.push_back(u.first);
            featurecode[u.second] = u.second;
        }
    }

    hash_bin<unsigned short, string>::iterator ia;
    for (ia = a->ialphabet.begin(); ia != a->ialphabet.end(); ia++) {
        if (ialphabet.find(ia->first) == ialphabet.end())
            ialphabet[ia->first] = ia->second;
    }

    clearmarks();
    a->clearmarks();

    start.merging(&a->start, *this);
    State* xe;
    for (long i = 1; i < a->garbage.size(); i++) {
        xe = a->garbage[i];
        if (xe != NULL) {
            xe->id = (uint32_t)garbage.size();
            garbage.push_back(xe);
            a->garbage[i] = NULL;
        }
    }
}

void State::regulars(DoubleSideAutomaton& a) {
	//Our features
	vector<uint32_t> indexes;

	string e = "\t+Dig+Card";
	indexes.push_back((uint32_t)a.index(e));
	e = "\t+Dig+Dec";
	indexes.push_back((uint32_t)a.index(e));
	e = "\t+Exp+Dec";
	indexes.push_back((uint32_t)a.index(e));
	e = "\t+Dig+Ord";
	indexes.push_back((uint32_t)a.index(e));
	e = "\t+Dig+Rom";
	indexes.push_back((uint32_t)a.index(e));

	agnostring expression("({+ -}) 0-9+ !1 (. 0-9+ !2 ({e E} ( {+ -} ) 0-9+ !3))");
	parse(a, expression, indexes);

    expression = "{D M C L X V I}+!5";
	parse(a, expression, indexes);

	expression = "{$1st $2nd $3rd}!4";
	parse(a, expression, indexes);

	expression = "[0-9][0-9]*$th!4";
	parse(a, expression, indexes);
}

//----------------------------------TOKENIZE AND MORPHO-ANALYSYS IN ONE SINGE STEP----------------
bool DoubleSideAutomaton::process(charRead& w, vector<string>& readings, bool option, long threshold, short flags) {
	if (!finalize) {
		fillencoding(true);
		start.shuffle();
	}

	string s;
	w.init(w.w);
	w.encoding_table = encoding_table;
    FstCompanion* f = &Companion;
    f->set(this, readings, w.addfeatures, threshold);

	if (start.vprocess(w, f, threshold, flags)) {
        long i;


        agnostring surface;
        
        if (!w.addfeatures) {
            
            if (readings.size() > 1) {
                //we only keep the longest match
                for (i = readings.size() - 1; i >= 0; i--) {
                    if (w.bends[i] < w.bend)
                        readings.erase(readings.begin() + i);
                }
            }

            surface = w.extract();
            if (surface.trim() == "")
                return false;

            hmap<string, bool> adding;
            for (i = 0; i < readings.size(); i++)
                adding[readings[i]] = true;
            
            readings.clear();
            readings.push_back(surface);
            for (auto& a : adding) {
                if (a.first != surface)
                    readings.push_back(a.first);
            }

            w.setpos(w.bend, w.cend);
            return true;
        }
        
        if (readings.size() > 2) {
            //we only keep the longest match
            long mx = (readings.size()>>1)-1;
            for (i = readings.size() - 2; i >= 0; i-=2) {
                if (w.bends[mx--] < w.bend) {
                    readings.erase(readings.begin() + i + 1);
                    readings.erase(readings.begin() + i);
                }
            }
        }

		w.setpos(w.bend, w.cend);
        surface = w.extract();
        if (surface.trim() == "")
            return false;
        
        if (!readings.size()) {
            readings.push_back(surface);
            readings.push_back(surface);
            if (w.addoffsets)
                readings.push_back(w.offsets());
            else
                readings.push_back("");
        }
        else {
            surface = surface.latintoutf8(encoding_table);
            if (w.addoffsets) {
                string off = w.offsets();
                for (i = 0; i < readings.size(); i+=2)
                    readings[i+1] = off+readings[i+1];
            }
            readings.insert(readings.begin(), surface);
        }
		return true;
	}


	//No value found....
	w.setpos(w.bbegin, w.cbegin);
	w.eset(w.bbegin, w.cbegin);

    string feat;
    
	TRANSCHAR cr = w.nextcode();
	if (c_is_punctuation(cr)) {
		w.eset(w.w);
		s = w.extract();
        
        feat="";
        
        if (!w.addfeatures) {
            readings.push_back(s);
            return true;
        }
        
        if (w.addoffsets) {
            string off = w.offsets();
            feat=off;
            feat+="+PUNCT";
        }
        else
            feat="+PUNCT";

        readings.push_back(s);
        readings.push_back(s);
        readings.push_back(feat);
        
        return true;
	}
    
    if (w.end())
        return false;
    
    if (isnotaction(action_longest_match)) {
        while (!w.end() && !c_is_space(cr) && !c_is_punctuation(cr)) {
            w.eset(w.w);
            cr = w.nextcode();
        }
        if (!w.end())
            w.setpos(w.bend, w.cend);
    }
    else
        w.eset(w.w);

    s = w.extract();
    if (s == "")
        return false;
    
    if (!w.addfeatures) {
        readings.push_back(s);
        return true;
    }

    feat="";
    if (w.addoffsets) {
        string off = w.offsets();
        feat=off;
        feat+="+Unknown+NOUN";
    }
    else
        feat="+Unknown+NOUN";
    
    readings.push_back(s);
    readings.push_back(s);
    readings.push_back(feat);

    return true;
}

bool State::editdistance(charRead& w, bool punct, FstCompanion* f,long threshold, short flags) {
    long cw=0, u, bpos=0, cpos=0, ubpos=0, ucpos=0, i, prev;
	TRANSCHAR cr = 0;
    bool found = false;
    
    char rsgate;
    
    w.getpos(ubpos,ucpos);
        //cr is the character at position ubpos, ucpos
    cr = w.nextcode();
    w.getpos(bpos,cpos);
    //if we have both flags and threshold
    //we have four cases: action_change, action_delete, action_insert, action_switch
    
        //We skip one character in the string...
    if (isaction(action_delete)) {
        if (process(w, punct, f, threshold-1, flags))
            found = true;
        w.setpos(bpos,cpos);
    }
    
        //There could be a repetition, in that case, we do not touch the threshold
    if (isaction(action_repetition)) {
		TRANSCHAR c;
        long nb = 0;
        long nbpos = bpos, ncpos = cpos;
        while (!w.end()) {
            w.getpos(nbpos, ncpos);
            c = w.nextcode();
            if (c != cr)
                break;
            nb++;
        }
        
        if (nb) {
            w.setpos(nbpos,ncpos);
            if (process(w, punct, f, threshold-1, flags))
                found = true;
        }
        w.setpos(bpos,cpos);
    }
    
    if (isaction(action_switch) && cpos >= 2) {
            //we switch the two characters...
        w.setpos(ubpos, ucpos);
        w.switchcurrent();
        
        if (process(w, punct, f, threshold-1, flags))
            found = true;
        
        w.setpos(ubpos, ucpos);
        w.switchcurrent();
        w.setpos(bpos, cpos);
    }
    
    if (flags & action_fullall) {
            //in that case, we do not try our current character, we accept any others
        for (i = 0; i < arcs.nb; i++) {
            u = arcs.indexes[i] & 0xFFFF;
            if (!u)
                continue;
            
            rsgate = f->isgate(u);
            if (rsgate == 2)
                continue;
            
            if (isaction(action_vowel) && c_is_vowel(cr)) {
                if (u == cw || u == 1) //already processed, no need to go further...
                    continue;
                
                    //we must check if the upper layer character (u here) is a vowel
                    //In this case, we need the current upper character to be a vowel...
				TRANSCHAR cu = 0;
                if (f->a->decoding.check(u))
                    cu = f->a->decoding.get(u);
                
                    //This is a comparison, which compare deaccentated vowels...
                if (cu && compare_vowel(cr,cu)) {//then it is ok, we can explore the rest...
                    if (isaction(action_surface))
                        u = arcs.character(i);
                    else
                        u = arcs.code(i);
                    if (u) {
                        if (u == 1)
                            prev = f->add(cr);
                        else {
                            if (rsgate)
                                prev = f->pos;
                            else
                                prev =  f->addforparse(u);
                        }
                        
                            //We do not touch the threshold in this case...
                        if (arcs.found(i)->process(w, punct, f, threshold-1, flags))
                            found = true;
                        
                        f->resetfeatures(prev);
                        f->resetgates(u, rsgate);
                    }
                }
                w.setpos(bpos, cpos);
            }
            
            if (isaction(action_nocase)) {
                if (u == cw || u == 1) //already processed, no need to go further...
                    continue;
                
                    //we must check if the upper layer character (u here) is a vowel
                    //In this case, we need the current upper character to be a vowel...
				TRANSCHAR cu = 0;
                if (f->a->decoding.check(u))
                    cu = f->a->decoding.get(u);
                
                    //This is a comparison, which compare deaccentated vowels...
                if (cu && c_to_lower(cu) == c_to_lower(cr)) {//then it is ok, we can explore the rest...
                    if (isaction(action_surface))
                        u = arcs.character(i);
                    else
                        u = arcs.code(i);
                    
                    if (u) {
                        if (u == 1)
                            prev = f->add(cr);
                        else {
                            if (rsgate)
                                prev = f->pos;
                            else
                                prev =  f->addforparse(u);
                        }
                        
                            //We do not touch the threshold in this case...
                        if (arcs.found(i)->process(w, punct, f, threshold-1, flags))
                            found = true;
                        
                        f->resetfeatures(prev);
                        f->resetgates(u, rsgate);
                    }
                }
                w.setpos(bpos, cpos);
            }
            
            if (isaction(action_insert)) {
                
                if (isaction(action_surface))
                    u = arcs.character(i);
                else
                    u = arcs.code(i);
                
                if (u) {
                    if (u == 1)
                        prev = f->add(cr);
                    else {
                        if (rsgate)
                            prev = f->pos;
                        else
                            prev =  f->addforparse(u);
                    }
                    
                    w.setpos(ubpos, ucpos);
                    
                    if (arcs.found(i)->process(w, punct, f, threshold-1, flags))
                        found = true;
                    
                    f->resetfeatures(prev);
                    f->resetgates(u, rsgate);
                }
                w.setpos(bpos, cpos);
            }
            
            if (isaction(action_change)) {
                if (u == cw)
                    continue;
                
                if (isaction(action_surface))
                    u = arcs.character(i);
                else
                    u = arcs.code(i);
                
                
                if (u) {
                    if (u == 1)
                        prev = f->add(cr);
                    else {
                        if (rsgate)
                            prev = f->pos;
                        else
                            prev =  f->addforparse(u);
                    }
                    
                    if (arcs.found(i)->process(w, punct, f, threshold-1, flags))
                        found = true;
                    
                    f->resetfeatures(prev);
                    f->resetgates(u, rsgate);
                }
                w.setpos(bpos, cpos);
            }
        }
    }
    return found;
}

//This method is called from vprocess...
//Basically, vprocess detects the beginning of a token and process tries to detect its full range within the string.
bool State::process(charRead& w, bool punct, FstCompanion* f,long threshold, short flags) {
    long cw=0, u, bpos=0, cpos=0, ubpos=0, ucpos=0, i, prev = 0;
	TRANSCHAR cr = 0;
    bool endtoken = false, found = false;

    char rsgate;
    
	if (w.end())
		endtoken = true;
	else {
        w.getpos(ubpos,ucpos);
        //cr is the character at position ubpos, ucpos
		cr = w.nextcode();
        w.getpos(bpos,cpos);

		cw = f->a->code(cr);

		if (c_is_space(cr) || c_is_punctuation(cr) || punct) {
			//we need then to check if we can carry on with arcs...
			if (!f->empty()) {
				if (!cw && c_is_punctuation(cr)) {
					//In this case, we try to convert it to a knowledgable one
					cr = c_unicode_to_latin(cr);
					if (cr != 0)
						cw = f->a->code(cr);
                    if (cw==0) {//then it means that the punctuation is not part of the vocabulary...
                        //which is actually ok... In that case, we transform it into a space
                        cw= f->a->code(32); //in order to finalize the process...
                    }
				}

				if (cw) {
					i = -1;
					while (arcs.checkup(cw, i)) {
						if (!(arcs.indexes[i] & 0xFFFF))
							w.setpos(ubpos, ucpos);

						u = arcs.code(i);
                        rsgate = f->isgate(u);
                        if (rsgate == 2)
                            continue;

						if (u) {
                            if (u == 1)
                                prev = f->add(cr);
                            else {
                                if (!rsgate)
                                    prev = f->addforparse(u);
                                else
                                    prev = f->pos;
                            }
							if (arcs.found(i)->process(w, punct, f, threshold, flags))
								found = true;
                            f->resetfeatures(prev);
                            f->resetgates(u, rsgate);
						}
						else {
							if (arcs.found(i)->process(w, punct, f, threshold, flags))
								found = true;
						}
						w.setpos(bpos, cpos);
					}

				}

                if (flags && threshold) {
                    //if we have both flags and threshold
                    //we have four cases: action_change, action_delete, action_insert, action_switch
                    //We need to be sure that we are positionned on the right character...
                    w.setpos(ubpos,ucpos);
                    if (editdistance(w, punct,f, threshold, flags))
                        return true;
                }
                
                //In this case, the character does not belong to the string
                //We try to analyze it...
                w.setpos(ubpos, ucpos);
                endtoken = true;
                
            }
			else {
				if (c_is_punctuation(cr))
					punct = true;
			}
		}
	}

    //an endtoken is set to true if we have met a space, a punctuation or the end of the string...
	if (endtoken) {
		if (status&xfarcend || status&xfepsilonlower) {
			if (!finals(f,threshold))
				return false;
			w.eset(w.w);
			return true;
		}
		return found;
	}

	if (!arcs.nb)
		return found;

	i = -1;
	if (cw) {
		while (arcs.checkup(cw, i)) {
			if (!(arcs.indexes[i] & 0xFFFF))
				w.setpos(ubpos, ucpos);

			u = arcs.code(i);
            rsgate = f->isgate(u);
            if (rsgate == 2)
                continue;

			if (u) {
                if (u == 1)
                    prev = f->add(cr);
                else {
                    if (!rsgate)
                        prev = f->addforparse(u);
                    else
                        prev = f->pos;
                }
                if (arcs.found(i)->process(w, punct, f, threshold, flags))
					found = true;
                
                f->resetfeatures(prev);
                f->resetgates(u, rsgate);
            }
			else {
				if (arcs.found(i)->process(w, punct, f, threshold, flags))
					found = true;
			}
			w.setpos(bpos, cpos);
		}

        if (!found && isaction(action_longest_match) && f->pos) {
            i = -1;
            State* ac;
            while (arcs.checkup(cw, i)) {
                if (!(arcs.indexes[i] & 0xFFFF))
                    continue;
                
                u = arcs.code(i);
                if (u) {
                    if (u == 1)
                        prev = f->add(cr);
                    else
                        prev = f->addforparse(u);
                }
                ac = arcs.found(i);
                if (ac->status&xfarcend || ac->status&xfepsilonlower) {
                    if (ac->finals(f,threshold)) {
                        w.eset(w.w);
                        return true;
                    }
                    f->resetfeatures(prev);
                }
                w.setpos(bpos, cpos);
            }
        }
        
		if (f->a->normalize && c_is_upper(cr)) {
            long th = threshold;
            if (th)
                --th;
            
			uint32_t cwl = f->a->code(c_to_lower(cr));
			i = -1;
			while (arcs.checkup(cwl, i)) {
				//the espsilon arcs have already been covered above...
				if (!(arcs.indexes[i] & 0xFFFF))
					continue;

				u = arcs.code(i);
                rsgate = f->isgate(u);
                if (rsgate == 2)
                    continue;

                if (u) {
                    if (u == 1)
                        prev = f->add(c_to_lower(cr));
                    else {
                        if (!rsgate)
                            prev = f->addforparse(u);
                        else
                            prev = f->pos;
                    }

					if (arcs.found(i)->process(w, punct, f, th, flags))
						found = true;
                    
                    f->resetfeatures(prev);
                    f->resetgates(u, rsgate);
                }
				else {
					if (arcs.found(i)->process(w, punct, f, th, flags))
						found = true;
				}
				w.setpos(bpos, cpos);
			}
		}
        
        if (flags && threshold) {
            w.setpos(ubpos,ucpos);
            bool subfound = editdistance(w, punct,f, threshold, flags);
            if (subfound)
                found = true;
        }
	}
	else {
		if (punct) {
			cr = c_unicode_to_latin(cr);
			cw = f->a->code(cr);
			if (cw)
				return found;

			i = -1;
			while (arcs.checkup(cw, i)) {
				if (!(arcs.indexes[i] & 0xFFFF))
					w.setpos(ubpos, ucpos);

				u = arcs.code(i);
                rsgate = f->isgate(u);
                if (rsgate == 2)
                    continue;

                if (u) {
                    if (u == 1)
                        prev = f->add(cr);
                    else {
                        if (!rsgate)
                            prev = f->addforparse(u);
                        else
                            prev = f->pos;
                    }

					if (arcs.found(i)->process(w, punct, f, threshold, flags))
						found = true;
                    f->resetfeatures(prev);
                    f->resetgates(u, rsgate);
				}
				else {
					if (arcs.found(i)->process(w, punct, f, threshold, flags))
						found = true;
				}
				w.setpos(bpos, cpos);
			}
		}
	}

	return found;
}

//vprocess is used to detect the beginning of a sequence of characters and to apply "process" to analyse this sequence up to the max against the transducer
bool State_Vectorized::vprocess(charRead& w, FstCompanion* f, long threshold, short flags) {
	long cw=0, u, ubpos=0, ucpos=0, bpos=0, cpos=0;
	TRANSCHAR cr = 0;
    bool endtoken = false, found = false, punct = false;

    //A sequence of characters is either ending at the end of the string or bounded with a space or a punctuation
	if (w.end())
		endtoken = true;
	else {
        w.getpos(ubpos,ucpos);
        cr = w.nextcode();
        w.getpos(bpos,cpos);

		if (c_is_space(cr)) {
			w.eset(bpos, cpos);
			return true;
		}

		cw = f->a->code(cr);

		if (c_is_punctuation(cr))
			punct = true;
	}

    //If we have reached an end within the transducer, we check if it can be an actual word in the transducer...
	if (endtoken) {
		if (status&xfarcend || status&xfepsilonlower) {
			if (!finals(f,threshold))
				return false;
			w.eset(w.w);
			return true;
		}
		return false;
	}


	long i, ia, prev;
    uchar rsgate;
	if (cw) {
		//then we are in the vectorized zone...
		//Then we use the code as an entry point to the arcs...
		if (f->a->start.arcsv.check(cw)) {
            vector<long>& thearcs = f->a->start.arcsv[cw];
			for (i = 0; i < thearcs.size(); i++) {
				ia = thearcs[i];
				u = arcs.code(ia);
                rsgate = f->isgate(u);
                if (rsgate == 2)
                    continue;

				if (u) {
                    if (u == 1)
                        prev = f->add(cr);
                    else {
                        if (!rsgate)
                            prev = f->addforparse(u);
                        else
                            prev = f->pos;
                    }
					if (arcs.found(ia)->process(w, punct, f, threshold, flags))
						found = true;
                    f->resetfeatures(prev);
                    f->resetgates(u, rsgate);
				}
				else {
					if (arcs.found(ia)->process(w, punct, f, threshold, flags))
						found = true;
				}
				w.setpos(bpos, cpos);
			}
		}

		if (f->a->normalize && c_is_upper(cr)) {
			uint32_t cwl = f->a->code(c_to_lower(cr));
			if (f->a->start.arcsv.check(cwl)) {
                vector<long>& thearcs = f->a->start.arcsv[cwl];
				for (i = 0; i < thearcs.size(); i++) {
					ia = thearcs[i];
					u = arcs.code(ia);
                    rsgate = f->isgate(u);
                    if (rsgate == 2)
                        continue;
					if (u) {
                        if (u == 1)
                            prev = f->add(cr);
                        else {
                            if (!rsgate)
                                prev = f->addforparse(u);
                            else
                                prev = f->pos;
                        }

						if (arcs.found(ia)->process(w, punct, f, threshold, flags))
							found = true;
                        f->resetfeatures(prev);
                        f->resetgates(u, rsgate);
					}
					else {
						if (arcs.found(ia)->process(w, punct, f, threshold, flags))
							found = true;
					}
					w.setpos(bpos, cpos);
				}
			}
		}
        
        if (flags && threshold) {
            w.setpos(ubpos,ucpos);
            bool subfound = editdistance(w, punct,f, threshold, flags);
            if (subfound)
                found = true;
        }
	}
	else {
		if (punct) {
			cr = c_unicode_to_latin(cr);
			cw = f->a->code(cr);
			if (cw) {
				if (f->a->start.arcsv.check(cw)) {
                    vector<long>& thearcs = f->a->start.arcsv[cw];
					for (i = 0; i < thearcs.size(); i++) {
						ia = thearcs[i];
						u = arcs.code(ia);
                        rsgate = f->isgate(u);
                        if (rsgate == 2)
                            continue;
						if (u) {
                            if (u == 1)
                                prev = f->add(cr);
                            else {
                                if (!rsgate)
                                    prev = f->addforparse(u);
                                else
                                    prev = f->pos;
                            }

                            if (arcs.found(ia)->process(w, punct, f, threshold, flags))
                                found = true;
                            f->resetfeatures(prev);
                            f->resetgates(u, rsgate);
                       }
						else {
							if (arcs.found(ia)->process(w, punct, f, threshold, flags))
								found = true;
						}
						w.setpos(bpos, cpos);
					}
				}
			}
		}
	}

    //we check for an epsilon...
	if (f->a->start.arcsv.check(0)) {
		w.setpos(ubpos, ucpos);
        vector<long>& thearcs = f->a->start.arcsv[0];
		for (i = 0; i < thearcs.size(); i++) {
			ia = thearcs[i];
			u = arcs.code(ia);
            rsgate = f->isgate(u);
            if (rsgate == 2)
                continue;
			if (u) {
                if (u == 1)
                    prev = f->add(cr);
                else {
                    if (!rsgate)
                        prev = f->addforparse(u);
                    else
                        prev = f->pos;
                }

                if (arcs.found(ia)->process(w, punct, f, threshold, flags))
                    found = true;
                f->resetfeatures(prev);
                f->resetgates(u, rsgate);
			}
			else {
				if (arcs.found(ia)->process(w, punct, f, threshold, flags))
					found = true;
			}
			w.setpos(ubpos, ucpos);
		}
	}

	return found;
}


bool State::finals(FstCompanion* f, long threshold) {
    
    long prev;
    bool found = false;
    if (status&xfepsilonlower) {
        long i = -1;
        char rsgate;
        uint32_t u;
        while (arcs.checkup(0, i)) {
            u = arcs.code(i);
            rsgate = f->isgate(u);
            if (rsgate == 2)
                continue;
            if (!rsgate)
                prev = f->addforparse(u);
            else
                prev = f->pos;

            if (arcs.found(i)->finals(f,threshold))
                found = true;
            f->resetfeatures(prev);
            f->resetgates(u, rsgate);
        }
    }

    if (status&xfarcend) {
        f->storingfeatures(threshold);
        return true;
    }
    
    return found;
}
//----------------------------------------------------------------------------------------
bool DoubleSideAutomaton::up(wstring& w, vector<string>& res, long threshold, short flags) {
    if (!finalize)
        fillencoding(true);

    FstCompanion* f = &Companion;
    res.clear();
    f->set(w,this,res);
    
    return start.up(f, 0, threshold, flags);
}

bool State::up(FstCompanion* f, long pos, long threshold, short flags) {

    long i = -1;
    long prev;
    char rsgate;
    uint32_t u;
    
    if (f->sz == pos) {
        if (status&xfepsilonlower) {
            for (i = 0; i < arcs.nb; i++) {
                //It should not be a transducer arc....
                u = arcs.code(i);
                rsgate = f->isgate(u);
                if (rsgate == 2)
                    continue;

                if (!rsgate && arcs.indexes[i] & 0xFFFF)
                    continue;
                
                if (isaction(action_surface))
                    prev = f->addalphabet(arcs.character(i));
                else {
                    if (!rsgate)
                        prev = f->addalphabet(u);
                    else
                        prev = f->pos;
                }
                arcs.found(i)->up(f, pos, threshold, flags);
                f->reset(prev);
                f->resetgates(u, rsgate);
            }
        }
        
        if (status&xfarcend) {
            f->storing();
            return true;
        }
		return false;
	}

	long current;
    
	TRANSCHAR cr = f->w[pos];
	uint32_t cw = f->a->code(cr);

	bool found = false;
    
    while (arcs.checkup(cw, i)) {
    
        rsgate = f->isgate(arcs.code(i));
        if (rsgate == 2)
            continue;
        
        if (rsgate || !(arcs.indexes[i] & 0xFFFF))
            current = pos; //this is an epsilon...
        else
            current = pos + 1;
        
        if (isaction(action_surface))
            u = arcs.character(i);
        else
            u = arcs.code(i);
                
        //We check with the upper layer and then extract the lower layer
        //u is the lower layer character...
		if (u) {
            if (u == 1)
                prev = f->add(cr);
            else {
                if (!rsgate)
                    prev = f->addalphabet(u);
                else
                    prev = f->pos;
            }
            
            if (arcs.found(i)->up(f, current, threshold, flags))
                found = true;
            
            f->reset(prev);
		}
		else {
			if (arcs.found(i)->up(f, current, threshold, flags))
				found = true;
		}

        f->resetgates(u, rsgate);
	}

	if (flags && threshold) {
		//if we have both flags and threshold
		//we have four cases: action_change, action_delete, action_insert, action_switch
		if (pos == 0 && isnotaction(action_first))
			return found;

        
        //We skip one character in the string...
		if (isaction(action_delete)) {
			if (up(f, pos + 1,threshold - 1, flags))
				found = true;
		}

        //There could be a repetition, in that case, we do not touch the threshold
        if (isaction(action_repetition)) {
            long nb = pos+1;
            while (nb < f->sz && f->w[pos]==f->w[nb])
                nb++;
            if ((nb-pos) >= 2) {
                if (up(f, nb-1 , threshold - 1, flags))
                    found = true;
            }
        }
        
		if (isaction(action_switch) && (pos + 1) < f->sz) {
			//we switch the two characters...
			TRANSCHAR c = f->w[pos];
			f->w[pos] = f->w[pos + 1];
			f->w[pos + 1] = c;

			if (up(f, pos, threshold - 1, flags))
				found = true;

			f->w[pos + 1] = f->w[pos];
			f->w[pos] = c;
		}

        if (isaction(action_nocase)) {
            bool doit=false;
            if (c_is_upper(cr)) {
                f->w[pos] = c_to_lower(cr);
                doit=true;
            }
            else
                if (c_is_lower(cr)) {
                    doit = true;
                    f->w[pos] = c_to_upper(cr);
                }
            
            if (doit) {
                if (up(f, pos, threshold - 1, flags))
                    found = true;
                f->w[pos] = cr;
            }
        }

		if (flags & action_suball) {
			//in that case, we do not try our current character, we accept any others
			for (i = 0; i < arcs.nb; i++) {
				u = arcs.indexes[i] & 0xFFFF;
				if (!u)
					continue;

                rsgate = f->isgate(u);
                if (rsgate == 2)
                    continue;

                if (isaction(action_vowel) && c_is_vowel(cr)) {
                    if (u == cw || u == 1) //already processed, no need to go further...
                        continue;
                    
                    //we must check if the upper layer character (u here) is a vowel
                    //In this case, we need the current upper character to be a vowel...
					TRANSCHAR cu = 0;
                    if (f->a->decoding.check(u))
                        cu = f->a->decoding.get(u);
                    
                    //This is a comparison, which compare deaccentated vowels...
                    if (cu && compare_vowel(cr,cu)) {//then it is ok, we can explore the rest...
                        if (isaction(action_surface))
                            u = arcs.character(i);
                        else
                            u = arcs.code(i);
                        if (u) {
                            if (u == 1)
                                prev = f->add(cr);
                            else {
                                if (rsgate)
                                    prev = f->pos;
                                else
                                    prev =  f->addalphabet(u);
                            }
                            
                            //We do not touch the threshold in this case...
                            if (arcs.found(i)->up(f, pos + 1, threshold-1, flags))
                                found = true;
                            
                            f->reset(prev);
                            f->resetgates(u, rsgate);
                        }
                    }
                }

				if (isaction(action_insert)) {
                    if (isaction(action_surface))
                        u = arcs.character(i);
                    else
                        u = arcs.code(i);
					if (u) {
                        if (u == 1)
                            prev = f->add(cr);
                        else {
                            if (rsgate)
                                prev = f->pos;
                            else
                                prev =  f->addalphabet(u);
                        }

                        if (arcs.found(i)->up(f, pos, threshold - 1, flags))
                            found = true;
                        f->reset(prev);
                        f->resetgates(u, rsgate);
					}
				}

				if (isaction(action_change)) {
					if (u == cw)
						continue;

                    if (isaction(action_surface))
                        u = arcs.character(i);
                    else
                        u = arcs.code(i);
					if (u) {
                        if (u == 1)
                            prev = f->add(cr);
                        else {
                            if (rsgate)
                                prev = f->pos;
                            else
                                prev =  f->addalphabet(u);
                        }

                        if (arcs.found(i)->up(f, pos + 1, threshold - 1, flags))
                            found = true;
                        
                        f->reset(prev);
                        f->resetgates(u, rsgate);
					}
				}
			}
		}
	}

	return found;
}

bool State::down(vector<unsigned short>& w, long pos, FstCompanion* f, char lemma) {
    long nxt, car, code, prev;
    bool found = false;

	if (pos == w.size()) {
        if (status&xfepsilonupper) {
            for (short i = 0; i < arcs.nb; i++) {
                car = arcs.character(i);
                if (car) {
                    prev = f->addalphabet(car);
                    if (arcs.found(i)->down(w, pos, f, lemma))
                        found = true;
                    f->reset(prev);
                }
            }
            return found;
        }
        
		if (status & xfarcend) {
            if (lemma == 2)
                f->storingfull();
            else
                f->storing();
			return true;
		}
        
        if (lemma) {
            long fprev = 0;
            for (short i = 0; i < arcs.nb; i++) {
                code = arcs.code(i);
                if (f->isfeature(code)) {
                    if (lemma == 2)
                        fprev = f->addfeature(code);
                    car = arcs.character(i);
                    if (car) {
                        prev = f->addalphabet(car);
                        if (arcs.found(i)->down(w, pos, f, lemma))
                            found = true;
                        f->reset(prev);
                    }
                    else {
                        if (arcs.found(i)->down(w, pos, f, lemma))
                            found = true;
                    }
                    if (lemma == 2)
                        f->resetfeature(fprev);
                }
            }
            return found;
        }
        
		return false;
	}

    for (long i = 0; i < arcs.nb; i++) {
        code = arcs.code(i);
        car = arcs.character(i);
        if (code) {
            if (code != w[pos])
                continue;
            nxt = pos + 1;
            if (!car) {
                if (status&xfepsilonlower) {
                    if (arcs.found(i)->down(w, nxt, f, lemma)) {
                        found = true;
                        continue;
                    }
                }
            }
        }
        else
            nxt = pos;
        
        if (car) {
            if (car == 1)
                prev = f->add(w[pos]);
            else
                prev = f->addalphabet(car);
            
            if (arcs.found(i)->down(w, nxt, f, lemma))
                found = true;
            f->reset(prev);
        }
        else {
            if (arcs.found(i)->down(w, nxt, f, lemma))
                found = true;
        }
    }
    
    return found;
}

#ifdef WSTRING_IS_UTF16
bool DoubleSideAutomaton::down(wstring& str, vector<string>& res, char lemma) {
	if (!finalize)
		fillencoding(true);

	unicodestring w(str);
#else
bool DoubleSideAutomaton::down(wstring& w, vector<string>& res, char lemma) {
	if (!finalize)
		fillencoding(true);
	//first we detect the features...
#endif

	long ps = w.find(L"\t", 0);
	if (ps == wstring::npos) {
		if (!lemma)
			return false;
		ps = w.size();
	}

	//we need a tab to cut at the feature level...
	vector<unsigned short> v;
	long i;
	for (i = 0; i < ps; i++)
		v.push_back(encoding[w[i]]);

	long fpos;
	for (i = 0; i < sortedmultis.size(); i++) {
		fpos = w.find(sortedmultis[i], ps);
		if (fpos != wstring::npos) {
#ifdef WSTRING_IS_UTF16
			w.replace(L'&', fpos, sortedmultis[i].size());
#else
			replaceonechar(w, L'&', fpos, sortedmultis[i].size());
#endif
			w[fpos] = (TRANSCHAR)multis[sortedmultis[i]];
		}
	}

	for (i = ps + 1; i < w.size(); i++)
		v.push_back((unsigned short)w[i]);

    FstCompanion* f = &Companion;
    res.clear();
    f->set(TAMGUVALUE(w),this,res);

    return start.down(v, 0, f, lemma);
}


bool CompareStrings(wstring s1, wstring s2) {
	if (s1.size() > s2.size())
		return(true);
	return(false);
}

void DoubleSideAutomaton::fillencoding(bool add) {
	finalize = true;
	
    encoding.clear();
    decoding.clear();
	for (auto& it : alphabet) {
		agnostring s(it.first);
		if (s.sizec() == 1) {
			ushort p = s.nextcode();
 			encoding[p] = it.second;
			decoding[it.second] = p;
		}
		else {
			if (add) {
				wstring u = s.latintounicode(encoding_table);
				if (it.first.find("\t") == string::npos) {
					multis[u] = it.second;
					sortedmultis.push_back(u);
					ialphabet[it.second] = "\t" + it.first;
                    featurecode[it.second] = it.second;
				}
			}
		}
	}
    
	sort(sortedmultis.begin(), sortedmultis.end(), CompareStrings);
}
