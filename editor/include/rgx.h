/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  rgx.h
//
//

#ifndef rgx_h
#define rgx_h

#include "vecte.h"

typedef enum{ an_epsilon=0, an_end=1, an_remove=2, an_negation=4, an_mandatory=8, an_error=12, an_rule=16, an_final=32, an_beginning=64, an_ending=128} an_flag;

typedef enum{aut_reg=1,aut_reg_plus,aut_reg_star,
    aut_meta, aut_meta_plus, aut_meta_star,
    aut_any,aut_any_plus,aut_any_star,
    aut_ocrl_brk, aut_ccrl_brk, aut_ccrl_brk_plus, aut_ccrl_brk_star,
    aut_obrk, aut_cbrk, aut_cbrk_plus, aut_cbrk_star,
    aut_opar, aut_cpar, aut_negation
}
aut_actions;

//------------------------character automatons---------------------------------------------------
class Au_automaton;
class Au_automatons;
class Au_automate;
//-----------------------------------------------------------------------------------------------

class Au_state;
class Au_any {
public:
    bool vero;
    an_flag type;
    
    Au_any(unsigned char t) {
        type=(an_flag)t;
        vero = true;
    }
    
    virtual bool same(Au_any* a) {
        if (a->Type()==type && a->vero == vero)
            return true;
        return false;
    }
    
    an_flag Type() {
        return type;
    }
    
    virtual char compare(wchar_t c) {
        return vero;
    }
    
    void setvero(bool neg) {
        vero = !neg;
    }
};

class Au_char : public Au_any {
public:
    wchar_t action;
    
    Au_char(wchar_t a, unsigned char t) : Au_any(t) {
        action=a;
    }
    
    bool same(Au_any* a) {
        if (a->Type()==type && a->vero == vero && ((Au_char*)a)->action==action)
            return true;
        return false;
    }

    char compare(wchar_t c) {
        if (action==c)
            return vero;
        return !vero;
    }
    
};

class Au_epsilon : public Au_any {
public:
    
    Au_epsilon()  : Au_any(an_epsilon) {}
    
    bool same(Au_any* a) {
        if (a->Type()==an_epsilon && a->vero == vero)
            return true;
        return false;
    }

    char compare(wchar_t c) {
        return 2;
    }
    
};

class Au_meta : public Au_any {
public:
    static UTF8_Handler* met;
    
    wchar_t action;
    
    Au_meta(uchar a, unsigned char t) : Au_any(t) {
        action=a;
    }
    
    bool same(Au_any* a) {
        if (a->Type()==type && a->vero == vero && ((Au_meta*)a)->action==action)
            return true;
        return false;
    }

    //CHSacdnprsx
    char compare(wchar_t car) {
        
        switch(action) {
            case 'C':
                if (met->c_is_upper(car))
                    return vero;
                return !vero;
            case 'S':
                if (car <= 32 || car == 160)
                    return vero;
                return !vero;
            case 'a':
                if (car=='_' || met->c_is_alpha(car))
                    return vero;
                return !vero;
            case 'c':
                if (met->c_is_lower(car))
                    return vero;
                return !vero;
            case 'd':
                if (c_is_digit(car))
                    return vero;
                return !vero;
            case 'e':
                if (met->c_is_emoji(car))
                    return vero;
                return !vero;
            case 'h': //Greek characters
                if (car >= 913 && car <= 987)
                    return vero;
                return !vero;
            case 'H':
                if (ckjchar(car))
                    return vero;
                return !vero;
            case 'E':
                if (met->c_is_emojicomp(car))
                    return vero;
                return !vero;
            case 'n':
                if (car == 160)
                    return vero;
                return !vero;
            case 'p':
                if (met->c_is_punctuation(car))
                    return vero;
                return !vero;
            case 'r':
                if (car == 10 || car == 13)
                    return vero;
                return !vero;
            case 's':
                if (car == 9 || car == 32 || car == 160)
                    return vero;
                return !vero;
            case 'x': //hexadecimal character
                if ((car>='A' && car <= 'F') || (car>='a' && car <= 'f') || (car >= '0' && car <= '9'))
                    return vero;
                return !vero;
            default:
                if (action == car)
                    return vero;
                return !vero;
        }
    }
};

class Au_arc {
public:
    Au_any* action;
    Au_state* state;
    unsigned char mark;
    
    Au_arc(Au_any* a) {
        action=a;
        state=NULL;
        mark=false;
    }
    
    ~Au_arc() {
        delete action;
    }
    
    an_flag Type() {
        return action->Type();
    }

    bool same(Au_arc* a) {
        return action->same(a->action);
    }
    
    bool checkfinalepsilon();

    bool find(u_ustring& w, u_ustring& sep, long i, vector<long>& res);

};

class Au_state {
public:
    vecte<Au_arc*> arcs;
    uchar status;
    unsigned char mark;
    
    Au_state() {
        status=0;
        mark=false;
    }

    void storerulearcs(unordered_map<long,bool>& rules);

    virtual long idx() {return -1;}
    bool isend() {
        if ((status&an_end)==an_end)
            return true;
        return false;
    }
    
    void removeend() {
        status &= ~an_end;
    }

    bool isfinal() {
        if (arcs.last == 0)
            return true;
        
        if ((status&an_final)==an_final)
            return true;
        return false;
    }
    
    bool isrule() {
        if ((status&an_rule)==an_rule)
            return true;
        return false;
    }
    
    bool match(u_ustring& w, long i);
    bool find(u_ustring& w, u_ustring& sep, long i, vector<long>& res);

    long loop(u_ustring& w, long i);
    
    void removeepsilon();
    void addrule(Au_arc*);
    void merge(Au_state*);
    
    Au_arc* build(Au_automatons* g, u_ustring& token, uchar type, Au_state* common, bool nega);
    Au_state* build(Au_automatons* g, long i,vector<u_ustring>& tokens, vector<aut_actions>& types, Au_state* common);
};

class Au_state_final : public Au_state {
public:
    
    long rule;
    
    Au_state_final(long r) {
        rule=r;
        status=an_rule;
    }

    long idx() {return rule;}

};

class Au_automaton {
public:
    
    Au_state* first;
    
    Au_automaton() {
        first=NULL;
    }

    Au_automaton(u_ustring rgx);
    
    bool match(u_ustring& w);
    bool search(u_ustring& w);

#ifdef WIN32
	bool match(wstring& w) {
		u_ustring u = _w_to_u(w);
		return match(u);
	}

	bool search(wstring& w) {
		u_ustring u = _w_to_u(w);
		return search(u);
	}

	bool search(wstring& w, long& first, long& last, long init = 0) {
		u_ustring u = _w_to_u(w);
		return search(u, first, last, init);
	}

	void searchall(wstring& w, vecte_a<long>& res, long init = 0) {
		u_ustring u = _w_to_u(w);
		searchall(u, res, init);
	}

#else
    bool match(wstring& w) {
        return match(_w_to_u(w));
    }
    
    bool search(wstring& w) {
        return search(_w_to_u(w));
    }

	bool search(wstring& w, long& first, long& last, long init = 0) {
		return search(_w_to_u(w), first, last, init);
	}

	void searchall(wstring& w, vecte_a<long>& res, long init = 0) {
		searchall(_w_to_u(w), res, init);
	}
#endif
    long find(u_ustring& w);
    long find(u_ustring& w, long i);


    bool search(u_ustring& w, long& first, long& last, long init = 0);

    bool searchlast(u_ustring& w, long& first, long& last, long init = 0);

    bool bytesearch(u_ustring& w, long& first, long& last);
    void bytesearchall(u_ustring& w, vecte_a<long>& res);

    void searchall(u_ustring& w, vecte_a<long>& res, long init = 0);

    void find(u_ustring& w, u_ustring& sep, vector<long>& res);
    virtual bool parse(u_ustring& rgx, Au_automatons* automatons=NULL);
    
};


class Au_automatons {
public:
    vecte<Au_state*> states;
    vecte<Au_arc*> arcs;

    Au_state* state() {
        Au_state* s=new Au_state;
        states.push_back(s);
        return s;
    }

    Au_state* statefinal(long r) {
        Au_state_final* s=new Au_state_final(r);
        states.push_back(s);
        return s;
    }

    Au_arc* arc(Au_any* a, Au_state* s=NULL) {
        Au_arc* ac=new Au_arc(a);
        arcs.push_back(ac);
        if (s==NULL)
            ac->state=state();
        else
            ac->state=s;
        return ac;
    }

    void clean() {
        states.wipe();
        arcs.wipe();
    }

    void clean(long s, long a) {
        long i,ii;
        //We delete the states marked for destruction...
        for (i=s;i<states.size();i++) {
            if (states.vecteur[i] != NULL && states.vecteur[i]->mark == an_remove) {
                delete states.vecteur[i];
                states.vecteur[i]=NULL;
            }
        }


        //Compacting
        //We remove the NULL...
        for (i=s;i<states.size();i++) {
            if (states.vecteur[i]==NULL) {
                ii=i;
                while (ii<states.last && states.vecteur[ii]==NULL) ii++;
                if (ii==states.last) {
                    states.last=i;
                    break;
                }
                states.vecteur[i]=states.vecteur[ii];
                states.vecteur[ii]=NULL;
            }
        }

        //We delete the arcs marked for destruction...
        for (i=a;i<arcs.size();i++) {
            if (arcs.vecteur[i] != NULL && arcs.vecteur[i]->mark == an_remove) {
                delete arcs.vecteur[i];
                arcs.vecteur[i]=NULL;
            }
        }

        //Compacting
        //We remove the NULL...
        for (i=a;i<arcs.size();i++) {
            if (arcs.vecteur[i]==NULL) {
                ii=i;
                while (ii<arcs.last && arcs.vecteur[ii]==NULL) ii++;
                if (ii==arcs.last) {
                    arcs.last=i;
                    break;
                }
                arcs.vecteur[i]=arcs.vecteur[ii];
                arcs.vecteur[ii]=NULL;
            }
        }
    }

    void clearmarks() {
        long i;
        for (i=0;i<states.last;i++) {
            if (states.vecteur[i]!=NULL)
                states.vecteur[i]->mark=false;
        }
        for (i=0;i<arcs.last;i++) {
            if (arcs.vecteur[i] != NULL)
                arcs.vecteur[i]->mark=false;
        }
    }

    void clear(long s, long a) {
        long i;
        for (i=s;i<states.last;i++) {
            if (states.vecteur[i]!=NULL)
                states.vecteur[i]->mark=false;
        }
        for (i=a;i<arcs.last;i++) {
            if (arcs.vecteur[i] != NULL)
                arcs.vecteur[i]->mark=false;
        }
    }

    void boundaries(long& s, long& a) {
        s=states.size();
        a=arcs.size();
    }

    ~Au_automatons() {
        states.wipe();
        arcs.wipe();
    }
};

class Au_automate : public Au_automaton {
public:
    Au_automatons garbage;

    Au_automate() {
        first=NULL;
    }

    Au_automate(wstring& rgx);
    
    Au_automate(u_ustring& rgx);

    bool compile(u_ustring& rgx) {
        return parse(rgx, &garbage);
    }

    bool compiling(u_ustring& rgx,long feature);
};


class Jag_automaton :  public Au_automate {
public:
    
    wstring regularexpression;
    
    Jag_automaton(wstring& rgx) : Au_automate(rgx) {
        regularexpression = rgx;
    }

};

#endif

