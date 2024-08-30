/*
 *  Tamgu (탐구)
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : Tamgu (탐구)
 Version    : See tamgu.cxx for the version number
 filename   : jagrgx.cxx
 Date       : 2019/03/25
 Purpose    : regular expression implementation for find/replace commands
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#ifdef BOOSTPOSIXREGEX
#include <boost/regex.hpp>
using boost::regex;
using boost::sregex_token_iterator;
using boost::smatch;
using boost::match_results;
using boost::wregex;
using boost::wsregex_token_iterator;
using boost::wsmatch;
using boost::sregex_iterator;
using boost::wsregex_iterator;
#else
#ifdef POSIXREGEX
#include <regex>
#include <iterator>
using std::regex;
using std::sregex_token_iterator;
using std::smatch;
using std::match_results;
using std::wregex;
using std::wsregex_token_iterator;
using std::wsmatch;
using std::sregex_iterator;
using std::wsregex_iterator;
#endif
#endif

#include "tools.h"
#include "rgx.h"


//--------------------------------------------------------------------
UTF8_Handler* Au_meta::met = &special_characters;
//--------------------------------------------------------------------
#define au_error -1
#define au_stop -2
#define setchar(x,y) x[0] = y[0]; x[1] = y[1]; x[2] = y[2]; x[3] = y[3]
#define charsz(c) c[1] ? c[2] ? c[3] ? 4 : 3 : 2 : 1
#define addtoken(tok,c) tok.add((uchar*)c,charsz(c))
#define checkcr if (chr[0] == '\n') l++
//---------------------------------------AUTOMATON------------------------------------------------------------
static bool tokenize(u_ustring& rg, vector<u_ustring>& stack, vector<aut_actions>& vtypes) {
    u_ustring sub;
    long sz = rg.size();
    uchar type=aut_reg;
    long inbracket = 0;

    for (long i = 0; i < sz; i++) {
        switch (rg[i]) {
            case 10:
            case 13:
                continue;
            case '\\': //escape character
                if ((i + 1) < sz) {
                    sub = rg[++i];
                    //we accept: \ddd, exactly three digits
                    if (c_is_digit(rg[i]) && (i+2)<sz && c_is_digit(rg[i+1]) && c_is_digit(rg[i+2])) {
                        sub+=rg[i+1];
                        sub+=rg[i+2];
                        i+=2;
                        sub = (u_uchar)convertinginteger(sub);
                    }
                    else //we also accept four hexa: \xFFFF
                        if (rg[i] == 'x' && (i+4)<sz && c_is_hexa(rg[i+1]) && c_is_hexa(rg[i+2]) && c_is_hexa(rg[i+3]) && c_is_hexa(rg[i+4])) {
                            sub=U"0x";
                            sub+=rg[i+1];
                            sub+=rg[i+2];
                            sub+=rg[i+3];
                            sub+=rg[i+4];
                            i+=4;
                            sub = (u_uchar)convertinginteger(sub);
                        }
                    type = aut_reg;
                }
                break;
            case '%':
                sub = U"%";
                if ((i + 1) < sz) {
                    type = aut_meta;
                    i++;
                    sub += rg[i];
                }
                break;
            case '~': // negation of the next character
                sub = L'~';
                type = aut_negation;
                break;
            case '?':
                sub = L'?';
                type = aut_any;
                break;
            case '(':
                sub = L'(';
                type = aut_opar;
                break;
            case ')':
                stack.push_back(U")");
                vtypes.push_back(aut_cpar);
                continue; //we cannot have: ..)+ or ..)*
            case '[':
                sub = L'[';
                type = aut_obrk;
                inbracket++;
                break;
            case ']':
                sub = L']';
                type = aut_cbrk;
                inbracket--;
                break;
            case '{': //curly bracket
                sub = L'{';
                type = aut_ocrl_brk;
                inbracket++;
                break;
            case '}':
                sub = L'}';
                type = aut_ccrl_brk;
                inbracket--;
                break;
            case '-':
                if (inbracket && i < sz-1 && vtypes.back() == aut_reg) {
                    //{a-z} or [a-z]
                    //in that case, we build the character list between the current character and the next one...
                    sub = stack.back();
                    u_ustring nxt;
                    nxt = rg[++i];
                    ++sub[0];
                    while (sub[0] <= nxt[0]) {
                        stack.push_back(sub);
                        vtypes.push_back(aut_reg);
                        ++sub[0];
                    }
                    continue;
                }
            default:
                sub = rg[i];
                type = aut_reg;
        }

        if ((i + 1) < sz) {
            if (rg[i + 1] == L'+') {
                i++;
                type += 1;
                sub += rg[i];
            }
            else {
                if (rg[i + 1] == L'*') {
                    i++;
                    type += 2;
                    sub += rg[i];
                }
            }
        }

        stack.push_back(sub);
        vtypes.push_back((aut_actions)type);
    }

    if (inbracket)
        return false;

    return true;
}
//an epsilon, which points to a final state with no arcs attached
bool Au_arc::checkfinalepsilon() {
    if (action->Type() == an_epsilon && state->isend() && !state->arcs.last)
        return true;
    return false;
}

void Au_state::removeepsilon() {
    if (mark)
        return;

    mark=true;
    vector<long> removals;
    long i;
    for (i=0;i<arcs.size();i++) {
        if (arcs[i]->checkfinalepsilon()) {
            //We can remove it...
            removals.push_back(i);
        }
        else
            arcs[i]->state->removeepsilon();
    }

    for (i = removals.size()-1; i>=0; i--) {
        status|=an_end;
        arcs.erase(removals[i]);
    }
}
//----------------------------------------------------------------

void Au_state::addrule(Au_arc* r) {
    if (mark)
        return;

    mark=true;
    //This is a terminal node...
    if (isend())
        arcs.push_back(r);

    for (long i=0;i<arcs.size();i++)
        arcs[i]->state->addrule(r);

    return;
}

//----------------------------------------------------------------
bool Au_state::match(u_ustring& w, long i) {
    if ((status&an_error) == an_error)
        return false;

    if (i==w.size()) {
        if (isend())
            return true;
        return false;
    }

    UWCHAR c = Au_meta::met->getachar(w,i);

    for (long j=0;j<arcs.last;j++) {
        switch(arcs[j]->action->compare(c)) {
            case 0:
                break;
            case 1:
                if (arcs[j]->state->match(w,i+1))
                    return true;
                break;
            case 2:
                if (arcs[j]->state->match(w,i))
                    return true;
        }
    }

    return false;
}

bool Au_automaton::match(u_ustring& w) {
    return first->match(w,0);
}

//----------------------------------------------------------------

void Au_state::storerulearcs(std::unordered_map<long,bool>& rules) {
    if (mark)
        return;

    if (isrule())
        rules[idx()]=true;

    mark=true;
    for (long j=0;j<arcs.last;j++)
        arcs[j]->state->storerulearcs(rules);
    mark=false;
}

//----------------------------------------------------------------

long Au_state::loop(u_ustring& w, long i) {
    if ((status & an_error) == an_error)
        return au_stop;

    if (i==w.size()) {
        if (isend())
            return i;
        return au_error;
    }

    if (status & an_beginning) {
        if (i)
            return au_error;
    }

    long l = au_error;
    long j;

    UWCHAR c = Au_meta::met->getachar(w,i);

    for (j=0;j<arcs.last;j++) {
        switch(arcs[j]->action->compare(c)) {
            case 0:
                l = au_error;
                continue;
            case 1:
                l = arcs[j]->state->loop(w,i+1);
                break;
            case 2:
                l = arcs[j]->state->loop(w, i);
        }
        if (l != au_error) {
            if (l == au_stop)
                break;
            return l;
        }
    }

    if (isend()) {
        if (status & an_ending) {
            if (i != w.size())
                return au_error;
        }
        return i;
    }

    return au_error;
}

long Au_automaton::find(u_ustring& w) {
    long sz = w.size();
    for (long d=0;d<sz;d++) {
        if (first->loop(w,d) != au_error) {
            return d;
        }
    }
    return au_error;
}

long Au_automaton::find(u_ustring& w, long i) {
    long sz = w.size();
    for (long d = i ; d < sz; d++) {
        if (first->loop(w,d) != au_error) {
            return d;
        }
    }
    return au_error;
}

bool Au_automaton::search(u_ustring& w) {
    long sz = w.size();
    for (long d=0;d<sz;d++) {
        if (first->loop(w,d) != au_error)
            return true;
    }
    return false;
}

bool Au_automaton::search(u_ustring& w, long& b, long& e, long init) {
    long sz = w.size();
    for (b=init;b<sz;b++) {
        e=first->loop(w,b);
        if (e != au_error) {
            return true;
        }
    }
    b=au_error;
    return false;
}

bool Au_automaton::searchlast(u_ustring& w, long& b, long& e, long init) {
    b=au_error;
    long f;
    long sz = w.size();
    for (long d=init;d<sz;d++) {
        f=first->loop(w,d);
        if (f!=au_error) {
            b=d;
            e=f;
            d=f-1;
        }
    }

    if (b!=au_error)
        return true;
    return false;
}



//----------------------------------------------------------------
void Au_automaton::searchall(u_ustring& w, vecte_a<long>& res, long init) {
    long f;
    long sz = w.size();

    for (long d=init;d<sz;d++) {
        f=first->loop(w,d);
        if (f!=au_error) {
            res.push_back(d);
            res.push_back(f);
            d=f-1;
        }
    }
}

//----------------------------------------------------------------
bool Au_arc::find(u_ustring& w, u_ustring& wsep, long i, vector<long>& res) {
    if (i==w.size())
        return false;

    UWCHAR c = Au_meta::met->getachar(w,i);

    switch(action->compare(c)) {
        case 0:
            return false;
        case 1:
            if (c == wsep[0])
                res.push_back(i+1);
            return state->find(w, wsep, i+1, res);
        case 2:
            return state->find(w, wsep, i, res);
    }
    return false;
}

bool Au_state::find(u_ustring& w, u_ustring& wsep, long i, vector<long>& res) {
    long ps=res.size();
    long j;

    if (status & an_beginning) {
        if (i)
            return au_error;
    }

    for (j=0;j<arcs.last;j++) {
        if (!arcs[j]->find(w, wsep, i, res)) {
            while (ps != res.size()) {
                res.pop_back();
            }
        }
        else
            return true;
    }

    if (isend()) {
        if (status & an_ending) {
            if (i != w.size())
                return false;
        }

        if (res[res.size()-1]!=i+1)
            res.push_back(i+1);
        return true;
    }

    return false;
}

//----------------------------------------------------------------
//The next two methods return raw indexes... No conversion needed
bool Au_automaton::bytesearch(u_ustring& w, long& b, long& e) {
    long sz = w.size();
    for (b=0; b<sz; b++) {
        e=first->loop(w,b);
        if (e!=au_error)
            return true;
    }
    b=au_error;
    return false;
}


void Au_automaton::bytesearchall(u_ustring& w, vecte_a<long>& res) {
    long f;
    long sz = w.size();
    for (long d=0; d<sz; d++) {
        f=first->loop(w,d);
        if (f!=au_error) {
            res.push_back(d);
            res.push_back(f);
            d=f-1;
        }
    }
}

//----------------------------------------------------------------

void Au_state::merge(Au_state* a) {
    if (a->mark)
        return;
    a->mark=true;

    status |= a->status;

    long sz=arcs.size();
    for (long i=0;i<a->arcs.size();i++) {
        if (a->arcs[i]->state->isrule()) {
            arcs.push_back(a->arcs[i]);
            continue;
        }

        long found=au_error;
        for (long j=0;j<sz;j++) {
            if (a->arcs[i]->same(arcs[j])) {
                found=j;
                break;
            }
        }
        if (found==au_error)
            arcs.push_back(a->arcs[i]);
        else {
            arcs[found]->state->merge(a->arcs[i]->state);
            a->arcs[i]->state->mark=an_remove;
            a->arcs[i]->mark=an_remove;
        }
    }
}
//----------------------------------------------------------------

Au_automate::Au_automate(u_ustring& wrgx) {
    first=NULL;
    if (!parse(wrgx,&garbage))
        first = NULL;
}

#ifdef WIN32
Au_automate::Au_automate(wstring& wrgx) {
    first=NULL;
	u_ustring u = _w_to_u(wrgx);
    if (!parse(u,&garbage))
        first = NULL;
}
#else
Au_automate::Au_automate(wstring& wrgx) {
	first = NULL;
	if (!parse(_w_to_u(wrgx), &garbage))
		first = NULL;
}
#endif

Au_automaton::Au_automaton(u_ustring wrgx) {
    first=NULL;
    if (!parse(wrgx))
        first = NULL;
}

bool Au_automaton::parse(u_ustring& w, Au_automatons* aus) {
    //static x_wautomaton xtok;
    //first we tokenize

    //First we detect the potential %X, where X is a macro

    vector<u_ustring> toks;
    vector<aut_actions> types;
    if (!tokenize(w,toks, types))
        return false;

    if (aus==NULL)
        aus=new Au_automatons;

    if (first==NULL)
        first=aus->state();

    Au_state base;
    long ab,sb;
    aus->boundaries(sb,ab);

    if (base.build(aus, 0, toks,types,NULL)==NULL)
        return false;

    base.removeepsilon();
    base.mark=false;
    aus->clear(sb,ab);
    first->merge(&base);

    //we delete the elements that have been marked for deletion...
    aus->clean(sb,ab);
    return true;
}

bool Au_automate::compiling(u_ustring& w,long r) {
    //static x_wautomaton xtok;
    //first we tokenize

    vector<u_ustring> toks;
    vector<aut_actions> types;
    if (!tokenize(w,toks, types))
        return false;

    if (first==NULL)
        first=garbage.state();

    Au_state base;
    long ab,sb;
    garbage.boundaries(sb,ab);

    if (base.build(&garbage, 0, toks,types,NULL)==NULL)
        return false;

    Au_state* af=garbage.statefinal(r);
    Au_arc* fin=garbage.arc(new Au_epsilon(),af);
    base.addrule(fin);

    base.mark=false;
    garbage.clear(sb,ab);
    first->merge(&base);

    garbage.clean(sb,ab);
    return true;
}

//----------------------------------------------------------------------------------------
#define an_mandatory 8

Au_state* Au_state::build(Au_automatons* aus, long i,vector<u_ustring>& toks, vector<aut_actions>& types, Au_state* common) {
    mark=false;
    Au_arc* ar;
    bool nega = false;

    if (i==toks.size()) {
        status |= an_end;
        if (common != NULL) {
            ar=aus->arc(new Au_epsilon(), common);
            arcs.push_back(ar);
        }

        return this;
    }

    if (types[i] == aut_negation) {
        ++i;
        nega = true;
    }

    long j;
    bool stop;
    short count;
    vector<u_ustring> ltoks;
    vector<aut_actions> ltypes;
    Au_state* ret = NULL;
    uchar localtype = types[i];

    switch(localtype) {
        case aut_ocrl_brk: { //{..}
            i++;
            Au_state* commonend=aus->state();
            vecte<Au_arc*> locals;
            stop=false;
            while (i<toks.size() && !stop) {
                switch(types[i]) {
                    case aut_ccrl_brk_plus: //}+
                    case aut_ccrl_brk_star: //}*
                    case aut_ccrl_brk: //}
                        stop=true;
                        break;
                    case aut_ocrl_brk:
                    case aut_obrk:
                    case aut_opar: { //a sub expression introduced with (), {} or []
                        uchar current_action=types[i]; // the current type...
                        uchar lbound=current_action+1;
                        uchar hbound=current_action+3;
                        if (current_action==aut_opar)
                            hbound=lbound; //the only value is aut_cpar...
                        ltoks.clear(); //we extract the sub-element itself...
                        ltypes.clear();
                        count=1;
                        ltoks.push_back(toks[i]);
                        ltypes.push_back(types[i]);
                        i++;
                        while (i<toks.size() && count) {
                            ltoks.push_back(toks[i]);
                            ltypes.push_back(types[i]);
                            if (types[i]==current_action) //which can be other sub-elements of the same kind...
                                count++;
                            else {
                                if (types[i]>=lbound && types[i]<=hbound) //the stop value, with a control over the potential embedding...
                                    count--;
                            }
                            i++;
                        }
                        if (i==toks.size() || !ltoks.size()) //We could not find the closing character, this is an error...
                            return NULL;

                        Au_state s;
                        ret=s.build(aus, 0,ltoks,ltypes,commonend);
                        if (ret==NULL)
                            return NULL;

                        //It cannot be an end state, commonend will be...
                        ret->removeend();

                        for (j=0;j<s.arcs.last;j++) {
                            if (s.arcs[j]->state == commonend)
                                continue;

                            locals.push_back(s.arcs[j]);
                            arcs.push_back(s.arcs[j]);
                        }
                        break;
                    }
                    default:
                        ar=build(aus, toks[i],types[i],commonend, false);
                        if (ar==NULL)
                            return NULL;
                        locals.push_back(ar);
                        i++;
                }
            }

            if (i==toks.size())
                return NULL;

            if (nega) {
                commonend->status = an_error;
                //in this case, commonend is the path to error...
                //we create a parallel path, which lead to either a loop or a
                ar=aus->arc(new Au_any(aut_any));
                arcs.push_back(ar);
                commonend = ar->state;
                locals.push_back(ar);
            }

            //aut_ccrl_brk_plus: closing curly bracked+
            //aut_ccrl_brk_star: closing curly bracked*
            if (types[i]==aut_ccrl_brk_plus || types[i]==aut_ccrl_brk_star) {//The plus and the star for the disjunction {...}
                for (j=0;j<locals.size();j++)
                    commonend->arcs.push_back(locals[j]);
                if (types[i]==aut_ccrl_brk_star) {
                    ar=aus->arc(new Au_epsilon());
                    arcs.push_back(ar);
                    commonend->arcs.push_back(ar);
                    commonend=ar->state;
                }
                else
                    commonend->status |= an_mandatory; //this is now an end to any automaton traversal...
            }
            else
                commonend->status |= an_mandatory;

            ret = commonend->build(aus, i+1,toks,types,common);
            if (ret != NULL && ret->isend() && !(ret->status&an_mandatory))
                status |= an_end;
            return ret;
        }
        case aut_opar: {//(..)
            if (nega)
                return NULL;

            i++;
            count=1;
            while (i<toks.size()) {
                if (types[i]==aut_opar) //embeddings
                    count++;
                else {
                    if (types[i]==aut_cpar) {
                        count--;
                        if (!count)
                            break;
                    }
                }
                ltoks.push_back(toks[i]);
                ltypes.push_back(types[i]);
                i++;
            }
            if (i==toks.size() || !ltoks.size())
                return NULL;
            ret=build(aus, 0,ltoks,ltypes,NULL);
            if (ret==NULL)
                return NULL;

            ret->removeend();
            //We jump...
            ar=aus->arc(new Au_epsilon(), ret);
            arcs.push_back(ar);
            //These are the cases, when we have a x* at the end of an expression...
            //The current node can be an end too
            ret->status &= ~an_mandatory;
            ret = ret->build(aus, i+1,toks,types,common);
            if (ret != NULL && ret->isend() && !(ret->status&an_mandatory))
                status |= an_end;
            return ret;
        }
        case aut_obrk: { //[..]
            i++;
            count=1;
            ltoks.clear();
            ltypes.clear();
            while (i<toks.size()) {
                if (types[i]==aut_obrk) //embeddings
                    count++;
                else {
                    if (types[i]==aut_cbrk_plus || types[i]==aut_cbrk_star || types[i]==aut_cbrk) {
                        count--;
                        if (!count)
                            break;
                    }
                }
                ltoks.push_back(toks[i]);
                ltypes.push_back(types[i]);
                i++;
            }
            if (i==toks.size() || !ltoks.size())
                return NULL;

            Au_state s;
            ret=s.build(aus, 0,ltoks,ltypes,NULL);
            if (ret==NULL)
                return NULL;

            ret->removeend();
            ret->status |= an_mandatory; //if it is a +, we expect at least one value, cannot be a potential end

            if (types[i]!=aut_cbrk) {//the plus
                //s is our starting point, it contains all the arcs we need...
                for (j=0;j<s.arcs.last;j++) {
                    arcs.push_back(s.arcs[j]);
                    ret->arcs.push_back(s.arcs[j]);
                } //we need to jump back to our position...

                if (types[i]==aut_cbrk_star) {//this is a star, we need an epsilon to escape it...
                    ar=aus->arc(new Au_epsilon(), ret);
                    arcs.push_back(ar);
                    ret->status &= ~an_mandatory;
                }
            }
            else {
                for (j=0;j<s.arcs.last;j++)
                    arcs.push_back(s.arcs[j]);
            }

            //These are the cases, when we have a x* at the end of an expression...
            //The current node can be an end too
            ret = ret->build(aus, i+1,toks,types,common);
            if (ret != NULL && ret->isend() && !(ret->status&an_mandatory))
                status |= an_end;
            return ret;
        }
    }

    Au_state* next;
    if ((i+1)==toks.size())
        next=common;
    else
        next=NULL;

    if (toks[i] == U"^") {
        status |= an_beginning;
        return build(aus, i+1,toks,types,common);
    }

    if (toks[i] == U"$") {
        ret = build(aus, i+1,toks,types,common);
        ret->status |= an_ending;
        return ret;
    }

    Au_arc* retarc=build(aus, toks[i], localtype,next, nega);
    if (retarc==NULL)
        return NULL;

    next = retarc->state->build(aus, i+1,toks,types,common);

    if (next != NULL && next->isend() && !(next->status&an_mandatory)) {
        //If the current element is a *, then it can be skipped up to the end...
        switch(localtype) {
            case aut_meta_star:
            case aut_reg_star:
            case aut_any_star:
                status |= an_end;
                break;
            default: //from now on, we are in a mandatory section
                next->status |= an_mandatory;
        }
    }

    return next;
}

bool checkmeta(u_ustring& tok) {
    switch(tok[1]) {
        case 'C':
        case 'E':
        case 'S':
        case 'a':
        case 'c':
        case 'd':
        case 'e':
        case 'n':
        case 'p':
        case 'r':
        case 's':
        case 'x': //hexadecimal character
            return true;
        default:
            return false;
    }
}

Au_arc* Au_state::build(Au_automatons* aus, u_ustring& token, uchar type, Au_state* common, bool nega) {
    //First we scan the arcs, in case, it was already created...
    Au_any* a=NULL;

    //value arc: meta or character...
    switch(type) {
        case aut_any: //?
        case aut_any_plus: //?+
        case aut_any_star: //?*
            a=new Au_any(type);
            break;
        case aut_meta:
        case aut_meta_plus: //%x+
        case aut_meta_star: //%x*
            if (checkmeta(token)) //if we are dealing with an escaped character
                a = new Au_meta(token[1], type);
            else
                a = new Au_char(token[1], type);
            break;
        case aut_reg:
        case aut_reg_plus: //x+
        case aut_reg_star: //x*
            a=new Au_char(token[0], type);
            break;
        default:
            return NULL;
    }

    a->setvero(nega);

    //we check if we already have such an arc...
    for (long j=0;j<arcs.size();j++) {
        if (arcs[j]->action->same(a)) {
            delete a;
            arcs[j]->mark=false;
            return arcs[j];
        }
    }

    //Different case...
    //first if a is not NULL and a is a loop
    Au_arc* current=aus->arc(a, common);
    Au_arc* ar;
    switch(type) {
        case aut_meta_plus: //+
        case aut_reg_plus:
        case aut_any_plus:
            current->state->status |= an_mandatory; //we mark that this state as a mandatory one
            current->state->arcs.push_back(current); //the loop
            arcs.push_back(current);
            return current;
        case aut_meta_star: //*
        case aut_reg_star:
        case aut_any_star:
            ar=aus->arc(new Au_epsilon(),current->state);
            arcs.push_back(ar);
            current->state->arcs.push_back(current); //the loop
            arcs.push_back(current);
            return current;
        default:
            current->state->status |= an_mandatory; //we mark that this state as a mandatory one
            if (arcs.last>0) //We always insert our arc at the top to force its recognition before any loop...
                arcs.insert(0,current);
            else
                arcs.push_back(current);
    }
    return current;
}

