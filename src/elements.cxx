/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  elements.cxx
//
//

#include "lispe.h"
#include "tools.h"
#include <math.h>
#include <algorithm>

//------------------------------------------------------------------------------------------

union double64 {
public:
    
    uint64_t bits;
    double v;
    
    double64(double d) {
        v = d;
    }
};

//------------------------------------------------------------------------------------------
Infinitelist::Infinitelist(LispE* lisp) : Element(t_list) {
    value = null_;
}

Cyclelist::Cyclelist(LispE* lisp) : Element(t_list) {
    value = null_;
}

//------------------------------------------------------------------------------------------
inline bool LIST::compare(LispE* lisp, List* comparison, short instruction, long i, long j) {
    comparison->liste[1] = item->buffer[i];
    comparison->liste[2] = item->buffer[j];
    return comparison->eval_Boolean(lisp, instruction);
}

void LIST::sorting(LispE* lisp, List* comparison, short instruction, long rmin, long rmax) {
    //(setq s (sort '< (shuffle (cons 5 (range 1 99999 1)))))
    //(sort '< '(28 60 10 38 80 34 8 22 78 68 85 48 13 39 100 56 89 82 11 52 99 50 20 96 97 59 23 81 53 15 3 67 77 7 57 74 49 32 86 66 43 26 75 62 29 71 2 91 51 1 18 12 24 21 36 72 90 40 70 14 61 93 6 4 79 94 47 58 30 83 84 44 88 63 95 45 33 65 37 92 27 64 55 9 31 73 54 16 98 5 46 25 76 42 17 69 19 35 5 41 87))
    //(sort '< '(20 12 15 13 19 17 14))
    //(sort '< (shuffle (range 1 16 1)))
    //(sort '< '(4 3 7 1 5))
    //(sort '< '(10 4 8 5 12 2 6 11 3 9 7 9))

    //check sorting stability
    //(loop i (range 1 9999 1) (select (<= (at s i) (at s (+ i 1))) (println 'erreur i)))
    
    long j = rmax-rmin+1;
    long pivot;
    
    if (j < 7) {
        if (j < 2)
            return;
        
        if (j == 2) {
            if (compare(lisp, comparison, instruction, rmax, rmin))
                item->swap(rmax, rmin);
            return;
        }
        
        if (j == 3) {
            if (compare(lisp, comparison, instruction, rmin, rmin + 1)) {
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            else {
                item->swap(rmin, rmin + 1);
                if (compare(lisp, comparison, instruction, rmin + 1, rmax))
                    return;
            }
            item->swap(rmax, rmin + 1);
            if (compare(lisp, comparison, instruction, rmin, rmin + 1))
                return;
            item->swap(rmin, rmin + 1);
            return;
        }
        
        long sz;
        while (rmax > rmin) {
              sz = rmin;
              for (j = rmin; j < rmax; j++) {
                  if (compare(lisp, comparison, instruction, j + 1, j)) {
                      item->swap(j, j + 1);
                      sz = j;
                      pivot = j;
                      while (pivot > rmin && compare(lisp, comparison, instruction, pivot, pivot - 1))
                          item->swap(pivot, pivot - 1);
                  }
              }
              rmax = sz;
          }
        return;
    }
    
    pivot = rmin - 1;
    comparison->liste[2] = item->buffer[rmax];
    for (j = rmin; j < rmax; j++) {
        comparison->liste[1] = item->buffer[j];
        if (comparison->eval_Boolean(lisp, instruction)) {
            pivot++;
            item->swap(pivot,j);
        }
    }
    pivot++;
    item->swap(pivot, rmax);
    
    sorting(lisp, comparison, instruction, rmin, pivot-1);
    sorting(lisp, comparison, instruction, pivot+1, rmax);
}

void LIST::sorting(LispE* lisp, List* comparison) {
    //We sort between home and last...
    long sz = item->last - home;
    if (sz <= 1)
        return;
    
    sorting(lisp, comparison, comparison->liste[0]->type, home, item->last - 1);
}
//------------------------------------------------------------------------------------------


void Element::prettyfying(LispE* lisp, string& code) {
    if (isList()) {
        if (size() == 0) {
            code += " ()";
            return;
        }
                
        short type = index(0)->type;
        if (type == l_lambda) {
            code += " ";
            code += toString(lisp);
            return;
        }
        
        Element* params;
        
        if (type == l_defun || type == l_defpat || type == l_deflib || type == l_loop) {
            code += "(";
            code += lisp->toString(type);
            code += " ";
            code += protected_index(lisp, (long)1)->toString(lisp);
            code += " ";
            params = protected_index(lisp, (long)2);
            if (type == l_defpat) {
                code += "(";
                string local;
                for (long i = 0; i < params->size(); i++) {
                    if (i)
                        code += " ";
                    local = params->protected_index(lisp, i)->toString(lisp);
                    if (local[0] == '(') {
                        local[0] = '[';
                        local.back() = ']';
                    }
                    code += local;
                }
                code += ")";
            }
            else
                code += params->toString(lisp);
            code += "\n";
            for (long i = 3; i < size(); i++) {
                protected_index(lisp, i)->prettyfying(lisp, code);
            }
            code += ")\n";
            return;
        }

        long i = 0;


        if (type == l_if || type == l_check || type == l_ncheck || type == l_ife) {
            code += "(";
            code += protected_index(lisp, i++)->toString(lisp);
            code += " ";
            code += protected_index(lisp, i++)->toString(lisp);
            code += "\n";
            for (; i < size(); i++) {
                protected_index(lisp, i)->prettyfying(lisp, code);
            }
            code += ")\n";
            return;
        }

        string local = toString(lisp);
        if (local.size() < 50) {
            code += local;
            code += "\n";
            return;
        }
        
        code += "(";

        if (type == l_while || type == l_setq || type == l_setg || type == l_loopcount || type == l_key || type == l_keyn) {
            code += protected_index(lisp, i++)->toString(lisp);
            code += " ";
            code += protected_index(lisp, i++)->toString(lisp);
            code += "\n";
        }
        else {
            if (type > t_error && type < l_final) {
                code += protected_index(lisp, i++)->toString(lisp);
                i = 1;
            }
            code += "\n";
        }
                
        for (; i < size(); i++) {
            params = protected_index(lisp, i);
            params->prettyfying(lisp, code);
            if (code.back() != '\n')
                code += "\n";
        }
        code += ")\n";
        return;
    }
    if (isString())
        code += jsonstring(toString(lisp));
    else {
        if (isDictionary()) {
            string local = toString(lisp);
            if (local.size() < 50) {
                code += local;
                return;
            }
            code += "{\n";
            if (type == t_dictionary) {
                map<wstring, Element*>& dico = ((Dictionary*)this)->dictionary;
                wstring key;
                for (auto& a: dico) {
                    local = "";
                    key = a.first;
                    s_unicode_to_utf8(local, key);
                    code += local;
                    code += ":";
                    a.second->prettyfying(lisp, code);
                    if (code.back() != '\n')
                        code += "\n";
                }
                code += "}\n";
                return;
            }
            unordered_map<double, Element*>& dico = ((Dictionary_n*)this)->dictionary;
            for (auto& a: dico) {
                local = std::to_string(a.first);
                code += local;
                code += ":";
                a.second->prettyfying(lisp, code);
                if (code.back() != '\n')
                    code += "\n";
            }
            code += "}\n";
            return;
        }
        else
            code += toString(lisp);
    }
}

//(defpat action ( [Take 'x] [Take y] )(if (check_object position x) (block (push belongings x) (println "Ok we have picked up" x)) (println "Cannot pick up the" x)))
//(prettify '((12 3) (4 5 6) (8 9 10) (12 3) (4 5 6) (8 9 10) (12 3) (4 5 6) (8 9 10)))

string Element::prettify(LispE* lisp) {
    string code;
    prettyfying(lisp, code);
    string body;
    IndentCode(code, body, GetBlankSize());
    return body;
}

//------------------------------------------------------------------------------------------
//This method returns the needed instructions to build the dictionary
Element* Dictionary_as_list::dictionary(LispE* lisp) {
    if (!choice || keyvalues.size() != valuevalues.size())
        throw new Error("Error: dictionary has a different number of key/value");
    
    List* l;
    List* last_element = new List;
    Element* keycmd;
    
    if (type == t_number || type == t_integer)
        // (keyn (keyn (keyn (keyn (keyn (keyn (keyn (keyn) c v) c v) c v)
        keycmd = lisp->delegation->_DICO_KEYN;
    else
        keycmd = lisp->delegation->_DICO_KEY;
    
    last_element->append(keycmd);
    for (long i = 0; i < keyvalues.size(); i++) {
        l = new List;
        l->append(keycmd);
        l->append(last_element);
        l->append(keyvalues[i]);
        l->append(valuevalues[i]);
        last_element = l;
    }
    return last_element;
}

//This method returns the actual dictionary. It is used in json_parse, which expects
//the input to have no reference to the current Lisp program (no variables)
Element* Dictionary_as_list::rawdictionary(LispE* lisp) {
    if (keyvalues.size() != valuevalues.size())
        throw new Error("Error: dictionary has a different number of key/value");
    if (!keyvalues.size())
        return emptydictionary_;
    
    Element* e;
    if (type == t_number || type == t_integer) {
        // (keyn (keyn (keyn (keyn (keyn (keyn (keyn (keyn) c v) c v) c v)
        Dictionary_n* dico = new Dictionary_n;
        double k;
        for (long i = 0; i < keyvalues.size(); i++) {
            k = keyvalues[i]->asNumber();
            e = dico->dictionary[k];
            if (e != NULL)
                e->decrementstatus(1, false);
            dico->dictionary[k] = valuevalues[i];
            valuevalues[i]->incrementstatus(1, false);
            keyvalues[i]->release();
        }
        return dico;
    }

    Dictionary* dico = new Dictionary;
    wstring k;
    for (long i = 0; i < keyvalues.size(); i++) {
        k = keyvalues[i]->asString(lisp);
        e = dico->dictionary[k];
        if (e != NULL)
            e->decrementstatus(1, false);
        dico->dictionary[k] = valuevalues[i];
        valuevalues[i]->incrementstatus(1, false);
        keyvalues[i]->release();
    }
    return dico;
}

//------------------------------------------------------------------------------------------

Element* Element::loop(LispE* lisp, short label,  List* code) {
    return null_;
}

Element* String::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    Element* element;
    long sz = code->liste.size();
    long i = 0;
    wstring localvalue;
    long szc = content.size();
    while (i < szc) {
        lisp->handlingutf8->getchar(content, localvalue, i, szc);
        element = lisp->provideString(localvalue);
        lisp->recording(element, label);
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            //the break is local, the return is global to a function
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* List::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    Element* element;
    long sz = code->liste.size();
    for (long i = 0; i < liste.size(); i++) {
        element = liste[i]->copying(false);
        lisp->recording(element, label);
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }        
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Infiniterange::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    double value = initial_value;
    Element* element;
    
    while (!lisp->hasStopped()) {
        element = lisp->provideNumber(value);
        lisp->recording(element, label);
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz  && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
        value += increment;
    }
    return e;
}

Element* InfiniterangeInteger::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    long value = initial_value;
    Element* element;
    
    while (!lisp->hasStopped()) {
        element = lisp->provideInteger(value);
        lisp->recording(element, label);
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz  && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
        value += increment;
    }
    return e;
}


Element* Infinitelist::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    Element* element = value->eval(lisp);
    lisp->recording(element, label);
    
    while (!lisp->hasStopped()) {
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz  && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Cyclelist::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    Element* element = value->eval(lisp);
    if (!element->isList()) {
        element->release();
        throw new Error("Error: we can only cycle on list");
    }
    List* values = (List*)element;
    long sze = values->liste.size();
    long i = 0;
    //We then execute our instructions
    while (!lisp->hasStopped()) {
        element = values->liste[i]->copying(false);
        lisp->recording(element, label);
        if ((++i) >= sze)
            i = 0;
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}



Element* Dictionary::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    Element* element;
    long sz = code->liste.size();
    //We record the keys first, in  case the dictionary is changed
    //in the following instructions
    vector<wstring> _keys;
    for (auto& a: dictionary)
        _keys.push_back(a.first);
    for (auto& a_key : _keys) {
        element = lisp->provideString(a_key);
        lisp->recording(element, label);
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

Element* Dictionary_n::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    Element* element;
    long sz = code->liste.size();
    //We record the keys first, in  case the dictionary is changed
    //in the following instructions
    vector<double> _keys;
    for (auto& a: dictionary)
        _keys.push_back(a.first);
    for (auto& a_key : _keys) {
        element = lisp->provideNumber(a_key);
        lisp->recording(element, label);
        e = null_;
        //We then execute our instructions
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
    }
    return e;
}

//------------------------------------------------------------------------------------------
List* s_findall(LispE* lisp, wstring& s, wstring& sub, long from) {
    long sz = sub.size();
    if (!sz)
        return emptylist_;
    
    long pos = s.find(sub, from);
    if (pos == -1)
        return emptylist_;
    List* liste = new List;
    Element* e;
    while (pos != -1) {
        e = lisp->provideNumber(pos);
        liste->append(e);
        pos=s.find(sub,pos+sz);
    }
    return liste;
}

//------------------------------------------------------------------------------------------
Element* Element::insert(LispE* lisp, Element* e, long idx) {
    return null_;
}

Element* String::insert(LispE* lisp, Element* e, long idx) {
    wstring res;
    if (idx >= content.size())
        res = content + e->asString(lisp);
    else {
        if (idx < 0)
            throw new Error("Error: Wrong index in 'insert'");
        res = content;
        res.insert(idx, e->asString(lisp));
    }
    return lisp->provideString(res);
}

Element* List::insert(LispE* lisp, Element* e, long idx) {
    if (idx < 0)
        throw new Error("Error: Wrong index in 'insert'");
 
    e = e->copying(false);
    List* l = (List*)duplicate_constant_container();
    l->liste.insert(idx, e);        
    return l;
}

Element* List::unique(LispE* lisp) {
    if (liste.size() == 0)
        return this;
    
    List* list = new List;
    long i, j;
    bool found;
    list->append(liste[0]->copying(false));
    for (i = 1; i < liste.size(); i++) {
        found = true;
        for (j = 0; j < list->liste.size(); j++) {
            if (liste[i]->unify(lisp, list->liste[j], false)) {
                found = false;
                break;
            }
        }
        if (found)
            list->append(liste[i]->copying(false));
    }
    return list;
}

//------------------------------------------------------------------------------------------

Element* Element::thekeys(LispE* lisp) {
    return emptylist_;
}

Element* Dictionary::thekeys(LispE* lisp) {
    List* liste = new List;
    Element* e;
    wstring keyvalue;
    for (auto& a: dictionary) {
        keyvalue = a.first;
        e = lisp->provideString(keyvalue);
        liste->append(e);
    }
    return liste;
}

Element* Dictionary_n::thekeys(LispE* lisp) {
    List* liste = new List;
    Element* e;
    for (auto& a: dictionary) {
        e = lisp->provideNumber(a.first);
        liste->append(e);
    }
    return liste;
}

Element* Element::thevalues(LispE* lisp) {
    return emptylist_;
}

Element* Dictionary::thevalues(LispE* lisp) {
    List* liste = new List;
    for (auto& a: dictionary) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

Element* Dictionary_n::thevalues(LispE* lisp) {
    List* liste = new List;
    for (auto& a: dictionary) {
        liste->append(a.second->copying(false));
    }
    return liste;
}

//------------------------------------------------------------------------------------------
Element* Element::Boolean(LispE* lisp) {
    return booleans_[Boolean()];
}

//------------------------------------------------------------------------------------------
Element* Element::search_element(LispE* lisp, Element* valeur, long idx) {
    return null_;
}

Element* String::search_element(LispE* lisp, Element* valeur, long idx) {
    wstring val = valeur->asString(lisp);
    idx =  content.find(val, idx);
    if (idx == -1)
        return null_;
    return lisp->provideInteger(idx);
}

Element* List::search_element(LispE* lisp, Element* valeur, long idx) {
    for (long i = idx; i < liste.size(); i++) {
        if (liste[i]->equal(lisp, valeur) == true_)
            return lisp->provideInteger(i);
    }
    return null_;
}

Element* Dictionary::search_element(LispE* lisp, Element* valeur, long idx) {
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            wstring keyvalue = a.first;
           return lisp->provideString(keyvalue);
        }
    }
    return null_;
}

Element* Dictionary_n::search_element(LispE* lisp, Element* valeur, long idx) {
    for (auto& a : dictionary) {
       if (a.second->equal(lisp, valeur) == true_)
           return lisp->provideNumber(a.first);
    }
    return null_;
}

Element* Element::checkkey(LispE* lisp, Element* e) {
    return null_;
}

//------------------------------------------------------------------------------------------

Element* Dictionary::checkkey(LispE* lisp, Element* e) {
    try {
        return dictionary.at(e->asString(lisp));
    }
    catch(const std::out_of_range& oor) {
        return null_;
    }
}

Element* Dictionary_n::checkkey(LispE* lisp, Element* e) {
    try {
        return dictionary.at(e->asNumber());
    }
    catch(const std::out_of_range& oor) {
        return null_;
    }
}


//------------------------------------------------------------------------------------------
Element* Element::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    return emptylist_;
}

Element* String::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    wstring val = valeur->asString(lisp);
    idx =  content.find(val, idx);
    return s_findall(lisp,content, val, idx);
}

Element* List::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    List* l = new List;
    for (long i = idx; i < liste.size(); i++) {
        if (liste[i]->equal(lisp, valeur) == true_) {
            l->append(lisp->provideInteger(i));
        }
    }
    if (l->liste.size() == 0) {
        delete l;
        return emptylist_;
    }
    return l;
}

Element* Dictionary::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    List* l = new List;
    wstring keyvalue;
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            keyvalue = a.first;
           l->append(lisp->provideString(keyvalue));
        }
    }
    if (l->liste.size() == 0) {
        delete l;
        return emptylist_;
    }
    return l;
}

Element* Dictionary_n::search_all_elements(LispE* lisp, Element* valeur, long idx) {
    List* l = new List;
    for (auto& a : dictionary) {
       if (a.second->equal(lisp, valeur) == true_)
           l->append(lisp->provideNumber(a.first));
    }
    if (l->liste.size() == 0) {
        delete l;
        return emptylist_;
    }
    return l;
}

//------------------------------------------------------------------------------------------
Element* Element::search_reverse(LispE* lisp, Element* valeur, long idx) {
    return minusone_;
}

Element* String::search_reverse(LispE* lisp, Element* valeur, long idx) {
    wstring val = valeur->asString(lisp);
    idx =  content.rfind(val, idx);
    return lisp->provideInteger(idx);
}

Element* List::search_reverse(LispE* lisp, Element* valeur, long idx) {
    for (long i = liste.size() - 1; i >= idx; i--) {
        if (liste[i]->equal(lisp, valeur) == true_)
            return lisp->provideInteger(i);
    }
    return minusone_;
}

Element* Dictionary::search_reverse(LispE* lisp, Element* valeur, long idx) {
    for (auto& a : dictionary) {
        if (a.second->equal(lisp, valeur) == true_) {
            wstring keyvalue = a.first;
           return lisp->provideString(keyvalue);
        }
    }
    return emptystring_;
}

Element* Dictionary_n::search_reverse(LispE* lisp, Element* valeur, long idx) {
    for (auto& a : dictionary) {
       if (a.second->equal(lisp, valeur) == true_)
           return lisp->provideNumber(a.first);
    }
    return minusone_;
}


//------------------------------------------------------------------------------------------
Element* Element::reverse(LispE* lisp, bool duplique) {
    return emptylist_;
}

Element* String::reverse(LispE* lisp, bool duplique) {
    wstring resultat;
    for (long i = content.size()-1; i >= 0; i--)
        resultat += content[i];

    if (duplique)
        return lisp->provideString(resultat);

    content = resultat;
    return this;
}

Element* List::reverse(LispE* lisp, bool duplique) {
    if (liste.size() <= 1)
        return this;
    
    if (duplique) {
        List* l = new List;
        for (long i = liste.size()-1; i >= 0; i--)
            l->append(liste[i]->copying(false));
        return l;
    }

    liste.reverse();
    return this;
}

Element* Dictionary::reverse(LispE* lisp, bool duplique) {
    Dictionary* dico = new Dictionary;
    
    wstring k;
    Element* e;
    for (auto& a: dictionary) {
        k = a.second->asString(lisp);
        e = dico->dictionary[k];
        if (e == NULL) {
            e = new List;
            dico->dictionary[k] = e;
            e->incrementstatus(1, false);
        }
        k = a.first;
        e->append(lisp->provideString(k));
    }
    return dico;
}

Element* Dictionary_n::reverse(LispE* lisp, bool duplique) {
    Dictionary_n* dico = new Dictionary_n;
    
    double k;
    Element* e;
    for (auto& a: dictionary) {
        k = a.second->asNumber();
        e = dico->dictionary[k];
        if (e == NULL) {
            e = new List;
            dico->dictionary[k] = e;
            e->incrementstatus(1, false);
        }
        e->append(lisp->provideNumber(a.first));
    }
    return dico;
}
//------------------------------------------------------------------------------------------
Element* Element::protected_index(LispE* lisp,Element*) {
    return null_;
}

Element* Element::protected_index(LispE* lisp,long i) {
    return null_;
}

Element* Element::protected_index(LispE* lisp, wstring&) {
    return null_;
}

Element* Element::protected_index(LispE* lisp, double k) {
    return null_;
}

Element* String::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    return null_;
}

Element* List::protected_index(LispE* lisp,long i) {
    if (i >= 0 && i < liste.size())
        return liste[i];
    return null_;
}
//------------------------------------------------------------------------------------------
Element* Element::last_element(LispE* lisp) {
    return null_;
}

Element* String::last_element(LispE* lisp) {
    if (!content.size())
        return null_;
    wchar_t c = content.back();
    return lisp->provideString(c);
}

Element* List::last_element(LispE* lisp) {
    if (!liste.size())
        return null_;
    return liste.back();
}

//------------------------------------------------------------------------------------------
Element* Element::value_on_index(LispE* lisp, long i) {
    return null_;
}

Element* List::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < liste.size())
        return liste[i]->copying(false);
    return null_;
}

Element* String::value_on_index(LispE* lisp, long i) {
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    return null_;
}

//------------------------------------------------------------------------------------------

Element* Element::value_on_index(wstring& k, LispE* lisp) {
    return null_;
}

Element* Dictionary::value_on_index(wstring& k, LispE* lisp) {
    try {
        return dictionary.at(k)->copying(false);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

Element* Dictionary::protected_index(LispE* lisp, wstring& k) {
    try {
        return dictionary.at(k);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

//------------------------------------------------------------------------------------------

Element* Element::value_on_index(double k, LispE* lisp) {
    return null_;
}

Element* Dictionary_n::value_on_index(double k, LispE* lisp) {
    try {
        return dictionary.at(k)->copying(false);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

Element* Dictionary_n::protected_index(LispE* lisp, double k) {
    try {
        return dictionary.at(k);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

//------------------------------------------------------------------------------------------

Element* Element::value_on_index(LispE* lisp, Element* i) {
    return null_;
}

Element* List::value_on_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return liste[i]->copying(false);
    
    return null_;
}

Element* String::value_on_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = content.size() + i;
    
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    return null_;
}

Element* Dictionary::value_on_index(LispE* lisp, Element* idx) {
    wstring k = idx->asString(lisp);
    try {
        return dictionary.at(k)->copying(false);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

Element* Dictionary_n::value_on_index(LispE* lisp, Element* idx) {
    try {
        return dictionary.at(idx->checkNumber(lisp))->copying(false);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}
//------------------------------------------------------------------------------------------

Element* List::protected_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return liste[i];
    
    return null_;
}

Element* String::protected_index(LispE* lisp, Element* idx) {
    long i = idx->checkInteger(lisp);

    if (i < 0)
        i = content.size() + i;
    
    if (i >= 0 && i < content.size())
        return lisp->provideString(content[i]);
    return null_;
}

Element* Dictionary::protected_index(LispE* lisp, Element* idx) {
    wstring k = idx->asString(lisp);
    try {
        return dictionary.at(k);
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

Element* Dictionary_n::protected_index(LispE* lisp, Element* idx) {
    try {
        return dictionary.at(idx->checkNumber(lisp));
    }
    catch (const std::out_of_range& oor) {
        return null_;
    }
}

//------------------------------------------------------------------------------------------
Element* Element::join_in_list(LispE* lisp, wstring& sep) {
    throw new Error("Error: 'join' can only be used for lists");
}

Element* List::join_in_list(LispE* lisp, wstring& sep) {
    wstring str;
    wstring beg;
    for (long i = 0; i < liste.size(); i++) {
        str += beg;
        beg = sep;
        str += liste[i]->asString(lisp);
    }
    return lisp->provideString(str);
}

Element* Dictionary::join_in_list(LispE* lisp, wstring& sep) {
    if (sep==L"")
        sep = L",";
    wstring str;
    wstring beg;
    for (auto& a: dictionary) {
        str += beg;
        beg = sep;
        str += a.first;
        str += L":";
        str += a.second->asString(lisp);
    }
    return lisp->provideString(str);
}

Element* Dictionary_n::join_in_list(LispE* lisp, wstring& sep) {
    if (sep==L"")
        sep = L",";

    wstring str;
    wstring beg;
    for (auto& a: dictionary) {
        str += beg;
        beg = sep;
        str += a.first;
        str += L":";
        str += a.second->asString(lisp);
    }
    return lisp->provideString(str);
}


//------------------------------------------------------------------------------------------
Element* Element::charge(LispE* lisp, string chemin) {
    return emptyatom_;
}
//------------------------------------------------------------------------------------------
Element* Element::replace(LispE* lisp, long i, Element* e) {
    throw new Error("Error: cannot modify this element");
}

Element* String::replace(LispE* lisp, long i, Element* e) {
    if (i < 0 || i >= content.size())
        throw new Error("Error: cannot modify at this position");
    wstring c = content.substr(0, i);
    c += e->asString(lisp);
    c += content.substr(i+1, content.size());
    return lisp->provideString(c);
}
//------------------------------------------------------------------------------------------

wstring Infinitelist::asString(LispE* lisp) {
    wstring tampon(L"(");
    tampon += lisp->asString(l_repeat);
    tampon += L" ";
    tampon += value->asString(lisp);
    tampon += L")";
    return tampon;
}

wstring Cyclelist::asString(LispE* lisp) {
    wstring tampon(L"(");
    tampon += lisp->asString(l_cycle);
    tampon += L" ";
    tampon += value->asString(lisp);
    tampon += L")";
    return tampon;
}

wstring Infiniterange::asString(LispE* lisp) {
    std::wstringstream str;
    str << L"(" << lisp->asString(l_irange) << " " << initial_value << " " << increment << L")";
    return str.str();
}

wstring InfiniterangeInteger::asString(LispE* lisp) {
    std::wstringstream str;
    str << L"(" << lisp->asString(l_irange) << " " << initial_value << " " << increment << L")";
    return str.str();
}

wstring Error::asString(LispE* lisp) {
    if (lisp->delegation->i_current_file != -1) {
        string s = lisp->delegation->allfiles_names[lisp->delegation->i_current_file];
        wstring w;
        s_utf8_to_unicode(w, USTR(s), s.size());
        std::wstringstream msg;
        msg << message << L" line: " << lisp->delegation->i_current_line << L" in: " << w;
        return msg.str();
    }
    else
        return message;
}

wstring Atom::asString(LispE* lisp) {
    return lisp->asString(atome);
}

wstring Atom::jsonString(LispE* lisp) {
    return wjsonstring(lisp->asString(atome));
}


wstring Instruction::asString(LispE* lisp) {
    return lisp->asString(type);
}

wstring Operator::asString(LispE* lisp) {
    return lisp->asString(type);
}

//------------------------------------------------------------------------------------------
Element* Element::equal(LispE* lisp, Element* e) {
    return booleans_[(e==this)];
}

Element* Atom::equal(LispE* lisp, Element* e) {
    return booleans_[(e->label() == atome)];
}

Element* Maybe::equal(LispE* lisp, Element* e) {
    return booleans_[(e->label() == t_error)];
}

Element* List::equal(LispE* lisp, Element* e) {
    return booleans_[e->isList() && liste.equal(((List*)e)->liste)];
}

Element* Dictionary::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_dictionary && e->size() == 0 && dictionary.size() == 0) || e == this)];
}

Element* Dictionary_n::equal(LispE* lisp, Element* e) {
    return booleans_[((e->type == t_dictionaryn && e->size() == 0 && dictionary.size() == 0) || e== this)];
}

Element* String::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_string && content == e->asString(lisp))];
}

Element* Number::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && number == e->asNumber())];
}

Element* Integer::equal(LispE* lisp, Element* e) {
    return booleans_[(e->isNumber() && integer == e->asInteger())];
}
//------------------------------------------------------------------------------------------
Element* Element::less(LispE* lisp, Element* e) {
    return false_;
}

Element* Element::lessorequal(LispE* lisp, Element* e){
    return false_;
}

Element* Element::more(LispE* lisp, Element* e) {
    return false_;
}

Element* Element::moreorequal(LispE* lisp, Element* e) {
    return false_;
}

Element* String::less(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_string && content < e->asString(lisp))];
}

Element* String::lessorequal(LispE* lisp, Element* e){
    return booleans_[(e->type == t_string && content <= e->asString(lisp))];
}

Element* String::more(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_string && content > e->asString(lisp))];
}

Element* String::moreorequal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_string && content >= e->asString(lisp))];
}

Element* Number::less(LispE* lisp, Element* e) {
    return booleans_[number < e->checkNumber(lisp)];
}

Element* Number::lessorequal(LispE* lisp, Element* e){
    return booleans_[number <= e->checkNumber(lisp)];
}

Element* Number::more(LispE* lisp, Element* e) {
    return booleans_[number > e->checkNumber(lisp)];
}

Element* Number::moreorequal(LispE* lisp, Element* e) {
    return booleans_[number >= e->checkNumber(lisp)];
}

Element* Integer::less(LispE* lisp, Element* e) {
    return booleans_[integer < e->checkInteger(lisp)];
}

Element* Integer::lessorequal(LispE* lisp, Element* e){
    return booleans_[integer <= e->checkInteger(lisp)];
}

Element* Integer::more(LispE* lisp, Element* e) {
    return booleans_[integer > e->checkInteger(lisp)];
}

Element* Integer::moreorequal(LispE* lisp, Element* e) {
    return booleans_[integer >= e->checkInteger(lisp)];
}

//------------------------------------------------------------------------------------------
Element* Element::plus(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '+' with these operands");
}

Element* Element::bit_and(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '&' with these operands");
}

Element* Element::bit_or(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '|' with these operands");
}

Element* Element::bit_xor(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '^' with these operands");
}

Element* Element::minus(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '-' with these operands");
}

Element* Element::multiply(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '*' with these operands");
}

Element* Element::divide(LispE* lisp, Element* e)  {
    throw new Error("Error: cannot evaluate '/' with these operands");
}

Element* Element::mod(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '%' with these operands");
}

Element* Element::power(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '^^' with these operands");
}

Element* Element::leftshift(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '<<' with these operands");
}

Element* Element::rightshift(LispE* lisp, Element* e) {
    throw new Error("Error: cannot evaluate '>>' with these operands");
}

Element* String::plus(LispE* lisp, Element* e) {
    if (status != s_constant) {
        content += e->asString(lisp);
        return this;
    }
    wstring c = content + e->asString(lisp);
    return lisp->provideString(c);
}

double Element::checkNumber(LispE* lisp) {
    wstring s = L"Error: cannot use this element in an arithmetic expression: '";
    s += asString(lisp);
    s += L"'";
    throw new Error(s);
}

long Element::checkInteger(LispE* lisp) {
    wstring s = L"Error: cannot use this element in an arithmetic expression: '";
    s += asString(lisp);
    s += L"'";
    throw new Error(s);
}

Element* Number::plus(LispE* lisp, Element* e) {
    if (status != s_constant) {
        number += e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(number+e->checkNumber(lisp));
}

Element* Number::minus(LispE* lisp, Element* e) {
    if (status != s_constant) {
        number -= e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(number-e->checkNumber(lisp));
}

Element* Number::multiply(LispE* lisp, Element* e) {
    if (status != s_constant) {
        number *= e->checkNumber(lisp);
        return this;
    }
    return lisp->provideNumber(number*e->checkNumber(lisp));
}

Element* Number::divide(LispE* lisp, Element* e) {
    double v = e->checkNumber(lisp);
    if (v == 0)
        throw new Error("Error: division by zero");
    if (status != s_constant) {
        number /= v;
        return this;
    }
    return lisp->provideNumber(number/v);
}

Element* Number::mod(LispE* lisp, Element* e) {
    long v = e->checkNumber(lisp);
    if (v == 0)
        throw new Error("Error: division by zero");
    if (status != s_constant) {
        number = (long)number % v;
        return this;
    }
    return lisp->provideInteger((long)number%v);
}

Element* Number::power(LispE* lisp, Element* e) {
    if (status != s_constant) {
        number = pow(number, e->checkNumber(lisp));
        return this;
    }
    return lisp->provideNumber(pow(number, e->checkNumber(lisp)));
}

Element* Number::bit_and(LispE* lisp, Element* e)  {
    double64 d(number);
    d.bits &= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::bit_or(LispE* lisp, Element* e)  {
    double64 d(number);
    d.bits |= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::bit_xor(LispE* lisp, Element* e)  {
    double64 d(number);
    d.bits ^= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::leftshift(LispE* lisp, Element* e)  {
    double64 d(number);
    d.bits <<= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Number::rightshift(LispE* lisp, Element* e)  {
    double64 d(number);
    d.bits >>= e->checkInteger(lisp);
    if (status != s_constant) {
        number = d.v;
        return this;
    }

    return lisp->provideNumber(d.v);
}

Element* Integer::plus(LispE* lisp, Element* e) {
    if (e->type == t_number) {
        double v = (double)integer + e->checkNumber(lisp);
        release();
        return lisp->provideNumber(v);
    }
    if (status != s_constant) {
        integer += e->checkInteger(lisp);
        return this;
    }
        
    return lisp->provideInteger(integer+e->checkInteger(lisp));
}

Element* Integer::minus(LispE* lisp, Element* e) {
    if (e->type == t_number) {
        double v = (double)integer + e->checkNumber(lisp);
        release();
        return lisp->provideNumber(v);
    }
    if (status != s_constant) {
        integer -= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer-e->checkInteger(lisp));
}

Element* Integer::multiply(LispE* lisp, Element* e) {
    if (e->type == t_number) {
        double v = (double)integer + e->checkNumber(lisp);
        release();
        return lisp->provideNumber(v);
    }
    if (status != s_constant) {
        integer *= e->asInteger();
        return this;
    }
    return lisp->provideInteger(integer*e->asInteger());
}

Element* Integer::divide(LispE* lisp, Element* e) {
    double v = e->checkNumber(lisp);
    if (v == 0)
        throw new Error("Error: division by zero");
    v = (double)integer/v;
    release();
    if (v == (long)v)
        return lisp->provideInteger(v);
    return lisp->provideNumber(v);
}


Element* Integer::mod(LispE* lisp, Element* e) {
    long v = e->checkInteger(lisp);
    if (v == 0)
        throw new Error("Error: division by zero");
    if (status != s_constant) {
        integer %= v;
        return this;
    }
    return lisp->provideInteger(integer%v);
}

Element* Integer::power(LispE* lisp, Element* e) {
    double v = pow((double)integer, e->checkNumber(lisp));
    release();
    return lisp->provideNumber(v);
}

Element* Integer::bit_and(LispE* lisp, Element* e)  {
    if (status != s_constant) {
        integer &= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer&e->checkInteger(lisp));
}

Element* Integer::bit_or(LispE* lisp, Element* e)  {
    if (status != s_constant) {
        integer |= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer|e->checkInteger(lisp));
}

Element* Integer::bit_xor(LispE* lisp, Element* e)  {
    if (status != s_constant) {
        integer ^= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer^e->checkInteger(lisp));
}


Element* Integer::leftshift(LispE* lisp, Element* e)  {
    if (status != s_constant) {
        integer <<= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer<<e->checkInteger(lisp));
}


Element* Integer::rightshift(LispE* lisp, Element* e)  {
    if (status != s_constant) {
        integer >>= e->checkInteger(lisp);
        return this;
    }
    return lisp->provideInteger(integer>>e->checkInteger(lisp));
}

//------------------------------------------------------------------------------------------
Element* Element::extraction(LispE* lisp, List* l) {
    return null_;
}

Element* List::extraction(LispE* lisp, List* l) {
    long depuis;
    l->evalAsInteger(2, lisp, depuis);
    if (depuis >= 0) {
        if (depuis >= liste.size())
            return null_;
    }
    else {
        //We start from the end...
        depuis = liste.size() + depuis;
        if (depuis < 0)
            return null_;
    }
    if (l->size() == 3) {
        //On returns only one element
        return liste[depuis]->copying(false);
    }
    long upto;
    l->evalAsInteger(3, lisp, upto);
    if (upto >= 0) {
        if (upto >= liste.size())
            upto = liste.size();
    }
    else {
        //We start from the end...
        upto = liste.size() + upto;
        if (upto < 0)
            return null_;
    }
    if (upto < depuis) {
        return null_;
    }
    l = new List;
    for (;depuis < upto; depuis++)
        l->append(liste[depuis]->copying(false));
    return l;
}

Element* String::extraction(LispE* lisp, List* liste) {
    Element* e_from = liste->liste[2]->eval(lisp);

    long from;
    bool firstisString = false;
    if (e_from->type == t_string) {
        wstring ch = e_from->asString(lisp);
        from = content.find(ch);
        if (from == -1)
            return emptystring_;
        firstisString = true;
    }
    else {
        from = e_from->asInteger();
        if (from < 0)
            from = content.size() + from;
    }
    e_from->release();

    if (from < 0 || from >= content.size())
        return emptystring_;

    if (liste->size() == 3) {
        //Only one element is returned
        return lisp->provideString(content[from]);
    }

    Element* e_upto = liste->liste[3]->eval(lisp);
    long upto;

    if (e_upto->type == t_string) {
        wstring ch = e_upto->asString(lisp);
        upto = content.find(ch, from + 1);
        if (upto == -1)
            return emptystring_;
        //All characters are integrated
        upto += ch.size();
    }
    else {
        upto = e_upto->asInteger();
        if (firstisString) {
            if (upto < 0)
                return emptystring_;
            //in this case upto is a number of characters, not a position
            upto += from;
        }
        else {
            if (upto < 0) {
                //We start from the end...
                upto = content.size() + upto;
            }
        }
    }

    e_upto->release();
    if (upto <= from)
        return emptystring_;

    if (upto > content.size())
        upto = content.size();
    wstring remplace = content.substr(from, upto - from);
    return lisp->provideString(remplace);
}
//------------------------------------------------------------------------------------------
Element* List::duplicate_constant_container(bool pair) {
    if (status == s_constant) {
        List* l;
        if (pair)
            l =  new Pair();
        else
            l = new List();
        for (long i = 0; i < liste.size(); i++) {
            l->append(liste[i]->copying(false));
        }
        return l;
    }
    return this;
}
//------------------------------------------------------------------------------------------
//For running car/cdr, everything that is not List is an error
Element* Element::cadr(LispE* lisp, Element*) {
    throw new Error("Error: You can only apply 'car/cdr' to a list");
}

Element* Element::car(LispE*) {
    throw new Error("Error: You can only apply 'car' to a list");
}

Element* Element::cdr(LispE*) {
    throw new Error("Error: 'cdr' can only be applied to a list");
}

Element* String::car(LispE* lisp) {
    if (content.size() == 0)
        throw new Error("Error: Empty string");
    return lisp->provideString(content[0]);
}

Element* String::cdr(LispE* lisp) {
    if (content.size() == 0)
        throw new Error("Error: Empty string");
    wstring w = content.substr(1, content.size()-1);
    return lisp->provideString(w);
}

Element* Cadr::cadr(LispE* lisp, Element* e) {
    long pos = 0;
    long sz = e->size();
    bool pair = (e->type == t_pair);
    
    for (long i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            e = e->protected_index(lisp, pos);
            if (e == null_)
                throw new Error("Error: You can only apply 'car/cdr' to a list");
         
            pair = (e->type == t_pair);
            sz = e->size();
            pos = 0;
        }
        else {
            if (pos == sz)
                throw new Error("Error: You can only apply 'car/cdr' to a list");
            pos++;
        }
    }
    
    if (pos) {
        if (pos == sz)
            return null_;
        if (pair) {
            //The last one...
            if (pos == sz - 1)
                return e->index(pos);
            return new Pair((Pair*)e, pos);
        }
        else
            return new List((List*)e, pos);
    }

    return e;
}


Element* List::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return liste[0];
}

Element* List::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    return new List(this, 1);
}

Element* Pair::cdr(LispE* lisp) {
    long sz = liste.size();
    if (!sz)
        return null_;
    
    if (sz == 2)
        return liste.back();

    return new Pair(this, 1);
}

Element* Infiniterange::car(LispE* lisp) {
    return lisp->provideNumber(initial_value);
}

Element* Infiniterange::cdr(LispE* lisp) {
    return new Infiniterange(initial_value+increment, increment);
}

Element* InfiniterangeInteger::car(LispE* lisp) {
    return lisp->provideInteger(initial_value);
}

Element* InfiniterangeInteger::cdr(LispE* lisp) {
    return new InfiniterangeInteger(initial_value+increment, increment);
}
//------------------------------------------------------------------------------------------
