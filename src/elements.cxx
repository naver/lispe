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
#include "elements.h"
#include "tools.h"
#include <math.h>
#include <algorithm>

//------------------------------------------------------------------------------------------
Infinitelist::Infinitelist(LispE* lisp) : Element(t_list) {
    value = null_;
}

Cyclelist::Cyclelist(LispE* lisp) : Element(t_list) {
    value = null_;
}

//------------------------------------------------------------------------------------------

string Element::prettify(LispE* lisp) {
    string code = toString(lisp);
    
    Tokenizer parse;
    lisp->segmenting(code, parse);
    string jolly;
    long sz = parse.types.size();
    bool inbrace = false;
    for (long i = 0; i <sz; i++) {
        switch (parse.types[i]) {
            case e_opening_parenthesis: {
                if (parse.types[i+1] == e_closing_parenthesis) {
                    if (i && !inbrace)
                        jolly += " ";
                    jolly += "()";
                    i++;
                }
                else {
                    if (i)
                        jolly += "\n";
                    jolly += "(\n";
                }
                inbrace = false;
                break;
            }
            case e_closing_parenthesis: {
                if (i)
                    jolly += "\n";
                jolly += ")\n";
                break;
            }
            case e_opening_brace: {
                if (parse.types[i+1] == e_closing_brace) {
                    if (i && !inbrace)
                        jolly += " ";
                    jolly += "{}";
                    i++;
                }
                else {
                    if (i)
                        jolly += "\n";
                    jolly += "{\n";
                }
                inbrace = false;
                break;
            }
            case e_closing_brace: {
                if (i)
                    jolly += "\n";
                jolly += "}\n";
                break;
            }
            case e_string:
                if (i && !inbrace)
                    jolly += " ";
                jolly += '"';
                jolly += parse.tokens[i];
                jolly += '"';
                inbrace = false;
                break;
            case e_emptystring:
                if (i && !inbrace)
                    jolly += " ";
                jolly += '"';
                jolly += '"';
                inbrace = false;
                break;
            case e_colon:
                jolly += " ";
                jolly += parse.tokens[i];
                jolly += " ";
                inbrace = true;
                break;
            default:
                if (i && !inbrace)
                    jolly += " ";                
                jolly += parse.tokens[i];
                inbrace = false;
        }
    }
    
    code = jolly;
    IndentCode(code, jolly, GetBlankSize());
    return jolly;
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
    size_t i = 0;
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
    long sze = liste.size();
    for (long i = 0; i < sze; i++) {
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
    wstring a_key;
    for (auto& a: dictionary) {
        a_key = a.first;
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
    for (auto& a: dictionary) {
        element = lisp->provideNumber(a.first);
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
 
    List* l = (List*)duplicate_constant_container();
    if (idx >= liste.size())
        l->liste.push_back(e);
    else
        l->liste.insert(l->liste.begin()+idx, e);
        
    e->incrementstatus(status+1, false);
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
    if (liste.size() == 0)
        return emptylist_;
    
    if (duplique) {
        List* l = new List;
        for (long i = liste.size()-1; i >= 0; i--)
            l->append(liste[i]->copying(false));
        return l;
    }

    vector<Element*> l;
    for (long i = liste.size()-1; i >= 0; i--)
        l.push_back(liste[i]);

    liste = l;
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
Element* Element::protected_index(LispE* lisp,long i) {
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
    return liste.back()->copying(false);
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
//------------------------------------------------------------------------------------------

Element* Element::value_on_index(LispE* lisp, Element* i) {
    return null_;
}

Element* List::value_on_index(LispE* lisp, Element* idx) {
    if (!idx->isNumber())
        throw new Error("Error: Wrong index type");
    
    long i = idx->asInteger();
    if (i < 0)
        i = liste.size() + i;
    
    if (i >= 0 && i < liste.size())
        return liste[i]->copying(false);
    
    return null_;
}

Element* String::value_on_index(LispE* lisp, Element* idx) {
    if (!idx->isNumber())
        throw new Error("Error: Wrong index type");
    
    long i = idx->asInteger();
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
    if (!idx->isNumber())
        throw new Error("Error: Wrong index type");
    
    try {
        return dictionary.at(idx->asNumber())->copying(false);
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
    for (auto& a: liste) {
        str += beg;
        beg = sep;
        str += a->asString(lisp);
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

wstring Number::asString(LispE* lisp) {
    std::wstringstream val;
    val << number;
    return val.str();
}

wstring Integer::asString(LispE* lisp) {
    std::wstringstream val;
    val << integer;
    return val.str();
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
    return booleans_[(e->isList() && e->size() == 0 && liste.size() == 0)];
}

Element* Dictionary::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_dictionary && e->size() == 0 && dictionary.size() == 0)];
}

Element* Dictionary_n::equal(LispE* lisp, Element* e) {
    return booleans_[(e->type == t_dictionaryn && e->size() == 0 && dictionary.size() == 0)];
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
    if (e->type == t_string && content < e->asString(lisp))
        return true_;
    return false_;
}

Element* String::lessorequal(LispE* lisp, Element* e){
    if (e->type == t_string && content <= e->asString(lisp))
        return true_;
    return false_;
}

Element* String::more(LispE* lisp, Element* e) {
    if (e->type == t_string && content > e->asString(lisp))
        return true_;
    return false_;
}

Element* String::moreorequal(LispE* lisp, Element* e) {
    if (e->type == t_string && content >= e->asString(lisp))
        return true_;
    return false_;
}

Element* Number::less(LispE* lisp, Element* e) {
    if (e->isNumber() && number < e->asNumber())
        return true_;
    return false_;
}

Element* Number::lessorequal(LispE* lisp, Element* e){
    if (e->isNumber() && number <= e->asNumber())
        return true_;
    return false_;
}

Element* Number::more(LispE* lisp, Element* e) {
    if (e->isNumber() && number > e->asNumber())
        return true_;
    return false_;
}

Element* Number::moreorequal(LispE* lisp, Element* e) {
    if (e->isNumber() && number >= e->asNumber())
        return true_;
    return false_;
}

Element* Integer::less(LispE* lisp, Element* e) {
    if (e->isNumber() && integer < e->asInteger())
        return true_;
    return false_;
}

Element* Integer::lessorequal(LispE* lisp, Element* e){
    if (e->isNumber() && integer <= e->asInteger())
        return true_;
    return false_;
}

Element* Integer::more(LispE* lisp, Element* e) {
    if (e->isNumber() && integer > e->asInteger())
        return true_;
    return false_;
}

Element* Integer::moreorequal(LispE* lisp, Element* e) {
    if (e->isNumber() && integer >= e->asInteger())
        return true_;
    return false_;
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
    wstring c = content + e->asString(lisp);
    return lisp->provideString(c);
}

Element* Number::plus(LispE* lisp, Element* e) {
    return lisp->provideNumber(number+e->asNumber());
}

Element* Number::minus(LispE* lisp, Element* e) {
    return lisp->provideNumber(number-e->asNumber());
}

Element* Number::multiply(LispE* lisp, Element* e) {
    return lisp->provideNumber(number*e->asNumber());
}

Element* Number::divide(LispE* lisp, Element* e) {
    double v = e->asNumber();
    if (v == 0)
        throw new Error("Error: division by zero");
    return lisp->provideNumber(number/v);
}

Element* Number::mod(LispE* lisp, Element* e) {
    long v = e->asInteger();
    if (v == 0)
        throw new Error("Error: division by zero");
    return lisp->provideInteger((long)number%v);
}

Element* Number::power(LispE* lisp, Element* e) {
    return lisp->provideNumber(pow(number, e->asNumber()));
}

Element* Number::bit_and(LispE* lisp, Element* e)  {
    return lisp->provideNumber((long)number & e->asInteger());
}

Element* Number::bit_or(LispE* lisp, Element* e)  {
    return lisp->provideNumber((long)number | e->asInteger());
}

Element* Number::bit_xor(LispE* lisp, Element* e)  {
    return lisp->provideNumber((long)number ^ e->asInteger());
}

Element* Number::leftshift(LispE* lisp, Element* e)  {
    return lisp->provideNumber((long)number << e->asInteger());
}

Element* Number::rightshift(LispE* lisp, Element* e)  {
    return lisp->provideNumber((long)number >> e->asInteger());
}

Element* Integer::plus(LispE* lisp, Element* e) {
    if (e->type == t_number) {
        return lisp->provideNumber((double)integer+e->asNumber());
    }
    return lisp->provideInteger(integer+e->asInteger());
}

Element* Integer::minus(LispE* lisp, Element* e) {
    if (e->type == t_number) {
        return lisp->provideNumber((double)integer-e->asNumber());
    }
    return lisp->provideInteger(integer-e->asInteger());
}

Element* Integer::multiply(LispE* lisp, Element* e) {
    if (e->type == t_number) {
        return lisp->provideNumber((double)integer*e->asNumber());
    }
    return lisp->provideInteger(integer*e->asInteger());
}

Element* Integer::divide(LispE* lisp, Element* e) {
    double v = e->asNumber();
    if (v == 0)
        throw new Error("Error: division by zero");
    v = (double)integer/v;
    if (v == (long)v)
        return lisp->provideInteger(v);
    return lisp->provideNumber(v);
}


Element* Integer::mod(LispE* lisp, Element* e) {
    long v = e->asInteger();
    if (v == 0)
        throw new Error("Error: division by zero");
    return lisp->provideInteger(integer%v);
}

Element* Integer::power(LispE* lisp, Element* e) {
    return lisp->provideNumber(pow((double)integer, e->asNumber()));
}

Element* Integer::bit_and(LispE* lisp, Element* e)  {
    return lisp->provideInteger(integer&e->asInteger());
}

Element* Integer::bit_or(LispE* lisp, Element* e)  {
    return lisp->provideInteger(integer|e->asInteger());
}

Element* Integer::bit_xor(LispE* lisp, Element* e)  {
    return lisp->provideInteger(integer^e->asInteger());
}


Element* Integer::leftshift(LispE* lisp, Element* e)  {
    return lisp->provideInteger(integer<<e->asInteger());
}


Element* Integer::rightshift(LispE* lisp, Element* e)  {
    return lisp->provideInteger(integer>>e->asInteger());
}

Element* List::divide(LispE* lisp, bool local)  {
    long listsize = liste.size();
    if (listsize <= 2)
        throw new Error("Error: Missing arguments for '/'");

    Element* base = liste[1]->eval(lisp);
    Element* element = null_;
    try {
        for (long i = 2; i < listsize; i++) {
            element->release();
            element = null_;
            element = liste[i]->eval(lisp);
            base = base->divide(lisp, element);
        }
    }
    catch(Error* err) {
        element->release();
        throw err;
    }

    element->release();
    
    if (local) {
        short label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(base, label);
    }
    return base;

}

Element* List::mod(LispE* lisp, bool local) {
    long listsize = liste.size();
    if (listsize <= 2)
        throw new Error("Error: Missing Arguments for '%'");

    Element* base = liste[1]->eval(lisp);
    Element* element = null_;
    try {
        for (long i = 2; i < listsize; i++) {
            element->release();
            element = null_;
            element = liste[i]->eval(lisp);
            base = base->mod(lisp, element);
        }
    }
    catch(Error* err) {
        element->release();
        throw err;
    }

    element->release();
    
    if (local) {
        short label = liste[1]->label();
        if (label > l_final)
            return lisp->recording(base, label);
    }
    return base;
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
    for (long i = action.size() - 1; i>= 0; i--) {
        if (action[i] == 'a') {
            e = e->protected_index(lisp, pos);
            if (e == null_)
                throw new Error("Error: You can only apply 'car/cdr' to a list");
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
        
        Element* res = new List;
        for (;pos < sz; pos++) {
            res->append(e->value_on_index(lisp, pos));
        }
        
        return res;
    }
    return e->copying(false);
}


Element* List::car(LispE* lisp) {
    if (liste.size() == 0)
        return null_;
    return liste[0]->copying(false);
}

Element* List::cdr(LispE* lisp) {
    if (liste.size() <= 1)
        return null_;
    List* l = new List;
    long taille = liste.size();
    for (long i = 1; i < taille; i++)
        l->append(liste[i]->copying(false));
    return l;
}

Element* Pair::cdr(LispE* lisp) {
    long sz = liste.size();
    if (!sz)
        return null_;
    
    if (sz == 2)
        return liste.back()->copying(false);
    
    Pair* lp = new Pair();
    for (long i = 1; i < sz; i++)
        lp->append(liste[i]->copying(false));
    return lp;
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
