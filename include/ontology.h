/*
 *  LispE
 *
 * Copyright 2021-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  ontology.h
//
//

#ifndef ontology_h
#define ontology_h

class Granule {
    
public:
    uint64_t values;
    long rank;
    
    Granule(long);
    Granule(Granule* g=NULL);
    Granule(Granule& g);
    Granule(long,uint64_t);
};


class Concept_element {
    
public:
        
    VECTE<Granule*> elements;

    Concept_element() {}
    Concept_element(Concept_element& v);
    
    ~Concept_element();
    
    void clean() {
        elements.clean();
    }
    
    bool isempty() {
        return (!elements.last);
    }
    
    void concept_or(Granule*);
    void concept_or(long idx);

    void concept_add(Concept_element& c);
    void concept_remove(Concept_element& c, long idx);

    void concept_or(Concept_element& c, Concept_element&);
    void concept_xor(Concept_element& c, Concept_element&);
    void concept_and(Concept_element& c, Concept_element&);
    void concept_and_not(Concept_element& c, Concept_element&);

    long count();
    void remove(long b);
    void table(VECTE<long>&);

    bool intersect(Concept_element&);
    bool contain(Concept_element&);
    bool equal(Concept_element& v);
};

class Ontology;
class Concept : public Element {
public:

    Concept_element concept;
    wstring semme;
    Ontology* ontologie;
    long index;

    Concept(Ontology* h);
    Concept(Concept* c);
    Concept(Ontology* h, Concept* c, wstring& s,long i);
    Concept(Ontology* h, wstring& s,long i);

    Concept* concept_add(Concept* c) {
        if (c->ontologie != ontologie)
            throw new Error("Error: these concepts do not belong to the same ontology");
        concept.concept_add(c->concept);
        return this;
    }

    Concept* concept_remove(Concept* c);

    Concept* concept_or(Concept* c);
    Concept* concept_xor(Concept* c);
    Concept* concept_and(Concept* c);
    Concept* concept_and_not(Concept* c);
    Concept* concept_not(long mx);

    Element* intersect(LispE*, Concept* c);
    Element* contain(LispE*, Concept* c);

    bool equal(Concept* c) {
        return concept.equal(c->concept);
    }

    Element* bit_not(LispE* l);
    Element* bit_and(LispE* l, Element* e);
    Element* bit_and_not(LispE* l, Element* e);
    Element* bit_or(LispE* l, Element* e);
    Element* bit_xor(LispE* l, Element* e);
    Element* equal(LispE* lisp, Element* e);

    long size() {
        return concept.count();
    }
    
    wstring asString(LispE*);
    Element* asList();
};


class Ontology : public Element {
public:
    std::vector<wstring> concepts;
    std::unordered_map<wstring, Concept*> indexes;
    wstring name;
    Concept* absurd;
    short local_concept;

    Ontology(wstring& n, short l_hie, short l_conc) : Element(l_hie) {
        local_concept = l_conc;
        name = n;
        wstring s_absurd = L"_absurd";
        absurd = new Concept(this, s_absurd, 0);
        absurd->status = s_constant;
        concepts.push_back(s_absurd);
        indexes[s_absurd] = absurd;
    }
    
    ~Ontology() {
        for (auto& a : indexes)
            a.second->decrementstatus(1, false);
    }
    
    wstring asString(LispE*) {
        return name;
    }
    
    long size() {
        return concepts.size();
    }
    
    Concept* create(wstring& w) {
        try {
            return indexes.at(w);
        }
        catch(const std::out_of_range& oor) {
            Concept* c = new Concept(this, w, concepts.size());
            c->status = 1;
            concepts.push_back(w);
            indexes[w] = c;
            return c;
        }
    }

    Concept* create(wstring& w, Concept* conc) {
        if (conc == absurd)
            return conc;
        
        try {
            return indexes.at(w);
        }
        catch(const std::out_of_range& oor) {
            Concept* c = new Concept(this, conc, w, concepts.size());
            c->status = 1;
            concepts.push_back(w);
            indexes[w] = c;
            return c;
        }
    }

    Element* loop(LispE* lisp, short label,  List* code);
    
    Element* find(LispE* lisp, Concept* c);
    Element* find(LispE* lisp, wstring& w);
};
#endif

