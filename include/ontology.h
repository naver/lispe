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
        
    vecte<Granule*> elements;

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
    void table(vecte<long>&);

    bool intersect(Concept_element&);
    bool contain(Concept_element&);
    bool equal(Concept_element& v);
};

class Ontology;
class Concept : public Element {
public:

    Concept_element concept;
    u_ustring semme;
    Ontology* ontologie;
    long index;

    Concept(Ontology* h);
    Concept(Concept* c);
    Concept(Ontology* h, Concept* c, u_ustring& s,long i);
    Concept(Ontology* h, u_ustring& s,long i);

    Element* concept_add(Concept* c) {
        if (c->ontologie != ontologie)
            throw new Error("Error: these concepts do not belong to the same ontology");
        concept.concept_add(c->concept);
        return this;
    }

    Element* concept_remove(Concept* c);

    Element* concept_or(Concept* c);
    Element* concept_xor(Concept* c);
    Element* concept_and(Concept* c);
    Element* concept_and_not(Concept* c);
    Element* concept_not(long mx);

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
    bool egal(Element* e);

    long size() {
        return concept.count();
    }
    
    wstring asString(LispE*);
    u_ustring asUString(LispE*);
    Element* asList();
};


class Ontology : public Element {
public:
    std::vector<u_ustring> concepts;
    std::unordered_map<u_ustring, Concept*> indexes;
    u_ustring name;
    Concept* absurd;
    int16_t local_concept;

    Ontology(u_ustring& n, int16_t l_hie, int16_t l_conc) : Element(l_hie) {
        local_concept = l_conc;
        name = n;
        u_ustring s_absurd = U"_absurd";
        absurd = new Concept(this, s_absurd, 0);
        absurd->status = s_constant;
        concepts.push_back(s_absurd);
        indexes[s_absurd] = absurd;
    }
    
    ~Ontology() {
        for (const auto& a : indexes)
            a.second->decrement();
    }
    
    wstring asString(LispE*) {
        return _u_to_w(name);
    }

    u_ustring asUString(LispE*) {
        return name;
    }
    

    long size() {
        return concepts.size();
    }
    
    Concept* create(u_ustring& w) {
        const auto& a = indexes.find(w);
        if (a == indexes.end()) {
            Concept* c = new Concept(this, w, concepts.size());
            c->status = 1;
            concepts.push_back(w);
            indexes[w] = c;
            return c;
        }
        return a->second;
    }
    
    Concept* create(u_ustring& w, Concept* conc) {
        if (conc == absurd)
            return conc;
        
        const auto& a = indexes.find(w);
        if (a == indexes.end()) {
            Concept* c = new Concept(this, conc, w, concepts.size());
            c->status = 1;
            concepts.push_back(w);
            indexes[w] = c;
            return c;
        }
        return a->second;
    }

    Element* loop(LispE* lisp, int16_t label,  List* code);
    
    Element* find(LispE* lisp, Concept* c);
    Element* find(LispE* lisp, u_ustring& w);
};
#endif

