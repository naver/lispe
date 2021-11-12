/*
 *  LispE
 *
 * Copyright 2021-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  ontology.cxx
//
//

#include "lispe.h"
#include "vecte.h"
#include "ontology.h"

//A concept is a binary position and a vector
const uint64_t Un = 1;
#define Rank(val) val >> 6;
#define Value(val,pos) Un << (val - (pos << 6));

Granule::Granule(long val) {
    rank = Rank(val);
    values =  Value(val,rank);
}

Granule::Granule(long rg,uint64_t val) {
    rank = rg;
    values =  val;
}


Granule::Granule(Granule* g) {
    if (g==NULL) {
        values=0;
        rank=0;
    }
    else {
        values=g->values;
        rank=g->rank;
    }
}

Granule::Granule(Granule& g) {
    values=g.values;
    rank=g.rank;
}

/*----------------------------------------------------------------------------------------*/
inline char checkRank(long r1, long r2) {
    return (r1 == r2?0:(r1 < r2)?-1:1);
}

void Concept_element::concept_and_not(Concept_element& c, Concept_element& add_element) {
    uint64_t v;
    long posadd=0;
    long poslocal=0;
    char tst;

    while (posadd < add_element.elements.last && poslocal < elements.last) {
        tst = checkRank(elements[poslocal]->rank,add_element.elements[posadd]->rank);
        switch (tst) {
            case -1:
                poslocal++;
                break;
            case 0: {
                v = elements[poslocal]->values & ~add_element.elements[posadd]->values;
                if (v) {
                    c.elements.push_back(new Granule(elements[poslocal]->rank, v));
                }
                //on passe aux elements suivants
                poslocal++;
                posadd++;
                break;
            }
            case 1:
                posadd++;
                break;
        }
    }
}

void Concept_element::concept_add(Concept_element& add_element) {
    long posadd=0;
    long poslocal=0;
    char tst;

    while (posadd < add_element.elements.last && poslocal < elements.last) {
        tst = checkRank(elements[poslocal]->rank,add_element.elements[posadd]->rank);
        switch (tst) {
            case -1:
                poslocal++;
                break;
            case 0: {
                elements[poslocal]->values |= add_element.elements[posadd]->values;
                //on passe aux elements suivants
                poslocal++;
                posadd++;
                break;
            }
            case 1:
                elements.insert(poslocal, new Granule(add_element.elements[posadd]));
                posadd++;
                poslocal++;
                break;
        }
    }

    for (;posadd<add_element.elements.last;posadd++)
        elements.push_back(new Granule(add_element.elements[posadd]));
}

void Concept_element::concept_remove(Concept_element& remove_element, long index) {
    long posadd=0;
    long poslocal=0;
    long clean = -1;
    char tst;
    
    while (posadd < remove_element.elements.last && poslocal < elements.last) {
        tst = checkRank(elements[poslocal]->rank,remove_element.elements[posadd]->rank);
        switch (tst) {
            case -1:
                poslocal++;
                break;
            case 0: {
                elements[poslocal]->values &= ~remove_element.elements[posadd]->values;
                if (!elements[poslocal]->values)
                    clean = poslocal;
                poslocal++;
                posadd++;
                break;
            }
            case 1:
                posadd++;
                break;
        }
    }
    
    if (clean != -1 && index) {
        long local_rank = Rank(index);
        for (poslocal = clean; poslocal >= 0; poslocal--) {
            if (local_rank == elements[poslocal]->rank) {
                elements[poslocal]->values |= Value(index, local_rank);
            }
            else {
                if (!elements[poslocal]->values) {
                    delete elements.removeElement(poslocal);
                }
            }
        }
    }
}

void Concept_element::concept_or(Concept_element& c, Concept_element& add_element) {
    uint64_t v;
    long posadd=0;
    long poslocal=0;
    char tst;

    while (posadd < add_element.elements.last && poslocal < elements.last) {
        tst = checkRank(elements[poslocal]->rank,add_element.elements[posadd]->rank);
        switch (tst) {
            case -1:
                c.elements.push_back(new Granule(elements[poslocal]));
                poslocal++;
                break;
            case 0: {
                v = elements[poslocal]->values | add_element.elements[posadd]->values;
                c.elements.push_back(new Granule(elements[poslocal]->rank, v));
                poslocal++;
                posadd++;
                break;
            }
            case 1:
                c.elements.push_back(new Granule(add_element.elements[posadd]));
                posadd++;
                break;
        }
    }

    for (;poslocal<elements.last;poslocal++)
        c.elements.push_back(new Granule(elements[poslocal]));
    
    for (;posadd<add_element.elements.last;posadd++)
        c.elements.push_back(new Granule(add_element.elements[posadd]));
}

void Concept_element::concept_xor(Concept_element& c, Concept_element& add_element) {
    uint64_t v;
    long posadd=0;
    long poslocal=0;
    char tst;

    while (posadd < add_element.elements.last && poslocal < elements.last) {
        tst = checkRank(elements[poslocal]->rank,add_element.elements[posadd]->rank);
        switch (tst) {
            case -1:
                c.elements.push_back(new Granule(elements[poslocal]));
                poslocal++;
                break;
            case 0: {
                v = elements[poslocal]->values ^ add_element.elements[posadd]->values;
                if (v) {
                    c.elements.push_back(new Granule(elements[poslocal]->rank, v));
                }
                poslocal++;
                posadd++;
                break;
            }
            case 1:
                c.elements.push_back(new Granule(add_element.elements[posadd]));
                posadd++;
                break;
        }
    }

    for (;poslocal<elements.last;poslocal++)
        c.elements.push_back(new Granule(elements[poslocal]));
    
    for (;posadd<add_element.elements.last;posadd++)
        c.elements.push_back(new Granule(add_element.elements[posadd]));

}

void Concept_element::concept_and(Concept_element& c, Concept_element& add_element) {
    uint64_t v;
    long posadd=0;
    long poslocal=0;
    char tst;

    while (posadd < add_element.elements.last && poslocal < elements.last) {
        tst = checkRank(elements[poslocal]->rank,add_element.elements[posadd]->rank);
        switch (tst) {
            case -1:
                poslocal++;
                break;
            case 0: {
                v = elements[poslocal]->values & add_element.elements[posadd]->values;
                if (v) {
                    c.elements.push_back(new Granule(elements[poslocal]->rank, v));
                }
                poslocal++;
                posadd++;
                break;
            }
            case 1:
                posadd++;
                break;
        }
    }
}
/*----------------------------------------------------------------------------------------*/
void Concept_element::concept_or(long idx) {
    long r = Rank(idx);
    long v = Value(idx, r);
            
    if (!elements.last || r > elements.back()->rank) {
        elements.push_back(new Granule(r,v));
        return;
    }
    
    long pos = 0;
    char tst;
    
    while (pos<elements.last) {
        tst = checkRank(r, elements[pos]->rank);
        switch (tst) {
            case -1:
                elements.insert(pos,new Granule(r, v));
                return;
            case 0:
                elements[pos]->values |= v;
                return;
            case 1:
                pos++;
                break;
        }
    }
    elements.push_back(new Granule(r,v));
}

void Concept_element::concept_or(Granule* g) {
    long pos=0;
    char tst;
    
    while (pos<elements.last) {
        tst = checkRank(g->rank, elements[pos]->rank);
        switch (tst) {
            case -1:
                elements.insert(pos,new Granule(g));
                return;
            case 0:
                elements[pos]->values |= g->values;
                //Et on detruit le pointeur
                delete g;
                return;
            case 1:
                pos++;
                break;
        }
    }
    elements.push_back(g);
}


bool Concept_element::contain(Concept_element& v) {

    long pos=0;
    long posv=0;
    char tst;
    
    while (posv<v.elements.last) {
        if (pos >= elements.last)
            return false;

        tst = checkRank(v.elements[posv]->rank , elements[pos]->rank);
        switch (tst) {
            case -1:
                return false;
            case 0:
                if ((elements[pos]->values & v.elements[posv]->values) != v.elements[posv]->values)
                    return false;
                pos++;
                posv++;
                break;
            case 1:
                pos++;
        }
    }
    return true;
}

bool Concept_element::intersect(Concept_element& v) {
    
    long poslocal = 0;
    long posv = 0;
    char tst;
    
    while (posv < v.elements.last && poslocal < elements.last) {
        tst = checkRank(v.elements[posv]->rank, elements[poslocal]->rank);
        switch (tst) {
            case -1:
                posv++;
                break;
            case 0:
                if ((elements[poslocal]->values & v.elements[posv]->values))
                    return true;
                poslocal++;
                posv++;
                break;
            case 1:
                poslocal++;
                break;
        }
    }
    
    return false;
}

Concept_element::Concept_element(Concept_element& v) {
    for (long i=0;i<v.elements.last;i++)
        elements.push_back(new Granule(v.elements[i]));
}

Concept_element::~Concept_element() {
    elements.clean();
}

void Concept_element::table(vecte<long>& liste) {
    
    //We build in reverse order
    for (long i=elements.last-1;i>=0;i--) {
        int64_t word=(int64_t)elements[i]->values;
        long base = elements[i]->rank<<6;
        for (long k=63;k>=0;k--) {
            //No more bits to seek, we stop
            if (word==0)
                break;
            //the sign bit is the left most bit
            if (word < 0)
                liste.push_back(base+k);
            //we then go to the next element...shifting bits by 1 to the left
            word <<= 1;
        }
    }
}

long Concept_element::count() {
    long counter=0;
    //We build in reverse order
    for (long i=elements.last-1;i>=0;i--) {
        int64_t word=(int64_t)elements[i]->values;
        for (long k=63;k>=0;k--) {
            //No more bits to seek, we stop
            if (word==0)
                break;
            //the sign bit is the left most bit
            if (word < 0)
                counter++;
            //we then go to the next element...shifting bits by 1 to the left
            word <<= 1;
        }
    }
    return counter;
}

//We remove the bit b
void Concept_element::remove(long b) {
    long rank= Rank(b);
    uint64_t word =  Value(b,rank);
    
    //We look for the first element of rang=rank
    for (long i=0;i<elements.last;i++) {
        if (elements[i]->rank==rank) {
            //We XOR the bit
            elements[i]->values^=word;
            if (elements[i]->values==0) {
                //the element does not contain any bit anymore
                //we retrait it
                Granule* g=elements.removeElement(i);
                delete g;
            }
            return;
        }
        
        //The elements are ordered...
        if (rank > elements[i]->rank)
            return;
    }
}

bool Concept_element::equal(Concept_element& v) {
    if (elements.last!=v.elements.last)
        return false;
    for (long i=0;i<elements.last;i++) {
        if (elements[i]->rank!=v.elements[i]->rank ||
            elements[i]->values!=v.elements[i]->values)
            return false;
    }
    return true;
}

/*----------------------------------------------------------------------------------------*/

Concept::Concept(Ontology* h) : Element(h->local_concept) {
    index = 0;
    semme = '_';
    ontologie = h;
}

Concept::Concept(Concept* c) : concept(c->concept), Element(c->ontologie->local_concept) {
    index = 0;
    semme = c->semme;
    ontologie = c->ontologie;
}

Concept::Concept(Ontology* h, Concept* c, u_ustring& s, long i) :
concept(c->concept), Element(h->local_concept) {
    semme = s;
    index = i;
    ontologie = h;
    Granule* g = concept.elements.back();
    long r = Rank(i);
    uint64_t v = Value(i,r);
    if (g->rank == r) {
        g->values |= v;
    }
    else {
        g = new Granule(r, v);
        concept.elements.push_back(g);
    }
}

Concept::Concept(Ontology* h, u_ustring& s, long i) : Element(h->local_concept) {
    semme = s;
    ontologie = h;
    semme = s;
    index = i;
    concept.elements.push_back(new Granule(i));
}

Concept* Concept::concept_remove(Concept* c) {
    if (c == this) {
        concept.clean();
        if (!index)
            return ontologie->absurd;
        concept.concept_or(index);
    }
    else {
        if (c->ontologie != ontologie)
            throw new Error("Error: these concepts do not belong to the same ontology");
        concept.concept_remove(c->concept, index);
        if (concept.isempty())
            return ontologie->absurd;
    }
    return this;
}

Concept* Concept::concept_or(Concept* c) {
    if (c->ontologie != ontologie)
        throw new Error("Error: these concepts do not belong to the same ontology");
    Concept* n = new Concept(ontologie);
    concept.concept_or(n->concept, c->concept);
    if (n->concept.isempty()) {
        delete n;
        return ontologie->absurd;
    }
    return n;
}

Concept* Concept::concept_xor(Concept* c) {
    if (c->ontologie != ontologie)
        throw new Error("Error: these concepts do not belong to the same ontology");
    Concept* n = new Concept(ontologie);
    concept.concept_xor(n->concept, c->concept);
    if (n->concept.isempty()) {
        delete n;
        return ontologie->absurd;
    }
    return n;
}

Concept* Concept::concept_and(Concept* c) {
    if (c->ontologie != ontologie)
        throw new Error("Error: these concepts do not belong to the same ontology");
    Concept* n = new Concept(ontologie);
    concept.concept_and(n->concept, c->concept);
    if (n->concept.isempty()) {
        delete n;
        return ontologie->absurd;
    }
    return n;
}

Concept* Concept::concept_and_not(Concept* c) {
    if (c->ontologie != ontologie)
        throw new Error("Error: these concepts do not belong to the same ontology");
    Concept* n = new Concept(ontologie);
    concept.concept_and_not(n->concept, c->concept);
    if (n->concept.isempty()) {
        delete n;
        return ontologie->absurd;
    }
    return n;
}

Concept* Concept::concept_not(long mx) {
    Concept* n = new Concept(ontologie);
    long pos = 0;
    uint64_t v;
    for (long i = 0; i < mx; i++) {
        if (i < concept.elements[pos]->rank || pos == concept.elements.last)
            n->concept.elements.push_back(new Granule(i, Un));
        else {
            if (i == concept.elements[pos]->rank) {
                v = ~concept.elements[pos]->values;
                n->concept.elements.push_back(new Granule(i, v));
                ++pos;
            }
        }
    }
    if (n->concept.isempty()) {
        delete n;
        return ontologie->absurd;
    }
    return n;
}

Element* Concept::bit_not(LispE* l) {
    return concept_not(ontologie->size());
}

Element* Concept::bit_and(LispE* l, Element* e) {
    if (e->type != type)
        throw new Error("Error: cannot apply '&' to this operand");
    Concept* result = concept_and((Concept*)e);
    release();
    return result;
    
}

Element* Concept::bit_and_not(LispE* l, Element* e) {
    if (e->type != type)
        throw new Error("Error: cannot apply '&~' to this operand");
    Concept* result =  concept_and_not((Concept*)e);
    release();
    return result;
}

Element* Concept::bit_or(LispE* l, Element* e) {
    if (e->type != type)
        throw new Error("Error: cannot apply '|' to this operand");
    Concept* result = concept_or((Concept*)e);
    release();
    return result;
}

Element* Concept::bit_xor(LispE* l, Element* e) {
    if (e->type != type)
        throw new Error("Error: cannot apply '^' to this operand");
    Concept* result =  concept_xor((Concept*)e);
    release();
    return result;
}

Element* Concept::equal(LispE* lisp, Element* e) {
    if (e->type != type)
        return null_;
    return booleans_[equal((Concept*)e)];
}

bool Concept::egal(Element* e) {
    return (e->type == type && equal((Concept*)e));
}

Element* Concept::intersect(LispE* lisp, Concept* c) {
    if (c->ontologie != ontologie)
        throw new Error("Error: these concepts do not belong to the same ontology");

    return booleans_[concept.intersect(c->concept)];
}

Element* Concept::contain(LispE* lisp, Concept* c) {
    if (c->ontologie != ontologie)
        throw new Error("Error: these concepts do not belong to the same ontology");
    
    return booleans_[concept.contain(c->concept)];
}

wstring Concept::asString(LispE*) {
    return _u_to_w(semme);
}

u_ustring Concept::asUString(LispE*) {
    return semme;
}


Element* Concept::asList() {
    vecte<long> table;
    concept.table(table);
    List* l = new List;
    u_ustring w;
    for (long i = 0; i < table.size(); i++) {
        w = ontologie->concepts[table[i]];
        l->append(ontologie->indexes[w]);
    }
    return l;
}

Element* Ontology::find(LispE* lisp, Concept* c) {
    if (c->ontologie != this)
        throw new Error("Error: this concept does not belong to this ontology");
    u_ustring w;
    for (auto& a: indexes) {
        if (c->equal(a.second)) {
            w = a.first;
            return lisp->provideString(w);
        }
    }
    return emptystring_;
}

Element* Ontology::find(LispE* lisp, u_ustring& w) {
    try {
        return indexes.at(w);
    }
    catch (...) {
        throw new Error("Error: unknown concept");
    }
}

Element* Ontology::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    Element* element;
    long sz = code->liste.size();
    for (auto& a : indexes) {
        element = a.second;
        lisp->replacingvalue(element, label);
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

//--------------------------------------------------------------------
 typedef enum {ontology_creation, ontology_ontology, ontology_concept,
     ontology_create, ontology_contain, ontology_list,
     ontology_absurd, ontology_absurdp, ontology_intersect,
     ontology_find, ontology_all, ontology_add, ontology_remove,
 } onto;
 
class Ontologyaction : public Element {
public:
    onto reg;
    short l_ontology;
    short l_concept;
    
    
    Ontologyaction(LispE* lisp, onto r) : reg(r), Element(l_lib, s_constant) {
        u_ustring w = U"ontology_";
        l_ontology = lisp->encode(w);
        w = U"concept_";
        l_concept = lisp->encode(w);
    }
    
    Element* eval(LispE* lisp) {
        switch (reg) {
            case ontology_creation: {
                u_ustring name = lisp->get(U"name")->asUString(lisp);
                return new Ontology(name, l_ontology, l_concept);
            }
            case ontology_concept: {
                Element* e = lisp->get(U"h");
                if (e->type != l_ontology)
                    throw new Error("Error: the first argument should be an ontology");
                Ontology* h = (Ontology*)e;
                u_ustring name = lisp->get(U"name")->asUString(lisp);
                return h->find(lisp, name);
            }
            case ontology_create: {
                //Creating a concept from nil or from other
                Element* e = lisp->get(U"h");
                if (e->type != l_ontology)
                    throw new Error("Error: the first argument should be an ontology");
                Ontology* h = (Ontology*)e;
                u_ustring name = lisp->get(U"name")->asUString(lisp);
                Element* conc = lisp->get(U"conc");
                if (conc != null_) {
                    if (conc->type != l_concept)
                        throw new Error("Error: the last element should be a 'concept'");
                    return h->create(name, (Concept*)conc);
                }
                return h->create(name);
            }
            case ontology_contain: {
                Element* conc = lisp->get(U"conc");
                Element* large_conc = lisp->get(U"large_conc");
                if (conc->type != l_concept || large_conc->type != l_concept)
                    throw new Error("Error: We can only compare concepts");
                return ((Concept*)large_conc)->contain(lisp, (Concept*)conc);
            }
            case ontology_intersect: {
                Element* conc = lisp->get(U"conc");
                Element* a = lisp->get(U"a");
                if (conc->type != l_concept || a->type != l_concept)
                    throw new Error("Error: We can only compare concepts");
                return ((Concept*)conc)->intersect(lisp, (Concept*)a);
            }
            case ontology_list: {
                Element* conc = lisp->get(U"conc");
                if (conc->type != l_concept)
                    throw new Error("Error: the last element should be a 'concept'");
                return ((Concept*)conc)->asList();
            }
            case ontology_find: {
                Element* e = lisp->get(U"h");
                if (e->type != l_ontology)
                    throw new Error("Error: the first argument should be an ontology");
                Ontology* h = (Ontology*)e;
                Element* conc = lisp->get(U"conc");
                if (conc->type != l_concept)
                    throw new Error("Error: the last element should be a 'concept'");
                return h->find(lisp, (Concept*)conc);
            }
            case ontology_all: {
                Element* e = lisp->get(U"h");
                if (e->type != l_ontology)
                    throw new Error("Error: the first argument should be an ontology");
                Ontology* h = (Ontology*)e;
                List* l = lisp->provideList();
                for(auto& a: h->indexes) {
                    l->append(a.second);
                }
                return l;
            }
            case ontology_add: {
                Element* conc = lisp->get(U"conc");
                Element* add = lisp->get(U"a");
                if (conc->type != l_concept)
                    throw new Error("Error: We can only enrich with concepts");
                if (add->type == l_concept)
                    return ((Concept*)conc)->concept_add((Concept*)add);
                if (add->isList()) {
                    long i;
                    for (i = 0; i < add->size(); i++) {
                        if (add->index(i)->type != l_concept)
                            throw new Error("Error: We can only enrich with concepts");
                    }
                    for (i = 0; i < add->size(); i++) {
                        ((Concept*)conc)->concept_add((Concept*)add->index(i));
                    }
                    return conc;
                }
                throw new Error("Error: We can only enrich with concepts");
            }
            case ontology_remove: {
                Element* conc = lisp->get(U"conc");
                Element* add = lisp->get(U"a");
                if (conc->type != l_concept)
                    throw new Error("Error: We can only remove a concept from another concept");
                if (add->type == l_concept)
                    return ((Concept*)conc)->concept_remove((Concept*)add);
                if (add->isList()) {
                    long i;
                    for (i = 0; i < add->size(); i++) {
                        if (add->index(i)->type != l_concept)
                            throw new Error("Error: We can only remove a concept from another concept");
                    }
                    for (i = 0; i < add->size(); i++) {
                        ((Concept*)conc)->concept_remove((Concept*)add->index(i));
                    }
                    return conc;
                }
                throw new Error("Error: We can only remove a concept from another concept");
            }
            case ontology_absurd: {
                Element* e = lisp->get(U"h");
                if (e->type != l_ontology)
                    throw new Error("Error: the first argument should be an ontology");
                return ((Ontology*)e)->absurd;
            }
            case ontology_absurdp: {
                Element* conc = lisp->get(U"conc");
                if (conc->type != l_concept)
                    throw new Error("Error: the first argument should be a concept");
                return booleans_[(((Concept*)conc)->ontologie->absurd == conc)];
            }
            case ontology_ontology: {
                Element* conc = lisp->get(U"conc");
                if (conc->type != l_concept)
                    throw new Error("Error: the first argument should be a concept");
                return ((Concept*)conc)->ontologie;
            }
        }
        
        return null_;
    }
 
    wstring asString(LispE* lisp) {
        switch (reg) {
            case ontology_creation:
                return L"Creates an ontology hierarchy";
            case ontology_ontology:
                return L"Returns the ontology in which this concept is declared";
            case ontology_concept:
                return L"Returns the concept associated with this name";
            case ontology_create:
                return L"Creates a new concept based on its name. The concept description can also be provided";
            case ontology_contain:
                return L"Check if mother contains conc";
            case ontology_list:
                return L"Returns the list of concepts stored in concepts";
            case ontology_absurd:
                return L"Returns the '_absurd' concept";
            case ontology_absurdp:
                return L"Checks if the concept is '_absurd'";
            case ontology_intersect:
                return L"Checks if two concepts have concepts in common";
            case ontology_find:
                return L"Checks if a concept has a name in the ontology";
            case ontology_all:
                return L"Returns the list of all concepts";
            case ontology_add:
                return L"Enriches the concept with the other concept";
                break;
            case ontology_remove:
                return L"Removes a concept from another concept";
        }

        return L"";
    }
 };


//We are also going to implement the body of the call
void moduleOntology(LispE* lisp) {
    lisp->extension("deflib ontology(name)", new Ontologyaction(lisp, ontology_creation));
    lisp->extension("deflib ontology_ontology(conc)", new Ontologyaction(lisp, ontology_ontology));
    lisp->extension("deflib ontology_create (h name (conc))", new Ontologyaction(lisp, ontology_create));
    lisp->extension("deflib ontology_concept (h name)", new Ontologyaction(lisp, ontology_concept));
    lisp->extension("deflib ontology_contain (large_conc conc)", new Ontologyaction(lisp, ontology_contain));
    lisp->extension("deflib ontology_intersect (conc a)", new Ontologyaction(lisp, ontology_intersect));
    lisp->extension("deflib ontology_add (conc a)", new Ontologyaction(lisp, ontology_add));
    lisp->extension("deflib ontology_remove (conc a)", new Ontologyaction(lisp, ontology_remove));
    lisp->extension("deflib ontology_list (conc)", new Ontologyaction(lisp, ontology_list));
    lisp->extension("deflib ontology_find (h conc)", new Ontologyaction(lisp, ontology_find));
    lisp->extension("deflib ontology_absurd (h)", new Ontologyaction(lisp, ontology_absurd));
    lisp->extension("deflib ontology_absurdp (conc)", new Ontologyaction(lisp, ontology_absurdp));
    lisp->extension("deflib ontology_all (h)", new Ontologyaction(lisp, ontology_all));
    
}

