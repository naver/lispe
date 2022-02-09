// LispE
// Copyright 2020-present NAVER Corp.
// The 3-Clause BSD License
//
//  delegation.h
//
//


#ifndef Avl_h
#define Avl_h
class Heap;

class Avl {
    long height;
    Avl* left;
    Avl* right;

 public:
    Avl* same;
    Element* value;

 
    Avl(Element* e);
    void tree_shape(Avl**,long&);
    void rebuilding(Avl**,long);
    void check_height(Avl**);
    
    void insertion(LispE*, Avl**, Element*, List* compare);
    bool erase(LispE*, Avl** root, Element* current, List* compare);
    void pop_front(Avl** root);
    void pop_last(Avl** root);
    Element* search(LispE*, Element* e, List* compare);
    bool check(LispE*, Element* e, List* compare);
    
    Element* front(LispE* lisp);
    Element* back(LispE* lisp);
    bool equal(LispE* lisp, Avl* avl, bool record);
    bool isequal(LispE* lisp, Avl* avl);
    
    void delete_node() {
        Avl* e = this;
        Avl* next;
        while (e != NULL) {
            e->value->decrement();
            next = e->same;
            delete e;
            e = next;
        }
    }
    
    void delete_avl() {
        if (left != NULL)
            left->delete_avl();
        if (right != NULL)
            right->delete_avl();
        delete_node();
    }

    Avl* fullcopy() {
        Avl* e = new Avl(value->fullcopy());
        Avl* o = same;
        Avl* a = e;
        while (o != NULL) {
            a->same = new Avl(o->value->fullcopy());
            a = a->same;
            o = o->same;
        }
       if (left)
           e->left = left->fullcopy();
        if (right)
            e->right = right->fullcopy();
        return e;
    }
    
    Avl* copy() {
        Avl* e = new Avl(value->copying(false));
        Avl* o = same;
        Avl* a = e;
        while (o != NULL) {
            a->same = new Avl(o->value->copying(false));
            a = a->same;
            o = o->same;
        }
       if (left)
           e->left = left->copy();
        if (right)
            e->right = right->copy();
        return e;
    }

    Element* element(LispE* lip);
    
    inline void element(LList* l) {
        Avl* e = this;
        while (e != NULL) {
            l->push_front(e->value);
            e = e->same;
        }
    }
    
    inline void element(List* l) {
        Avl* e = this;
        while (e != NULL) {
            l->append(e->value);
            e = e->same;
        }
    }
    
    inline void element(List& l) {
        Avl* e = this;
        while (e != NULL) {
            l.liste.push_raw(e->value);
            e = e->same;
        }
    }
    
    Element* element();

    Element* traverse(LispE* lisp, long& i) {
        if (!i)
            return element(lisp);
        i--;
        if (left != NULL) {
            Element* e = left->traverse(lisp, i);
            if (e != NULL)
                return e;
        }
        if (right != NULL)
            return right->traverse(lisp, i);
        return NULL;
    }

    Element* traverse(long& i) {
        if (!i)
            return element();
        i--;
        if (left != NULL) {
            Element* e = left->traverse(i);
            if (e != NULL)
                return e;
        }
        if (right != NULL)
            return right->traverse(i);
        return NULL;
    }

    void size(long& i) {
        i++;
        if (left != NULL)
            left->size(i);
        if (right != NULL)
            right->size(i);
    }

    void to_list(LispE* lisp, List* l);
    void to_llist(LispE* lisp, LList* l);
    void flatten(List* l);
    void flatten(List& l);

    void jsonString(LispE* lisp, wstring& w);
    void asString(LispE* lisp, wstring& w);
    void asUString(LispE* lisp, u_ustring& w);

};

class Iter_heap {
public:
    List l;
    long size;
    long index;
    
    Iter_heap(Avl* a) {
        if (a == NULL)
            size = 0;
        else {
            a->flatten(l);
            size = l.size();
        }
        index = 0;
    }
    
    Element* next() {
        if (index == size)
            return NULL;
        return l.index(index++);
    }
    
};

class Heap : public Element {
public:
    Avl* root;
    List* compare;
    
    Heap(List* c) : Element(t_heap) {
        root = NULL;
        compare = c;
        compare->increment();
    }
    
    ~Heap() {
        if (root != NULL)
            root->delete_avl();
        compare->decrement();
    }
    
    
    void push_element(LispE* lisp, List* l);
    void push_element_front(LispE* lisp, List* l);
    void push_element_back(LispE* lisp, List* l);

    bool remove(LispE* lisp, Element* e);
    Element* insert(LispE* lisp, Element* e, long idx);
    Element* insert(LispE* lisp, Element* e);
    bool check_element(LispE* lisp, Element* element_value);
    Element* search_element(LispE*, Element* element_value, long idx);
    Element* asList(LispE* lisp);
    Element* asLList(LispE* lisp);
    void flatten(LispE*, List* l) {
        if (root == NULL)
            return;
        root->flatten(l);
    }
    
    bool removefirst() {
        if (root == NULL)
            return false;
        root->pop_front(&root);
        return true;
    }
    
    bool removelast() {
        if (root == NULL)
            return false;
        root->pop_last(&root);
        return true;
    }
    
    Element* duplicate_constant(bool pair = false) {
        if (status == s_constant) {
            return fullcopy();
        }
        return this;
    }

    Element* fullcopy() {
        Heap* tree = new Heap(compare);
        if (root == NULL)
            return tree;
        tree->root = root->fullcopy();
        return tree;
    }
    
    Element* copyatom(LispE* lisp, uint16_t s) {
        if (status < s)
            return this;
        
        Heap* tree = new Heap(compare);
        if (root == NULL)
            return tree;
        tree->root = root->copy();
        return tree;
    }

    Element* copying(bool duplicate = true) {
        if (!status)
            return this;
        
        Heap* tree = new Heap(compare);
        if (root == NULL)
            return tree;
        tree->root = root->copy();
        return tree;
    }

    bool Boolean() {
        return (root != NULL);
    }
    
    bool isEmpty() {
        return (root == NULL);
    }
    
    Element* car(LispE*);
    
    Element* index(long i);
    Element* protected_index(LispE*,long i);
    Element* value_from_index(LispE*, long i);
    Element* value_on_index(LispE*, long i);
    Element* value_on_index(LispE*, Element* idx);
    Element* protected_index(LispE*, Element* k);
    
    long size() {
        if (root == NULL)
            return 0;
        long i = 0;
        root->size(i);
        return i;
    }
    
    bool isContainer() {
        return true;
    }

    bool unify(LispE* lisp, Element* value, bool record);
    
    bool isequal(LispE* lisp, Element* value) {
        if (value == this)
            return true;
        if (value->type != type)
            return false;
        Heap* a_value = (Heap*)value;
        if (root == NULL)
            return (a_value->root == NULL);
        return root->isequal(lisp, a_value->root);
    }
    
    void* begin_iter() {
        return new Iter_heap(root);
    }
    
    Element* next_iter(LispE* lisp, void* it);
    Element* next_iter_exchange(LispE* lisp, void* it);

    void clean_iter(void* it) {
        delete (Iter_heap*)it;
    }

    wstring jsonString(LispE* lisp);
    wstring asString(LispE* lisp);
    u_ustring asUString(LispE* lisp);
    Element* loop(LispE* lisp, short label, List* code);
    bool egal(Element* e);
};
#endif
