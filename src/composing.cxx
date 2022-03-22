/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  composing.cxx
//
//

#include "lispe.h"

/*
 Implementation Notes
 --------------------
 
 Basically, every single high level instruction is transformed into a loop.
 
 We distinguish two sorts of instructions:
 
 filtering: filter, l_take, l_drop, l_takewhile and l_dropwhile
 mapping: map, scanl, foldl, scanr, foldr, scanl1, foldl1, scanr1, foldr1
 
 The filtering methods checks on intermediate values to only keep the ones that match their conditions
 The mapping methods apply computing on each value from the initial list
 
 Mapping
 -------
 
 For instance: (map '+ '(1 2 3)) is transformed into:
 
 (push #recipient ()) (loop #i '(1 2 3) (setq #accu (+ #i #i)) (push #recipient #accu))
 
 #i is the iterator variable
 #accu the accumulator in which different operations are computed
 #recipient a list in which every single #accu is stored
 
 Filtering
 ----------
 
 (filter '(< 10) '(1 2 3)) is implemented as:
 
 (push #recipient ()) (loop #i '(1 2 3) (check (< #i 10) (push #recipient #i)))
 
 Combining
 ---------
 When these different instructions are composed together (combined together) such as:
 
 (filter '(< 20) (map '+ '(1 3 5 9 6 4)))
 -----------------------------------------
 
 Then, we keep only one single loop, in which we add our conditions:
 ----------------------------------
 
 First the map: (push #recipient ()) (loop #i '(1 3 5 9 6 4) (setq #accu (+ #i #i)) (push #recipient #accu))
 
 Then the filtering, whose condition now encapsulates '(push #recipient #)'
 
 (push #recipient ()) (loop #i '(1 2 3) (setq #accu (+ #i #i)) (check (< #accu 10) (push #recipient #accu)))
 
 Note that the 'check' now takes #accu as the variable to check against
 
 An other example is applying a mapping onto a mapping:
 
 (mapping '* (mapping '+ '(1 2 3 4)))
 ------------------------------------
 
 In this case, we simply modify the operation twice:
 
 The first map: (push #recipient ()) (loop #i '(1 2 3 4) (setq #accu (+ #i #i)) (push #recipient #accu))
 
 The second map:
 
 (push #recipient ()) (loop #i '(1 3 5 9 6 4) (setq #accu (* (+ #i #i) (+ #i #i))) (push #recipient #accu))

 Tracking which part to modify
 -----------------------------
 To track, which parts to modify, we need to track the operation and the checks and ifs.
 
 1) operation tracks the different operations, its initial value is nil, when only filterings are applied
 2) final_section tracks the different checks, its initial value is: (push #recipient #accu)
 
 In the 'compose' list, operation is stored in liste[2] and final_section in liste[3]...
 
 If we take the following example:
 
 (map '* (filter '(< 20) (map '+ '(1 3 5 9 6 4))))
 -------------------------------------------------
 
 a) First we create our initial loop:
 
 (push #recipient ()) (loop #i '(1 3 5 9 6 4) (setq #accu (+ #i #i)) (push #recipient #accu))
 
 operation = (set #accu (+ #i #i))
 final_section = (push #recipient #accu)
 
 b) Second, we introduce our filter:
 (push #recipient ()) (loop #i '(1 2 3) (setq #accu (+ #i #i)) (check (< #accu 10) (push #recipient #accu)))
 
 operation = (set #accu (+ #i #i))
 final_section = (check (< #accu 10) (push #recipient #accu))
 
 c) Third, we add the final map:
 (push #recipient ()) (loop #i '(1 2 3) (setq #accu (+ #i #i)) (check (< #accu 10) (setq ##accu (* #accu #accu0)) (push #recipient ##accu)))
 
 Note that we have introduced an new accumulator: ##accu, since we cannot modify the initial #accu, on which the test depends...
 
 operation = (setq ##accu (* #accu #accu0))
 final_section = (check (< #accu 10) (setq ##accu (* #accu #accu0)) (push #recipient ##accu))
 
 Hence if add another layer on top:
 
 (filter '(!= 100) (map '* (filter '(< 20) (map '+ '(1 3 5 9 6 4)))))
 --------------------------------------------------------------------
 
 The loop is now:
 
 (push #recipient ())
 (loop #i '(1 2 3)
    (setq #accu (+ #i #i))
    (check (< #accu 10)
            (setq ##accu (* #accu #accu0))
            (check (!= ##accu 100) (push #recipient ##accu))
    )
 )

 with
 
 operation = (setq ##accu (* #accu #accu0))
 final_section = (check (!= ##accu 100) (push #recipient ##accu)); this is the most embedded 'check'
 
 Note how the last 'check' in the structure now bears on ##accu and not on #accu...

 However, if we add a new map on the top:
 
 (map '(!= 100) (map '* (filter '(< 20) (map '+ '(1 3 5 9 6 4)))))
 -----------------------------------------------------------------

 Then we modify the operation:
 
 (push #recipient ())
 (loop #i '(1 2 3)
    (setq #accu (+ #i #i))
    (check (< #accu 10)
            (setq ##accu  (+ (* #accu #accu0) (* #accu #accu0)))
            (push #recipient ##accu)
    )
 )

 operation = (setq ##accu  (+ (* #accu #accu0) (* #accu #accu0)))
 final_section = (check (< #accu 10) (setq ##accu  (+ (* #accu #accu) (* #accu #accu))) (push #recipient ##accu))
 
 
 check/ncheck
 ------------
 
 The core idea in this implementation is that the final section is ALWAYS the last element of the structure.
 
a) (check CONDITION FINAL)
b) (ncheck CONDITION ELSE FINAL)
 
The difference between check and ncheck is that ncheck executes the ELSE when the CONDITION is nil.
 
 Hence, whenever an update is implemented through a composition, we always know where to modifiy the final_section instruction set.

 This method is used at compile time to evaluate take, takewhile, repeat, map, filter etc..
 In order to compose them when they are embedded...
 case l_cycle:
 case l_repeat:
 case l_take:
 case l_drop:
 case l_takewhile:
 case l_dropwhile:
 case l_filter:
 case l_map:
 case l_scan
 
 This section is akin to a complex macro, which transforms these instructions into LispE loops.
 Hence, we build a single loop, in which we introduce our tests and our intermediate computations.
 
 Examples:
 
 //(map '* (map '+ '(1 2 3 4 5 6 7 8 9)))
 //(map '* (filter '(< 10) '(1 3 9 10 12 34 5 7)))
 //(filter '(< 100) (map '* '(1 3 9 10 12 34 5 7)))
 //(filter '(< 100) (map '* (filter '(< 10) (map '+ '(1 2 3 4 5 6 7 8)))))
 //(map '* (take 3 (filter '(> 2) (map '+ '(1 2 3 4 5)))))
 //(map '+ (map '* (filter '(> 2) '(1 2 3 4 5))))
 //(scanl '+ 10 (take 3 '(1 2 3 4)))
 //(scanl '+ 10 (take 3 (map '* '(1 2 3 4))))
 //(scanl '+ 10 (drop 3 (map '* '(1 2 3 4 5 6 7 8 9 10))))
 //(map '* (takewhile '(< 100) '(1 2 3 4 5 10 102 12 32)))
 //(map '+ (filter '(< 100) (map '* (takewhile '(< 10) (map '+ '(1 2 3 4 5 12 34 4 6 7 8))))))
 //(map '+ (filter '(< 1000) (map '* (dropwhile '(< 10) (map '+ '(1 2 3 4 5 12 21 25 34 44 6 7 8))))))
 //(map 'eval (map 'atom (filter (\(x) (in x "prgx")) (map 'string (atoms)))))
 //(scanl1 '+ (map '* '(1 2 3 4)))
 //(scanl1 '+ (filter '(> 2) '(1 2 3 4)))
 //(scanl1 '+ (filter '(< 20) (map '* '(1 2 3 4))))
 //(scanl1 '+ (take 3 (map '* '(1 2 3 4))))
 //(scanl1 '+ (map '* (filter '(< 20) '(1 2 3 4))))
 //(defun x(v) (for i (map '+ (range 1 10 1)) (+ i 10)))
 //(defun xx(v) (for i (map '+ (range 1 10 1)) (+ i 10) (> i 3)))
 */

#define P(x) lisp->provideAtom(x)

static bool findvariable(Element* L, Element* v) {
    if (L == v)
        return true;
    
    if (L->isList()) {
        for (long i = 0; i < L->size(); i++) {
            if (findvariable(L->index(i), v))
                return true;
        }
    }
    return false;
}

static void replacevariable(Element* L, Element* v, Element* n) {
    if (L->isList()) {
        Element* e;
        for (long i = 0; i < L->size(); i++) {
            e = L->index(i);
            if (e->label() == l_lambda)
                return;
            if (e == v)
                L->change(i, n);
            else
                replacevariable(e, v, n);
        }
    }
}


static bool replace_recipient(Element* L, Element* rec, Element* accu) {
    int16_t label = L->index(0)->label();
    if (L->index(1) == rec && (label == l_push || label == l_insert)) {
        L->change(2, accu);
        return true;
    }
    if (L->isList()) {
        for (long i = 0; i < L->size(); i++) {
            if (replace_recipient(L->index(i), rec, accu))
                return true;
        }
    }
    return false;
}



Element* List::composing(LispE* lisp, bool docompose) {
    static int idx_var = 0;

    long listsize = liste.size();
    Element* first_element = liste[0];
    
    Element* element;
    Element* final_section = NULL;
    List* compose = NULL;
    List* loop;
    Element* iterator_variable;
    
    Element* _iterator = null_;
    Element* _recipient;
    Element* _value;
    Element* _check = null_;
    Element* operation = null_;
    Element* action;
    Element* _first = null_;
    Element* _id_var;
    
    //Then we can compose our elements...
    int16_t labeltype = first_element->type;
    if (!lisp->delegation->checkArity(labeltype, listsize)) {
        u_ustring err(U"Error: wrong number of arguments for: '");
        err += lisp->asUString(labeltype);
        err += U"'";
        throw new Error(err);
    }
    
    first_element = liste[1];
    
    if (labeltype == l_for) {
        element = liste[2];
        _iterator = liste[1];
        if (docompose && element->index(0)->label() == l_compose) {
            //We are composing with a substructure...
            compose = (List*)element;
            //First, we replace the current variable with our local iterator
            replacevariable(compose, compose->liste.back()->index(1), _iterator);
            if (listsize == 5) {
                //We first create a filter
                loop = (List*)lisp->create_local_instruction(l_filter, liste[4], compose, _iterator);
                _check = loop->composing(lisp, true);
                //Then we create our map...
                compose = (List*)lisp->create_local_instruction(l_map, liste[3], _check, _iterator);
                operation =  compose->composing(lisp, true);
                loop->release();
                compose->release();
                return operation;
            }
            compose = (List*)lisp->create_local_instruction(l_map, liste[3], compose, _iterator);
            operation =  compose->composing(lisp, true);
            compose->release();
            return operation;
        }
        docompose = false;
    }
    else {
        if (labeltype == l_repeat || labeltype == l_cycle) {
            if (labeltype == l_cycle)
                element = new Cyclelist(lisp);
            else
                element = new Infinitelist(lisp);
            lisp->garbaging(element);
        }
        else {
            if (listsize == 4 && (labeltype == l_map || labeltype == l_filter)) {
                //This map or this filter was issued from a for interpretation
                _iterator = liste[3];
                listsize = 3;
            }
            if (listsize == 3)
                element = liste[2];
            else
                element = liste[3];
        }
    }
    wchar_t buffer[20];

    int idx = idx_var;
    bool creation = true;
    if (docompose && element->index(0)->label() == l_compose) {
        creation = false;
        compose = (List*)element;
        _id_var = compose->liste[1];
        idx = (int)_id_var->index(0)->asInteger();
    }
    else {
        _id_var = new List;
        lisp->garbaging(_id_var);
        _value = new Integer(idx_var++);
        lisp->garbaging(_value);
        _id_var->append(_value);
        _id_var->append(zero_);
        //We only create a certain number of variables
        //we expect encapsulation to be less than 16...
        if (idx_var == 16)
            idx_var = 0;
    }

    if (_iterator == null_) {
        swprintf_s(buffer,20, L"#i%d", idx);
        _iterator = P(buffer);
    }
    
    swprintf_s(buffer,20, L"#recipient%d", idx);
    _recipient = P(buffer);
    
    swprintf_s(buffer,20, L"#accu%d", idx);
    _value = P(buffer);

    int16_t fold_ordering = 0;
    if (creation) {
        //We need to create our structure from scratch
        iterator_variable = _iterator;
        switch (labeltype) {
            case l_replicate:
                final_section = lisp->create_instruction(l_push, _recipient, element);
                loop = lisp->create_instruction(l_loopcount, _iterator, final_section);
                element = lisp->create_instruction(l_setq, _recipient, emptylist_);
                compose = lisp->create_instruction(l_block, element, loop);
                break;
            case l_map:
                operation = lisp->create_instruction(l_setq, _value, _iterator);
                final_section = lisp->create_instruction(l_push, _recipient, _value);
                loop = lisp->create_instruction(l_loop, _iterator, element, operation, final_section);
                element = lisp->create_instruction(l_setq, _recipient, emptylist_);
                compose = lisp->create_instruction(l_compose, _id_var, operation, final_section, element, loop);
                break;
            case l_for:
                operation = lisp->create_instruction(l_setq, _value, liste[3]);
                final_section = lisp->create_instruction(l_push, _recipient, _value);
                element = lisp->create_instruction(l_setq, _recipient, emptylist_);
                if (listsize == 5) {
                    action = lisp->create_instruction(l_check, liste[4], operation, final_section);
                    loop = lisp->create_instruction(l_loop, _iterator, liste[2], action);
                    compose = lisp->create_instruction(l_compose, _id_var, operation, action, element, loop);
                    _id_var->change(1, one_);
                }
                else {
                    loop = lisp->create_instruction(l_loop, _iterator, liste[2], operation, final_section);
                    compose = lisp->create_instruction(l_compose, _id_var, operation, final_section, element, loop);
                }
                return compose;
            case l_foldl:
                //First we need to initialize out value
                //(foldl op init list)
                //In this case, the initialisation value is provided as the third parameter
                action = lisp->create_instruction(l_setq, _value, liste[2]);
                operation = lisp->create_instruction(l_setq, _value, _iterator);
                //The fourth parameter is the list to loop on
                final_section = lisp->create_instruction(l_setq, _recipient, _value);
                loop = lisp->create_instruction(l_loop, _iterator, liste[3], operation, final_section);
                compose = lisp->create_instruction(l_compose, _id_var, operation, final_section, _recipient, action, loop);
                fold_ordering = 1;
                break;
            case l_scanl:
                //First we need to initialize out value
                //(scanl op init list)
                //(setq value init)...(setq recipient (cons value ())
                //In this case, the initialisation value is provided as the third parameter
                action = lisp->create_instruction(l_setq, _value, liste[2]);
                operation = lisp->create_instruction(l_setq, _value, _iterator);
                //The fourth parameter is the list to loop on
                final_section = lisp->create_instruction(l_push, _recipient, _value);
                loop = lisp->create_instruction(l_loop, _iterator, liste[3], operation, final_section);
                element = lisp->create_instruction(l_setq, _recipient, lisp->create_instruction(l_cons, liste[2], emptylist_));
                compose = lisp->create_instruction(l_compose, _id_var, operation, final_section, element, action, loop);
                fold_ordering = 1;
                break;
            case l_foldr:
                //In this case, the initialisation value is provided as the third parameter
                action = lisp->create_instruction(l_setq, _value, liste[2]);
                operation = lisp->create_instruction(l_setq, _value, _iterator);
                //The fourth parameter is the list to loop on
                loop = lisp->create_instruction(l_reverse, liste[3]);
                final_section = lisp->create_instruction(l_setq, _recipient, _value);
                loop = lisp->create_instruction(l_loop, _iterator, loop, operation, final_section);
                compose = lisp->create_instruction(l_compose, _id_var, operation, final_section, _recipient, action, loop);
                fold_ordering = 2;
                break;
            case l_scanr:
                //In this case, the initialisation value is provided as the third parameter
                action = lisp->create_instruction(l_setq, _value, liste[2]);
                operation = lisp->create_instruction(l_setq, _value, _iterator);
                //The fourth parameter is the list to loop on
                loop = lisp->create_instruction(l_reverse, liste[3]);
                final_section = lisp->create_instruction(l_insert, _recipient, _value, zero_);
                loop = lisp->create_instruction(l_loop, _iterator, loop, operation, final_section);
                element = lisp->create_instruction(l_setq, _recipient, lisp->create_instruction(l_cons, liste[2], emptylist_));
                compose = lisp->create_instruction(l_compose, _id_var, operation, final_section, element, action, loop);
                fold_ordering = 2;
                break;
            case l_foldl1:
                //First we need to initialize out value
                //In this case, the initialisation value is provided as the first element from the list
                //(foldl1 op list)
                //The fourth parameter is the list to loop on
                
                swprintf_s(buffer,20, L"#first%d", idx);
                _first = P(buffer);
                
                //This is the prototype of operation, the last element will be modified
                operation = lisp->create_instruction(l_setq, _value, _iterator);

                //We need to initialize our process with the first element from the list
                //(ncheck #first (setq #value #i) (setq #first nil) (setq #value #i))
                action = lisp->create_instruction(l_ncheck, _first);
                action->append(operation);
                action->append(lisp->create_instruction(l_setq, _first, null_));
                action->append(lisp->create_instruction(l_setq, _value, _iterator));

                final_section = lisp->create_instruction(l_setq, _recipient, _value);
                loop = lisp->create_instruction(l_loop, _iterator, liste[2], action, final_section);
                action = lisp->create_instruction(l_setq, _first, true_);
                compose = lisp->create_instruction(l_compose, _id_var, operation, final_section, _recipient, action, loop);
                fold_ordering = 1;
                break;
            case l_scanl1:
                //First we need to initialize out value
                swprintf_s(buffer,20, L"#first%d", idx);
                _first = P(buffer);
                

                //In this case, the initialisation value is provided as the first element from the list
                //(foldl1 op list)
                //This is the prototype of operation, the last element will be modified
                operation = lisp->create_instruction(l_setq, _value, _iterator);

                //We need to initialize our process with the first element from the list
                //(ncheck #first (setq #value #i) (setq #first nil) (setq #value #i))
                action = lisp->create_instruction(l_ncheck, _first);
                action->append(operation);
                action->append(lisp->create_instruction(l_setq, _first, null_));
                action->append(lisp->create_instruction(l_setq, _value, _iterator));

                final_section = lisp->create_instruction(l_push, _recipient, _value);
                loop = lisp->create_instruction(l_loop, _iterator, liste[2], action, final_section);
                action = lisp->create_instruction(l_setq, _first, true_);
                element = lisp->create_instruction(l_setq, _recipient, emptylist_);
                compose = lisp->create_instruction(l_compose, _id_var, operation, final_section, element, action, loop);
                fold_ordering = 1;
                break;
            case l_foldr1:
                //First we need to initialize out value
                swprintf_s(buffer,20, L"#first%d", idx);
                _first = P(buffer);
                
                //In this case, the initialisation value is provided as the last element from the list
                //(foldr1 op list)
                //The fourth parameter is the list to loop on
                loop = lisp->create_instruction(l_reverse, liste[2]);
                
                //This is the prototype of operation, the last element will be modified
                operation = lisp->create_instruction(l_setq, _value, _iterator);

                //We need to initialize our process with the first element from the list
                //(ncheck #first (setq #value #i) (setq #first nil) (setq #value #i))
                action = lisp->create_instruction(l_ncheck, _first);
                action->append(operation);
                action->append(lisp->create_instruction(l_setq, _first, null_));
                action->append(lisp->create_instruction(l_setq, _value, _iterator));

                final_section = lisp->create_instruction(l_setq, _recipient, _value);
                loop = lisp->create_instruction(l_loop, _iterator, loop, action, final_section);
                action = lisp->create_instruction(l_setq, _first, true_);
                compose = lisp->create_instruction(l_compose, _id_var, operation, final_section, _recipient, action, loop);
                fold_ordering = 2;
                break;
            case l_scanr1:
                //First we need to initialize out value
                swprintf_s(buffer,20, L"#first%d", idx);
                _first = P(buffer);
                
                //In this case, the initialisation value is provided as the last element from the list
                //(foldr1 op list)
                //The fourth parameter is the list to loop on
                loop = lisp->create_instruction(l_reverse, liste[2]);

                //This is the prototype of operation, the last element will be modified
                operation = lisp->create_instruction(l_setq, _value, _iterator);

                //We need to initialize our process with the first element from the list
                //(ncheck #first (setq #value #i) (setq #first nil) (setq #value #i))
                action = lisp->create_instruction(l_ncheck, _first);
                action->append(operation);
                action->append(lisp->create_instruction(l_setq, _first, null_));
                action->append(lisp->create_instruction(l_setq, _value, _iterator));

                final_section = lisp->create_instruction(l_insert, _recipient, _value, zero_);
                loop = lisp->create_instruction(l_loop, _iterator, loop, action, final_section);
                action = lisp->create_instruction(l_setq, _first, true_);
                element = lisp->create_instruction(l_setq, _recipient, emptylist_);
                compose = lisp->create_instruction(l_compose, _id_var, operation, final_section, element, action, loop);
                fold_ordering = 2;
                break;
            case l_drop:
                //We need to add a check...
                swprintf_s(buffer,20, L"#count%d", idx);
                _check = P(buffer);
                final_section = lisp->create_instruction(l_push, _recipient, _iterator);
                loop = lisp->create_instruction(l_loop, _iterator, element, final_section);
                action = lisp->create_instruction(l_size, _recipient);
                action = lisp->create_instruction(l_minus, one_, action);
                action = lisp->create_instruction(l_setq, _check, action);
                element = lisp->create_instruction(l_setq, _recipient, emptylist_);
                _id_var->change(1, one_);
                compose = lisp->create_instruction(l_compose, _id_var, null_, final_section, element, action, loop);
                break;
            case l_dropwhile:
                swprintf_s(buffer,20, L"#check%d", idx);
                _check = P(buffer);
                final_section = lisp->create_instruction(l_push, _recipient, _iterator);
                loop = lisp->create_instruction(l_loop, _iterator, element, final_section);
                action = lisp->create_instruction(l_setq, _check, true_);
                element = lisp->create_instruction(l_setq, _recipient, emptylist_);
                _id_var->change(1, one_);
                compose = lisp->create_instruction(l_compose, _id_var, null_, final_section, element, action, loop);
                break;
            default:
                final_section = lisp->create_instruction(l_push, _recipient, _iterator);
                loop = lisp->create_instruction(l_loop, _iterator, element, final_section);
                action = lisp->create_instruction(l_setq, _recipient, emptylist_);
                _id_var->change(1, one_);
                compose = lisp->create_instruction(l_compose, _id_var, null_, final_section, action, loop);
        }
    }
    else {
        //-----------------------------------------------------------------
        // COMPOSITION
        //-----------------------------------------------------------------
        //We will update our compose structure by combining the new
        //instruction with existing ones...
        //-----------------------------------------------------------------
        operation = (List*)compose->liste[2];
        final_section = (List*)compose->liste[3];
        loop = (List*)compose->last();


        iterator_variable = _iterator;
        switch(labeltype) {
                //The function returns an element, we push it in #l
            case l_map:
                if (!compose->index(4)->isList())
                    throw new Error("Error: 'map' can only apply to lists");
            case l_foldl:
            case l_scanl:
            case l_foldr:
            case l_scanr: {
                //we already have an operation stored in _value
                //We use a new accumulator value. IF we are in this section
                //it means that a test on current _value has already been implemented
                //we do not want to interfer with it:
                int16_t label = final_section->index(0)->label();
                if (label == l_check || label == l_ncheck) {
                    Element* accu = NULL;
                    if (operation == null_) {
                        operation = lisp->create_instruction(l_setq, _value, _iterator);
                        accu = _value;
                        replace_recipient(final_section, _recipient, _value);
                    }
                    else {
                        if (labeltype != l_map || findvariable(final_section->index(1), operation->index(1))) {
                            wstring wvalue = operation->index(1)->asString(lisp);
                            wvalue = L"#" + wvalue;
                            accu = P(wvalue);
                            operation = lisp->create_instruction(l_setq, accu, _value);
                            replace_recipient(final_section, _recipient, operation->index(1));
                            if (labeltype != l_map) {
                                _value = accu;
                            }
                        }
                    }
                    
                    if (accu != NULL) {
                        compose->liste[2] = operation;
                        final_section->beforelast(operation);
                    }
                }
                iterator_variable = operation->index(2);
                break;
            }
            case l_foldl1:
            case l_scanl1:
            case l_foldr1:
            case l_scanr1: {
                swprintf_s(buffer,20, L"#first%d", idx);
                wstring code(buffer);
                _first = P(code);
                while (findvariable(compose, _first)) {
                    code = L"#"+code;
                    _first = P(code);
                }

                action = lisp->create_instruction(l_ncheck, _first);
                
                int16_t label = final_section->index(0)->label();
                if (label == l_check || label == l_ncheck) {
                    Element* accu = _value;
                    if (operation == null_) {
                        operation = lisp->create_instruction(l_setq, _value, _iterator);
                        _check = lisp->create_instruction(l_setq, _value, _iterator);
                        compose->liste[2] = operation;
                        replace_recipient(final_section, _recipient, _value);
                    }
                    else {
                        wstring wvalue = operation->index(1)->asString(lisp);
                        wvalue = L"#" + wvalue;
                        accu = P(wvalue);
                        _check =  lisp->create_instruction(l_setq, accu, operation->index(1));
                        operation = lisp->create_instruction(l_setq, accu, _value);
                        compose->liste[2] = operation;
                        replace_recipient(final_section, _recipient, accu);
                        _value = accu;
                    }

                    action->append(operation);
                    action->append(lisp->create_instruction(l_setq, _first, null_));
                    action->append(_check);

                    final_section->beforelast(action);
                }
                else {
                    //Test to use the first element
                    action->append(operation);
                    action->append(lisp->create_instruction(l_setq, _first, null_));
                    action->append(lisp->create_instruction(l_setq, _value, _iterator));
                    loop->change(3, action);
                }
                iterator_variable = operation->index(2);
                break;
            }
            default:
                if (_id_var->index(1) == one_)
                    iterator_variable = _iterator;
                else {
                    iterator_variable = operation->index(1);
                }
                
                if (!compose->index(4)->isList()) {
                    std::wstringstream err;
                    err << L"'" << lisp->asString(labeltype) << "' can only apply to lists";
                    throw new Error(err.str());
                }
        }
        
        switch(labeltype) {
            //The function returns an element, we push it in #l
            case l_foldl:
                compose->liste[4] = _recipient;
                action = lisp->create_instruction(l_setq, _value, liste[2]);
                compose->beforelast(action);
                fold_ordering = final_section->index(0)->label();
                if (fold_ordering == l_block || fold_ordering == l_check || fold_ordering == l_ncheck)
                    final_section->last()->change(0, P(l_setq));
                else
                    final_section->change(0, P(l_setq));
                fold_ordering = 1;
                break;
            case l_scanl:
                if (!compose->index(4)->isList())
                    throw new Error("Error: 'scanl' can only apply to lists");
                action = lisp->create_instruction(l_setq, _value, liste[2]);
                compose->beforelast(action);
                compose->liste[4] = lisp->create_instruction(l_setq, _recipient, lisp->create_instruction(l_cons, liste[2], emptylist_));
                fold_ordering = 1;
                break;
            case l_foldr:
                compose->liste[4] = _recipient;
                action = lisp->create_instruction(l_setq, _value, liste[2]);
                compose->beforelast(action);
                fold_ordering = final_section->index(0)->label();
                if (fold_ordering == l_block || fold_ordering == l_check || fold_ordering == l_ncheck)
                    final_section->last()->change(0, P(l_setq));
                else
                    final_section->change(0, P(l_setq));
                loop->change(2, lisp->create_instruction(l_reverse, loop->index(2)));
                fold_ordering = 2;
                break;
            case l_scanr:
                if (!compose->index(4)->isList())
                    throw new Error("Error: 'scanr' can only apply to lists");
                action = lisp->create_instruction(l_setq, _value, liste[2]);
                compose->beforelast(action);
                compose->liste[4] = lisp->create_instruction(l_setq, _recipient, lisp->create_instruction(l_cons, liste[2], emptylist_));
                loop->change(2, lisp->create_instruction(l_reverse, loop->index(2)));
                fold_ordering = final_section->index(0)->label();
                if (fold_ordering == l_block || fold_ordering == l_check || fold_ordering == l_ncheck) {
                    final_section->last()->change(0, P(l_insert));
                    final_section->last()->append(zero_);
                }
                else {
                    final_section->change(0, P(l_insert));
                    final_section->append(zero_);
                }
                fold_ordering = 2;
                break;
            case l_foldl1: {
                fold_ordering = 1;
                compose->liste[4] = _recipient;
                action = lisp->create_instruction(l_setq, _first, true_);
                compose->beforelast(action);
                Element* recipient = final_section;
                int16_t label = recipient->index(0)->label();
                if (label == l_check || label == l_ncheck) {
                    recipient = recipient->last();
                    label = recipient->index(0)->label();
                }
                if (label == l_push)
                    recipient->change(0, P(l_setq));
                else {
                    if (label == l_insert) {
                        recipient->change(0, P(l_setq));
                        recipient->removelast();
                    }
                }
                break;
            }
            case l_scanl1:
                if (!compose->index(4)->isList())
                    throw new Error("Error: 'scanl1' can only apply to lists");
                fold_ordering = 1;
                action = lisp->create_instruction(l_setq, _first, true_);
                compose->beforelast(action);
                break;
            case l_foldr1: {
                fold_ordering = 2;
                compose->liste[4] = _recipient;
                action = lisp->create_instruction(l_setq, _first, true_);
                compose->beforelast(action);
                
                Element* recipient = final_section;
                int16_t label = recipient->index(0)->label();
                if (label == l_check || label == l_ncheck) {
                    recipient = recipient->last();
                    label = recipient->index(0)->label();
                }
                if (label == l_push)
                    recipient->change(0, P(l_setq));
                else {
                    if (label == l_insert) {
                        recipient->change(0, P(l_setq));
                        recipient->removelast();
                    }
                }
                
                loop->change(2, lisp->create_instruction(l_reverse, loop->index(2)));
                break;
            }
            case l_scanr1: {
                if (!compose->index(4)->isList())
                    throw new Error("Error: 'scanr1' can only apply to lists");
                fold_ordering = 2;
                action = lisp->create_instruction(l_setq, _first, true_);
                compose->beforelast(action);
                Element* recipient = final_section;
                int16_t label = recipient->index(0)->label();
                if (label == l_check || label == l_ncheck) {
                    recipient = recipient->last();
                    label = recipient->index(0)->label();
                }
                if (label == l_push) {
                    recipient->change(0, P(l_insert));
                    recipient->append(zero_);
                }

                loop->change(2, lisp->create_instruction(l_reverse, loop->index(2)));
                break;
            }
            case l_drop: {
                swprintf_s(buffer,20, L"#count%d", idx);
                wstring code(buffer);
                _check = P(code);
                while (findvariable(compose, _check)) {
                    code = L"#"+code;
                    _check = P(code);
                }
                action = lisp->create_instruction(l_size, _recipient);
                action = lisp->create_instruction(l_minus, one_, action);
                action = lisp->create_instruction(l_setq, _check, action);
                compose->beforelast(action);
                break;
            }
            case l_dropwhile:
                swprintf_s(buffer,20, L"#check%d", idx);
                wstring code(buffer);
                _check = P(code);
                while (findvariable(compose, _check)) {
                    code = L"#"+code;
                    _check = P(code);
                }
                action = lisp->create_instruction(l_setq, _check, true_);
                compose->beforelast(action);
                break;
        }
    }
    
    action = NULL;
    if (findvariable(first_element, _iterator)) {
        //We are in a "for" action through a map recomposition
        //(for i (map '+ '(1 2 3)) (+ i 10))
        // --> (map (+ i 10) (compose ...))
        // iterator_variable is: (+ #i1 #i1)
        // --> we replace "i" with the iterator_variable
        replacevariable(first_element, _iterator, iterator_variable);
        action = first_element;
    }
    else {
        if ((labeltype >= l_take && labeltype <= l_drop))
            action = first_element;
        else {
            if (first_element->isList()) {
                first_element = first_element->eval(lisp);
            }
            else
                if (first_element->isInstruction())
                    return first_element->eval(lisp); //we send an error message back
            
            if (first_element->isList()) {
                long sz = first_element->size();
                if (sz == 2) {
                    action = lisp->provideList();
                    // (map '(- 1) '(1 2 3 4))
                    if (first_element->index(0)->isInstruction()) {
                        action->append(first_element->index(0));
                        action->append(iterator_variable); //the iterator variable
                        action->append(first_element->index(1));
                    }
                    else { // (_map '(1 -) '(1 2 3 4))
                        // First we append the operator
                        if (first_element->index(1)->isInstruction()) {
                            action->append(first_element->index(1));
                            action->append(first_element->index(0)); //finally the number itself so as to have: (- x 1)
                            action->append(iterator_variable); //the position of the second number
                        }
                        else {
                            //This is a tricky case, the operator or the function could be given as an atom, which is
                            //should be evaluated later on the fly
                            action->append(P(l_checking));
                            action->append(first_element->index(0)); //this could be an instruction or else
                            action->append(first_element->index(1));
                            action->append(iterator_variable);
                        }
                    }
                }
                else {//lambda
                    if (sz && first_element->index(0)->label() == l_lambda) {
                        action = new Listlambda;
                        action->append(first_element);
                        switch (fold_ordering) {
                            case 0:
                                action->append(iterator_variable);
                                break;
                            case 1:
                                action->append(_value);
                                action->append(iterator_variable);
                                break;
                            case 2:
                                action->append(iterator_variable);
                                action->append(_value);
                                break;
                        }
                    }
                    else
                        action = liste[1];
                }
            }
            else {
                if (lisp->is_math_operator(first_element->type)) {
                    //This is a single operator (map only)
                    //we use the variable twice then
                    switch (fold_ordering) {
                        case 0:
                            if (labeltype == l_map) {
                                action = lisp->provideList();
                                action->append(first_element);
                                action->append(iterator_variable);
                                action->append(iterator_variable);
                            }
                            break;
                        case 1: {
                            action = lisp->provideList();
                            //We push our value first then the iterator variable
                            action->append(first_element);
                            action->append(_value);
                            action->append(iterator_variable);
                            break;
                        }
                        case 2: {
                            action = lisp->provideList();
                            //We push our iterator variable first
                            action->append(first_element);
                            action->append(iterator_variable);
                            action->append(_value);
                        }
                    }
                }
                else {
                    if (first_element->isAtom()) {
                        //then it has to be a function name or an instruction
                        //otherwise it will fail
                        action = lisp->provideList();
                        //if it is not a quoted expression, we add a l_mapping
                        if (liste[1] == first_element)
                            action->append(P(l_mapping));
                        
                        switch (fold_ordering) {
                            case 0:
                                //We will check on the fly what to do with this structure
                                action->append(first_element);
                                action->append(iterator_variable);
                                break;
                            case 1:
                                //We push our value first then the iterator variable
                                action->append(first_element);
                                action->append(_value);
                                action->append(iterator_variable);
                                break;
                            case 2:
                                //We push our iterator variable first
                                action->append(first_element);
                                action->append(iterator_variable);
                                action->append(_value);
                        }
                    }
                }
            }
        }
    }
    if (action == NULL) {
        wstring msg = L"Error: cannot use: '";
        msg += first_element->asString(lisp);
        msg += L"'";
        throw new Error(msg);
    }
    
    //If the value does not belong to a pool or the garbage
    //we need to keep track of it
    if (action->status != s_constant)
        lisp->garbaging(action);
    
    //The recipient variable
    List* test = NULL;
    switch(labeltype) {
            //The function returns an element, we push it in #l
        case l_map:
        case l_foldl:
        case l_scanl:
        case l_foldl1:
        case l_scanl1:
        case l_foldr:
        case l_scanr:
        case l_foldr1:
        case l_scanr1:
            operation->change(2,action);
            return compose;
        case l_cycle:
        case l_repeat: {
            //(push #l iterator_value)
            element->append(action);
            return compose;
        }
        case l_replicate:
            //l_loopcount, we add the integer value
            loop->liste[1] = action;
            return compose;
        case l_filter: {
            //The final_section should ALWAYS be the last element in the structure
            //We insert the test within the final section
            //(check action <fin>)
            test = lisp->create_instruction(l_check, action);
            break;
        }
        case l_take: {
            //The final_section should ALWAYS be the last element in the structure
            //(ncheck (< (size _recipient) action) (break) <fin>)
            test = lisp->create_instruction(l_ncheck);
            _check = lisp->create_instruction(l_size, _recipient);
            test->append(lisp->create_instruction(l_lower, _check, action));
            test->append(lisp->create_instruction(l_break));
            break;
        }
        case l_drop: {
            //The final_section should ALWAYS be the last element in the structure
            //(ncheck (> #check action) (+= #check 1) <fin>)
            test = lisp->create_instruction(l_ncheck);
            test->append(lisp->create_instruction(l_greater, _check, action));
            test->append(lisp->create_instruction(l_plusequal, _check, one_));
            break;
        }
        case l_takewhile: {
            //as long as the value is correct we store
            //The final_section should ALWAYS be the last element in the structure
            //(ncheck (action) (break) <fin>)
            //We insert the test within the final section
            test = lisp->create_instruction(l_ncheck, action, lisp->create_instruction(l_break));
            break;
        }
        case l_dropwhile: {
            //The final_section should ALWAYS be the last element in the structure
            //(check (not (and #check action)) (setq #check false) <fin>)
            //as long as the value is correct we drop it
            test = lisp->create_instruction(l_and, _check, action);
            test = lisp->create_instruction(l_not, test);
            test = lisp->create_instruction(l_check, test);
            test->append(lisp->create_instruction(l_setq, _check, false_));
            break;
        }
        default:
            return compose;
    }

    //Last operation for filter, take, takewhile, drop, dropwhile
    labeltype = final_section->index(0)->label();
    if (labeltype == l_check || labeltype == l_ncheck) {
        test->append(final_section->last());
        final_section->changelast(test);
    }
    else {
        test->append(loop->last());
        loop->changelast(test);
    }
    compose->liste[3] = test;
    return compose;
}

//--------------------------------------------------------------------------------
Element* Atome::transformargument(LispE* lisp) {
    if (name.back() == '+' || name.back() == '*' || name.back() == '%') {
        char a = name.back();
        u_ustring bare_name = name.substr(0, name.size()-1);
        int16_t l_name = lisp->encode(bare_name);
        Element* e = new Atomekleene(l_name, bare_name, a);
        return lisp->push_in_garbage(e);
    }
    return this;
}
//--------------------------------------------------------------------------------
Element* List::transformargument(LispE* lisp) {
    long sz = liste.size();

    if (!sz)
        return this;

    Element* element;
    int16_t label = liste[0]->label();

    if (label == l_quote) {
        element = new Listargumentquote(this);
        lisp->removefromgarbage(this);
        return lisp->push_in_garbage(element);
    }

    bool kleene = false;
    //We first evaluate all the next elements in the list
    for (long i = 1; i < sz; i++) {
        element = liste[i]->transformargument(lisp);
        if (element->label() == l_plus || element->label() == l_multiply || element->label() == l_mod) {
            if (liste[i-1]->argumentvalue() != NULL) {
                sz--;
                element = new Listkleene(liste[i-1], liste[i-1]->argumentvalue(), element->label());
                lisp->garbaging(element);
                liste[i-1] = element;
                liste.erase(i--);
            }
            else {
                if (i > 1)
                    throw new Error("Error: The Kleene operators (*+%) can only apply to a function call");
                else //In this case, the first element was not yet evaluated...
                    kleene = true;
            }
        }
        else
            liste[i] = element;
    }
    
    //Then we evaluate the initial label
    //This is a set description
    if (label >= l_set && label <= l_sets) {
        element = new Listargumentset(this);
        lisp->removefromgarbage(this);
        return lisp->push_in_garbage(element);
    }

    if (label >= l_dictionary && label <= l_dictionaryn) {
        element = new Argumentdictionary(lisp, this);
        lisp->removefromgarbage(this);
        return lisp->push_in_garbage(element);
    }

    bool sep = false;
    if (sz > 1 && liste[sz-2] == separator_) {
        sep = true;
    }


    //If it is a list, then we need to evaluate this element
    //We do not test; label == t_list, because in this case it could
    //be a constraint on the object type...
    if (liste[0]->isAtom()) {
        //This is either a data structure or a type constraint
        //It has to be integrated into a list... [string_ xxx]
        if (lisp->checkDataStructure(label)) {
            if (label >= t_atom && label <= t_maybe)
                element = new Listargumentlabel(this, label);
            else
                element = new Listargumentdata(this);
            
            lisp->removefromgarbage(this);
            return lisp->push_in_garbage(element);
        }
        
        //This is an executable...
        //In this case, it could be an embedded list of calls...
        if (liste[0]->isExecutable(lisp)) {
            element = liste.back();

            //If the last element is also an argument function, then eval returns its argument
            if (element->isArgumentFunction())
                element = new Listargumentfunction(this, element->argumentvalue());
            else {
                //We find the last atom in the sequence
                if (!element->isAtom()) {
                    for (long i = sz - 2; i > 0; i--) {
                        if (liste[i]->isAtom()) {
                            element = liste[i];
                            break;
                        }
                    }
                    if (!element->isAtom())
                        throw new Error("Error: Missing argument in defpat function");
                }
                element = new Listargumentfunction(this, element);
            }
            
            lisp->removefromgarbage(this);
            return lisp->push_in_garbage(element);
        }
    }
    
    //When liste[0] is also a liste, we need to evaluate it as well
    if (kleene) {
        element = liste[0]->transformargument(lisp);
        if (element->argumentvalue() == NULL)
            throw new Error("Error: The Kleene operators (*+%) can only apply to a function call");
        sz--;
        element = new Listkleene(element, element->argumentvalue(), element->label());
        lisp->garbaging(element);
        liste[0] = element;
        liste.erase(1);
    }
    else
        liste[0] = liste[0]->transformargument(lisp);

    if (sep) {
        element = new Listseparator(this);
        lisp->removefromgarbage(this);
        return lisp->push_in_garbage(element);
    }
    return this;
}
