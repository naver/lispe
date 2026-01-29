/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  unify.cxx
//
//


#include <stdio.h>
#include "lispe.h"
#include "segmentation.h"
#include "tools.h"
#include "tokens.h"

#ifdef UNIX
#include <sys/resource.h>
#endif

void launchthread(ThreadElement* the);
//------------------------------------------------------------------------------------------
//We return one value less since, the arity comprises the function definition as first element of a list
//For functions, it is one too many.
Element* LispE::pop(Element* e) {
    e->increment();
    removeStackElement();
    e->decrementkeep();
    return e;
}


long arity_value(unsigned long arity) {
    long nb = 0;
    while (arity != 2) {
        nb++;
        arity >>= 1;
    }
    return nb;
}

Element* eval_body_as_argument_min(LispE* lisp, Element* function, unsigned long arity, bool lmbd = false) {
    if (function->isInstruction()) {
        lisp->arity_check(function->label(), arity);
        Element* value = lisp->cloning(function->label());
        value->append(function);
        value->type = t_eval;
        return value;
    }
    
    if (function->isAtom())
        function = function->eval(lisp);
    
    if (function->isList()) {
        switch(function->function_label(lisp)) {
            case l_defpred:
                function = new List_predicate_eval((List*)function);
                break;
            case l_defprol:
                function = new List_prolog_eval((List*)function);
                break;
            case l_defpat:
                function = new List_pattern_eval((List*)function);
                break;
            case l_deflib:
                function = new List_library_eval((List*)function, arity_value(arity));
                break;
            case l_deflibpat:
                function = new List_library_pattern_eval((List*)function, arity_value(arity));
                break;
            case l_defun:
                function = new List_function_eval(lisp, (List*)function, arity_value(arity));
                break;
            case l_lambda:
                if (lmbd)
                    function = new List_call_lambda((List_lambda_eval*)function);
                else
                    function = new Atomefonction(function, t_lambda);
                break;
            default:
                throw new Error("Error: wrong function call");
        }
        
    }

    return function;
}

//This function is dedicated to executing callbacks
Element* lispe_eval_callback(LispE* lisp, Element* function, vector<Element*>& arguments) {
    const unsigned long arity = 1 << (arguments.size() + 1);
    List* callback = (List*)eval_body_as_argument_min(lisp, function->eval(lisp), arity);
    
    for (long i = 0; i < arguments.size(); i++) {
        callback->append(lisp->quoted(arguments[i]));
    }
    
    Element* e = null_;
    try {
        e = callback->eval(lisp);
    }
    catch (Error* err) {
        e = err;
    }
    
    e->increment();
    callback->force_release();
    e->decrementkeep();
    return e;
}

//We evaluation function beforehand
Element* eval_body_as_argument(LispE* lisp, Element* function, const unsigned long arity) {
    return eval_body_as_argument_min(lisp, function->eval(lisp), arity);
}

Element* eval_body_as_argument(LispE* lisp, Element* function) {
    return eval_body_as_argument_min(lisp, function->eval(lisp), P_TWO);
}


Element* List_call_lambda::eval(LispE* lisp) {
    //The first element is the lambda itself,
    //The rest the arguments...
    // It is either a lambda or a function
    //otherwise it's an error
    Element* element;
        
    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    
    //((lambda (x y) (if (eq x 0) (* 2 y) (self (- x 1) (+ y 2)))) 20 1)
    //( (lambda (x) (if (eq x 1) 1 (+ x (self (- x 1))))) 20)
    
    //terminal helps detects terminal recursion...
    //A terminal recursion can be processed in a quasi iterative way.
    //When a if or cond is processed, their last element is automatically set to terminal
    //When it corresponds to a function call, then it is processed here
    //We do not create any new stack element and we store our arguments back into the stack
    //with their new values in case of terminal recursion.
    
    if (terminal && lisp->called() == body) {
        sameSizeTerminalArguments(lisp, body->parameters);
        lisp->check_end_trace(lisp->check_trace_in_function(), lisp->trace);
        return terminal_;
    }
    
    // For each of the parameters we record in the stack
    long i;
    vecte<Element*> recorded(nbarguments);

    lisp->setStack();
    char localtrace = lisp->trace;
    
    try {
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        for (i = 0; i < nbarguments; i++) {
            //The evaluation must be done on the previous stage of the stack
            element = liste[i+1]->eval(lisp);
            recorded.push_raw(lisp->record_or_replace(element, body->labels[i]));
        }
    }
    catch (Error* err) {
        lisp->resetStack();
        //We must call reset_in_stack from end to begin
        //the status should popped out from Stack::status
        for (i = nbarguments - 1; i >= 0; i--)
            lisp->reset_in_stack(recorded[i], body->labels[i]);
        throw err;
    }

    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.
    char tr = lisp->check_trace_in_function();
    long nbinstructions = body->size();
    Element* stackfunction = lisp->exchangestackfunction(body);
    try {
        do {
            if (nbinstructions == 3)
                element = body->liste[2]->eval(lisp);
            else {
                element = null_;
                for (i = 2; i < nbinstructions && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = body->liste[i]->eval(lisp);
                }
            }
            if (element->type == l_return) {
                Element* e = element->eval(lisp);
                element->release();
                //We must call reset_in_stack from end to begin
                //the status should popped out from Stack::status
                for (i = nbarguments - 1; i >= 0; i--)
                    lisp->reset_in_stack(recorded[i], body->labels[i], e);
                lisp->resetStack();
                lisp->check_end_trace(tr, localtrace);
                return e;
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        lisp->resetStack();
        lisp->check_end_trace(tr, localtrace);
        lisp->setstackfunction(stackfunction);
        for (i = nbarguments - 1; i >= 0; i--)
            lisp->reset_in_stack(recorded[i], body->labels[i]);
        throw err;
    }
    
    for (i = nbarguments - 1; i >= 0; i--)
        lisp->reset_in_stack(recorded[i], body->labels[i], element);
    lisp->setstackfunction(stackfunction);
    lisp->resetStack();
    lisp->check_end_trace(tr, localtrace);
    return element;
}

//Execution of a function as well as the shift of parameters with arguments
//For this specific lambda, we do not create a stack
Element* List_lambda_eval::eval_lambda_min(LispE* lisp) {
    Element* stackfunction = lisp->exchangestackfunction(this);
    Element* element;
    
    try {
        if (nbinstructions == 3)
            element = liste[2]->eval(lisp);
        else {
            element = null_;
            for (int16_t i = 2; i < nbinstructions && element->type != l_return; i++) {
                element->release();
                element = liste[i]->eval(lisp);
            }
        }
        if (element->type == l_return) {
            Element* e = element->eval(lisp);
            element->release();
            lisp->setstackfunction(stackfunction);
            //This version protects 'e' from being destroyed in the stack.
            return e;
        }
    }
    catch (Error* err) {
        lisp->setstackfunction(stackfunction);
        throw err;
    }
    
    lisp->setstackfunction(stackfunction);
    //This version protects 'e' from being destroyed in the stack.
    return element;
}

Element* List_from::eval(LispE* lisp) {
    if (lisp->current_instance == NULL) {
        lisp->setStack();
        return lisp->check_error(this, new Error("Error: cannot execute this method out of an instance"), idxinfo);
    }
    
    Element* element = null_;
    try {
        for (long i = 2; i < liste.size(); i++) {
            element->release();
            element = liste[i]->eval(lisp);
        }
    }
    catch(Error* err) {
        throw err;
    }
    return element;
}


void List_predicate_eval::check_body(LispE* lisp, Element** e) {
    if (lisp->current_space != space) {
        *e = new List_predicate_eval(this, (List*)lisp->returnBody(this), lisp->current_space);
        lisp->storeforgarbage(*e);
    }
}

void List_prolog_eval::check_body(LispE* lisp, Element** e) {
    if (lisp->current_space != space) {
        *e = new List_prolog_eval(this, (List*)lisp->returnBody(this), lisp->current_space);
        lisp->storeforgarbage(*e);
    }
}

void List_pattern_eval::check_body(LispE* lisp, Element** e) {
    if (lisp->current_space != space) {
        *e = new List_pattern_eval(this, (List*)lisp->returnBody(this), lisp->current_space);
        lisp->storeforgarbage(*e);
    }
}

void List_function_eval::check_body(LispE* lisp, Element** e) {
    if (lisp->current_space != space) {
        *e = new List_function_eval(lisp, this, (List*)lisp->returnBody(this), lisp->current_space);
        lisp->storeforgarbage(*e);
    }
}

void List_thread_eval::check_body(LispE* lisp, Element** e) {
    if (lisp->current_space != space) {
        *e = new List_thread_eval(lisp, this, (List*)lisp->returnBody(this), lisp->current_space);
        lisp->storeforgarbage(*e);
    }
}

Element* List_instance_eval::eval(LispE* lisp) {
    Element* instance = liste[0]->eval(lisp);
    long beg = 1;

    //c_label != 0, means that the instance was created within a (withclass instance)
    //else it means that the second argument is a class definition

    int16_t c_label = class_label;
    if (!c_label) {
        beg = 2;
        c_label = liste[1]->label();
    }
    
    if (instance->type != c_label) {
        instance->release();
        lisp->setStack();
        return lisp->check_error(this, new Error("Error: mismatch between class instance and class definition"), idxinfo);
    }

    lisp->checkState(this);
    char localtrace = lisp->trace;
    char tr = lisp->check_trace_in_function();
    if (localtrace == debug_inside_function) {
        lisp->trace = debug_inside_function;
        localtrace = debug_next;
    }
    
    int16_t space = lisp->current_space;
    lisp->current_space = ((List_instance*)instance)->space;
    List_instance* previous = lisp->current_instance;
    lisp->current_instance = (List_instance*)instance;
    Element* element = null_;
    
    try {
        for (long i = beg; i < size(); i++) {
            element->release();
            element = liste[i]->eval(lisp);
        }
    }
    catch(Error* err) {
        lisp->current_instance = previous;
        lisp->current_space = space;
        lisp->check_end_trace(tr, localtrace);
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->current_space = space;
    lisp->current_instance = previous;
    lisp->resetStack();
    lisp->check_end_trace(tr, localtrace);
    return element;
}

Element* List::execute_instance_function(LispE* lisp, List_instance* instance) {
    
    int16_t space = lisp->current_space;
    lisp->current_space = instance->space;
    List_instance* previous = lisp->current_instance;
    lisp->current_instance = instance;
    Element* element = null_;
    
    lisp->setStack();
    char localtrace = lisp->trace;
    char tr = lisp->check_trace_in_function();
    if (localtrace == debug_inside_function) {
        lisp->trace = debug_inside_function;
        localtrace = debug_next;
    }

    try {
        for (long i = 1; i < liste.size(); i++) {
            element->release();
            liste[i]->check_body(lisp, &liste.items->buffer[i]);
            element = liste[i]->eval(lisp);
        }
    }
    catch(Error* err) {
        lisp->current_instance = previous;
        lisp->current_space = space;
        lisp->resetStack();
        lisp->check_end_trace(tr, localtrace);
        throw err;
    }
    lisp->current_space = space;
    lisp->current_instance = previous;
    lisp->resetStack();
    lisp->check_end_trace(tr, localtrace);
    return element;
}
    

Element* List_class_eval::eval(LispE* lisp) {
    //We create a List_instance instance, which will have its space, class_label and names based on the class definition.
    //We also initialize the list with emptyatom elements.
    long i;
    long sz = class_definition->names.size();
    int16_t label;
    
    lisp->checkState(this);

    char localtrace = lisp->trace;
    char tr = lisp->check_trace_in_function();
    
    List_instance* nv = new List_instance(class_definition->space, class_definition->class_label, class_definition->names, emptyatom_, sz);
    int16_t currentspace = lisp->current_space;
    List_instance* current = lisp->current_instance;
    lisp->current_instance = nv;
    
    lisp->current_space = class_definition->space;
    
    List* p = (List*)class_definition->parameters;
    Element* data;
    Element* element;

    nv->increment();

    if (class_definition->same) {
        try {
            if (sz != size() - 1)
                throw new Error("Error: argument list mismatch with the parameter list");
    
            // For each of the parameters we record in the stack
            for (i = 0; i < sz; i++) {
                element = liste[i+1];
                if (element->isNotEmptyList()) {
                    label = element->index(0)->label();
                    switch(label) {
                        case l_record_in_stack: {
                            ((List_record_in_stack_eval*)element)->eval_in_class(lisp);
                            break;
                        }
                        case l_setqi:
                            element->eval(lisp);
                            break;
                        default:
                            data = element->eval(lisp);
                            if (data == emptyatom_)
                                nv->liste[i] = null_;
                            else {
                                nv->liste[i] = data;
                                data->increment();
                            }
                    }
                }
                else {
                    data = element->eval(lisp)->duplicate_constant(lisp);
                    nv->liste[i] = data;
                    data->increment();
                }
            }
            
            for (i = 0; i < sz; i++) {
                if (nv->liste[i] == emptyatom_)
                    throw new Error("Error: Missing arguments");
            }
        }
        catch (Error* err) {
            lisp->current_space = currentspace;
            lisp->current_instance = current;
            nv->decrement();
            lisp->check_end_trace(tr, localtrace);
            return lisp->check_error(this, err, idxinfo);
        }
        
        lisp->current_space = currentspace;
        lisp->current_instance = current;
        nv->decrementkeep();
        lisp->resetStack();
        lisp->check_end_trace(tr, localtrace);
        return nv;
    }
    
    try {
        long szlist = size() - 1;
        
        for (i = 0; i < szlist; i++) {
            element = liste[i+1];
            if (element->isNotEmptyList()) {
                label = element->index(0)->label();
                switch(label) {
                    case l_record_in_stack: {
                        ((List_record_in_stack_eval*)element)->eval_in_class(lisp);
                        break;
                    }
                    case l_setqi:
                        element->eval(lisp);
                        break;
                    default:
                        data = element->eval(lisp);
                        if (data == emptyatom_)
                            nv->liste[i] = null_;
                        else {
                            nv->liste[i] = data;
                            data->increment();
                        }
                }
            }
            else {
                data = element->eval(lisp)->duplicate_constant(lisp);
                nv->liste[i] = data;
                data->increment();
            }
        }
        
        for (i = 0; i < sz; i++) {
            if (nv->liste[i] != emptyatom_)
                continue;
            
            element = p->liste[i];
            switch(element->size()) {
                case 0:
                    throw new Error("Error: Missing arguments");
                case 1:
                    data = null_;
                    break;
                default:
                    data = element->index(1)->eval(lisp)->duplicate_constant(lisp); //default value
                    data->increment();
            }
            nv->liste[i] = data;
        }
    }
    catch (Error* err) {
        lisp->current_space = currentspace;
        lisp->current_instance = current;
        nv->decrement();
        lisp->check_end_trace(tr, localtrace);
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->current_space = currentspace;
    lisp->current_instance = current;
    nv->decrementkeep();
    lisp->resetStack();
    lisp->check_end_trace(tr, localtrace);
    return nv;
}

Element* List::create_class_instance(LispE* lisp, List_class_definition* lce) {
    List_class_eval lceval(this, lce);
    return lceval.eval(lisp);
}

Element* List_function_eval::eval(LispE* lisp) {
    //terminal helps detects terminal recursion...
    //A terminal recursion can be processed in a quasi iterative way.
    //When a if or cond is processed, their last element is automatically set to terminal
    //When it corresponds to a function call, then it is processed here
    //We do not create any new stack element and we store our arguments back into the stack
    //with their new values in case of terminal recursion.
    try {
        if (terminal && lisp->called() == body) {
            if (same)
                sameSizeTerminalArguments(lisp, (List*)parameters);
            else
                differentSizeTerminalArguments(lisp, (List*)parameters, nbarguments, defaultarguments);
            lisp->check_end_trace(lisp->check_trace_in_function(), lisp->trace);
            return terminal_;
        }
        
        if (same)
            sameSizeNoTerminalArguments(lisp, body, (List*)parameters);
        else
            differentSizeNoTerminalArguments(lisp, body, (List*)parameters, nbarguments, defaultarguments);
    }
    catch(Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }

    //We check if the current function belongs to the current space, to avoid using variables from the current_instance, which should be out of range.
    lisp->check_space(space);

    lisp->checkState(this);
    char localtrace = lisp->trace;
    char tr = lisp->check_trace_in_function();
    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.

    long i;
    long nbinstructions = body->size();
    Element* element;
    try {
        do {
            if (nbinstructions == 4)
                element = body->liste[3]->eval(lisp);
            else {
                element = null_;
                for (i = 3; i < nbinstructions && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = body->liste[i]->eval(lisp);
                }
            }
            if (element->type == l_return) {
                Element* current_value = element->eval(lisp);
                element->release();
                //This version protects 'e' from being destroyed in the stack.
                lisp->resetStack();
                lisp->check_end_trace(tr, localtrace);
                return lisp->pop(current_value);
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        lisp->pop();
        lisp->check_end_trace(tr, localtrace);
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    lisp->check_end_trace(tr, localtrace);
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}

Element* List_thread_eval::eval(LispE* lisp) {
    //We are already running this thread, it is a recursive call
    //we treat as a regular function
    if (lisp->thread_body != NULL && lisp->thread_body->current_body == body)
        return eval_function(lisp, body);

    //We create a new lisp container for the thread
    LispE* thread_lisp = new LispE(lisp, this, body);
    Element* parameters;
    
    long nbarguments = liste.size()-1;
    //the third element is the argument list .
    //we need our body to be the same number
    parameters = body->liste[2];
    long defaultarguments = parameters->argumentsize(nbarguments);
    if (defaultarguments == -1) {
        wstring message = L"Error: Wrong number of arguments in call to: '(";
        message += body->liste[1]->asString(lisp);
        message += L" ";
        message += body->liste[2]->asString(lisp);
        message += L"...)'";
        throw new Error(message);
    }
    
    if (defaultarguments == parameters->size())
        sameSizeNoTerminalArguments_thread(lisp, thread_lisp, body, (List*)parameters);
    else
        differentSizeNoTerminalArguments_thread(lisp, thread_lisp, body, (List*)parameters, nbarguments, defaultarguments);
    
    lisp->check_space(-1);
    //The thread can be of course recursive, but we do not want this recursivity
    //to trigger a new thread at each call...
    //We only create a thread once... The next calls will be executed
    //as regular functions
    ThreadElement* the = new ThreadElement(thread_lisp);
    the->thid = new std::thread(launchthread, the);
    if (the->thid == NULL) {
        delete the;
        throw new Error("Error: Too many threads created. Cannot execute it...");
    }
    
    lisp->delegation->clean_threads(the);
    return True_;
}

Element* List_library_eval::eval(LispE* lisp) {
    try {
        if (same)
            sameSizeNoTerminalArguments(lisp, body, parameters);
        else
            differentSizeNoTerminalArguments(lisp, body, parameters, nbarguments, defaultarguments);
    }
    catch(Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }

    Element* element;
    try {
        lisp->checkState(this);
        element = body->liste[3]->eval(lisp);
    }
    catch (Error* err) {
        lisp->pop();
        return lisp->check_error(this, err, idxinfo);
    }
#ifndef LISPE_WASM_NO_EXCEPTION
    catch (void* x) {
        lisp->pop();
        if (((Error*)x)->type == t_error)
            return lisp->check_error(this, (Error*)x, idxinfo);
        return lisp->check_error(this, new Error("Unknown error"), idxinfo);
    }
#endif
    lisp->resetStack();
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}

Element* List_library_pattern_eval::eval(LispE* lisp) {
    List* arguments = lisp->provideList();
    Element* element;
    Element* current_body;
    
    long i;
    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    long nbarguments = liste.size()-1;
    int16_t ilabel = -1;
    int16_t sublabel = -1;
    char match;
    char depth = lisp->depths[function_label] - 1;
    lisp->checkState(this);
    char localtrace = lisp->trace;

    try {
        for (i = 1; i <= nbarguments; i++) {
            element = liste[i]->eval(lisp);
            
            ilabel = lisp->extractdynamiclabel(element, depth);
            if (ilabel > l_final && element->type != t_data) {
                match = lisp->getDataStructure(ilabel)->check_match(lisp,element);
                if (match != check_ok) {
                    element->release();
                    if (match == check_mismatch)
                        throw new Error(L"Error: Size mismatch between argument list and data structure definition");
                    else {
                        std::wstringstream message;
                        message << L"Error: Mismatch on argument: " << match;
                        message << " (" << lisp->asString(lisp->getDataStructure(ilabel)->index(match)->label()) << " required)";
                        throw new Error(message.str());
                    }
                }
            }
            //We keep the track of the first element as it used as an index to gather pattern methods
            if (i == 1)
                sublabel = ilabel;
            arguments->append(element->duplicate_constant(lisp));
        }
    }
    catch (Error* err) {
        arguments->release();
        return lisp->check_error(this, err, idxinfo);
    }

    char tr = lisp->check_trace_in_function();

    current_body = NULL;
    auto& functions = lisp->delegation->method_pool[0]->at(function_label);
    auto subfunction = functions.find(sublabel);
    if (subfunction == functions.end()) {
        sublabel = v_null;
        //We check, if we have a rollback function
        subfunction = functions.find(sublabel);
        if (subfunction == functions.end()) {
            arguments->release();
            wstring message = L"Error: Could not find a match for function: '";
            message += lisp->asString(function_label);
            message += L"'";
            lisp->check_end_trace(tr, localtrace);
            return lisp->check_error(this, new Error(message), idxinfo);
        }
    }

    current_body = subfunction->second[0];

    ilabel = 1;
    match = 0;
    long sz = subfunction->second.size();
    Stackelement* sta = lisp->topstack();
    lisp->push(current_body);
    
    while (current_body != NULL) {
        element = current_body->index(2);
        if (element->size() == nbarguments) {
            //if (lisp->current_instance)
            //    lisp->current_instance->store_variables(lisp->topstack());
            match = true;
            for (i = 0; i < nbarguments && match; i++) {
                match = element->index(i)->unify(lisp, arguments->liste[i], true);
            }
            
            if (match) {
                if (terminal && sta->called() == current_body) {
                    lisp->remove_sub_stack(sta);
                    arguments->release();
                    lisp->resetStack();
                    lisp->check_end_trace(tr, localtrace);
                    return terminal_;
                }
                lisp->setstackfunction(current_body, space);
                break;
            }
            
            lisp->clear_top_stack();
        }
        current_body = NULL;
        if (ilabel < sz)
            current_body = subfunction->second[ilabel++];
        else {
            if (sublabel != v_null) {
                sublabel = v_null;
                ilabel = 1;
                //We check, if we have a rollback function
                subfunction = functions.find(sublabel);
                if (subfunction != functions.end()) {
                    current_body = subfunction->second[0];
                    sz = subfunction->second.size();
                }
            }
        }
    }

    if (!match) {
        lisp->pop();
        arguments->release();
        wstring message = L"Error: Could not find a match for function: '";
        message += lisp->asString(function_label);
        message += L"'";
        return lisp->check_error(this, new Error(message), idxinfo);
    }
    try {
        lisp->checkState(this);
        element = ((List*)current_body)->liste[3]->eval(lisp);
    }
    catch (Error* err) {
        lisp->pop();
        return lisp->check_error(this, err, idxinfo);
    }
#ifndef LISPE_WASM_NO_EXCEPTION
    catch (void* x) {
        lisp->pop();
        if (((Error*)x)->type == t_error)
            return lisp->check_error(this, (Error*)x, idxinfo);
        return lisp->check_error(this, new Error("Unknown error"), idxinfo);
    }
#endif
    lisp->resetStack();
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}

Element* List_data_eval::eval(LispE* lisp) {
    Element* first = liste[0];
    if (first->isList())
        first = first->eval(lisp);
    Element* data = lisp->getDataStructure(first->label());
    List* values = lisp->provideList();
    values->append(first);
    Element* element;
    long nbarguments = liste.size()-1;
    try {
        for (long i = 1; i <= nbarguments; i++) {
            element = liste[i]->eval(lisp);
            values->append(element->duplicate_constant(lisp));
        }
    }
    catch (Error* err) {
        values->clear();
        delete values;
        throw err;
    }
    
    char res = data->check_match(lisp,values);
    if (res != check_ok) {
        values->clear();
        delete values;
        lisp->setStack();
        if (res == check_mismatch)
            return lisp->check_error(this, new Error(L"Error: Size mismatch between argument list and data structure definition"), idxinfo);
        else {
            std::wstringstream message;
            message << L"Error: Mismatch on argument: " << (int)res;
            message << " (" << lisp->asString(data->index((int)res)->label()) << " required)";
            return lisp->check_error(this, new Error(message.str()), idxinfo);
        }
    }
    values->type = t_data;
    return values;
}

/*
 ------------------------------------------------------------------------
 Evaluation of defpat rules
 ------------------------------------------------------------------------
*/

Element* List_pattern_eval::eval(LispE* lisp) {
    List* arguments = lisp->provideList();
    Element* element;
    Element* current_body;
    
    long i;
    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    long nbarguments = liste.size()-1;
    int16_t ilabel = -1;
    int16_t sublabel = -1;
    char match;
    char depth = lisp->depths[function_label] - 1;
    lisp->checkState(this);
    char localtrace = lisp->trace;

    try {
        for (i = 1; i <= nbarguments; i++) {
            element = liste[i]->eval(lisp);
            
            ilabel = lisp->extractdynamiclabel(element, depth);
            if (ilabel > l_final && element->type != t_data) {
                match = lisp->getDataStructure(ilabel)->check_match(lisp,element);
                if (match != check_ok) {
                    element->release();
                    if (match == check_mismatch)
                        throw new Error(L"Error: Size mismatch between argument list and data structure definition");
                    else {
                        std::wstringstream message;
                        message << L"Error: Mismatch on argument: " << match;
                        message << " (" << lisp->asString(lisp->getDataStructure(ilabel)->index(match)->label()) << " required)";
                        throw new Error(message.str());
                    }
                }
            }
            //We keep the track of the first element as it used as an index to gather pattern methods
            if (i == 1)
                sublabel = ilabel;
            arguments->append(element->duplicate_constant(lisp));
        }
    }
    catch (Error* err) {
        arguments->release();
        return lisp->check_error(this, err, idxinfo);
    }

    char tr = lisp->check_trace_in_function();

    current_body = NULL;
    int16_t space = lisp->delegation->getPatternMethods(function_label, lisp->current_space);
    if (space == -1) {
        arguments->release();
        wstring message = L"Error: Could not find a match for function: '";
        message += lisp->asString(function_label);
        message += L"'";
        return lisp->check_error(this, new Error(message), idxinfo);
    }
    
    auto& functions = lisp->delegation->method_pool[space]->at(function_label);
    auto subfunction = functions.find(sublabel);
    if (subfunction == functions.end()) {
        sublabel = v_null;
        //We check, if we have a rollback function
        subfunction = functions.find(sublabel);
        if (subfunction == functions.end()) {
            arguments->release();
            wstring message = L"Error: Could not find a match for function: '";
            message += lisp->asString(function_label);
            message += L"'";
            lisp->check_end_trace(tr, localtrace);
            return lisp->check_error(this, new Error(message), idxinfo);
        }
    }

    current_body = subfunction->second[0];

    ilabel = 1;
    match = 0;
    long sz = subfunction->second.size();
    Stackelement* sta = lisp->topstack();
    lisp->push(current_body);
    
    while (current_body != NULL) {
        element = current_body->index(2);
        if (element->size() == nbarguments) {
            //if (lisp->current_instance)
            //    lisp->current_instance->store_variables(lisp->topstack());
            match = true;
            for (i = 0; i < nbarguments && match; i++) {
                match = element->index(i)->unify(lisp, arguments->liste[i], true);
            }
            
            if (match) {
                if (terminal && sta->called() == current_body) {
                    lisp->remove_sub_stack(sta);
                    arguments->release();
                    lisp->resetStack();
                    lisp->check_end_trace(tr, localtrace);
                    return terminal_;
                }
                lisp->setstackfunction(current_body, space);
                break;
            }
            
            lisp->clear_top_stack();
        }
        current_body = NULL;
        if (ilabel < sz)
            current_body = subfunction->second[ilabel++];
        else {
            if (sublabel != v_null) {
                sublabel = v_null;
                ilabel = 1;
                //We check, if we have a rollback function
                subfunction = functions.find(sublabel);
                if (subfunction != functions.end()) {
                    current_body = subfunction->second[0];
                    sz = subfunction->second.size();
                }
            }
        }
    }

    if (!match) {
        lisp->pop();
        arguments->release();
        wstring message = L"Error: Could not find a match for function: '";
        message += lisp->asString(function_label);
        message += L"'";
        return lisp->check_error(this, new Error(message), idxinfo);
    }
            
    try {
        nbarguments = current_body->size();
        do {
            if (nbarguments == 4)
                element = current_body->index(3)->eval(lisp);
            else {
                element = null_;
                for (i = 3; i < nbarguments && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = current_body->index(i)->eval(lisp);
                }
            }
            if (element->type == l_return) {
                current_body = element->eval(lisp);
                element->release();
                //This version protects 'e' from being destroyed in the stack.
                arguments->release(current_body);
                lisp->resetStack();
                lisp->check_end_trace(tr, localtrace);
                return lisp->pop(current_body);
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        arguments->release();
        lisp->pop();
        lisp->check_end_trace(tr, localtrace);
        return lisp->check_error(this, err, idxinfo);
    }

    arguments->release(element);
    lisp->resetStack();
    lisp->check_end_trace(tr, localtrace);
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}

/*
 ------------------------------------------------------------------------
 Evaluation of defpred rules
 ------------------------------------------------------------------------
*/

Element* List_predicate_eval::eval(LispE* lisp) {
    List* arguments = lisp->provideList();
    Element* element;
    Element* current_body;
    
    long i;
    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    long nbarguments = liste.size()-1;
    int16_t ilabel = -1;
    int16_t sublabel = -1;
    char match;
    char depth = lisp->depths[function_label] - 1;
    
    lisp->checkState(this);
    char localtrace = lisp->trace;
    
    try {
        for (i = 1; i <= nbarguments; i++) {
            element = liste[i]->eval(lisp);
            
            ilabel = lisp->extractdynamiclabel(element, depth);
            if (ilabel > l_final && element->type != t_data) {
                match = lisp->getDataStructure(ilabel)->check_match(lisp,element);
                if (match != check_ok) {
                    element->release();
                    throw &lisp->delegation->predicate_error;
                }
            }
            //We keep the track of the first element as it used as an index to gather pattern methods
            if (i == 1)
                sublabel = ilabel;
            arguments->append(element->duplicate_constant(lisp));
        }
    }
    catch (Error* err) {
        arguments->release();
#ifndef LISPE_WASM_NO_EXCEPTION
        err->release();
#endif
        return null_;
    }

    char tr = lisp->check_trace_in_function();

    current_body = NULL;
    int16_t space = lisp->delegation->getPatternMethods(function_label, lisp->current_space);
    if (space == -1) {
        arguments->release();
        wstring message = L"Error: Could not find a match for function: '";
        message += lisp->asString(function_label);
        message += L"'";
        return lisp->check_error(this, new Error(message), idxinfo);
    }

    auto& functions = lisp->delegation->method_pool[space]->at(function_label);
    auto subfunction = functions.find(sublabel);
    if (subfunction == functions.end()) {
        sublabel = v_null;
        //We check, if we have a rollback function
        subfunction = functions.find(sublabel);
        if (subfunction == functions.end()) {
            arguments->release();
            lisp->resetStack();
            lisp->check_end_trace(tr, localtrace);
            return null_;
        }
    }

    current_body = subfunction->second[0];

    ilabel = 1;
    match = 0;
    long sz = subfunction->second.size();
    lisp->push(current_body);
    Element* copying;
    Element* ele;
    
    while (current_body != NULL) {
        match = false;
        while (!match && current_body != NULL) {
            element = current_body->index(2);
            if (element->size() == nbarguments) {
                //if (lisp->current_instance)
                //    lisp->current_instance->store_variables(lisp->topstack());

                match = true;
                copying = null_;
                for (i = 0; i < nbarguments && match; i++) {
                    ele = element->index(i);
                    if (ele->isUnifiable()) {
                        copying = arguments->liste[i]->fullcopy();
                        match = ele->unify(lisp, copying, true);
                    }
                    else {
                        copying = null_;
                        match = ele->unify(lisp,arguments->liste[i], true);
                    }
                }
                
                if (match) {
                    lisp->setstackfunction(current_body, space);
                    break;
                }
                copying->release();
                lisp->clear_top_stack();
            }
            current_body = NULL;
            if (ilabel < sz)
                current_body = subfunction->second[ilabel++];
            else {
                if (sublabel != v_null) {
                    sublabel = v_null;
                    ilabel = 1;
                    //We check, if we have a rollback function
                    subfunction = functions.find(sublabel);
                    if (subfunction != functions.end()) {
                        current_body = subfunction->second[0];
                        sz = subfunction->second.size();
                    }
                }
            }
        }
        if (!match) {
            lisp->pop();
            arguments->release();
            lisp->resetStack();
            lisp->check_end_trace(tr, localtrace);
            return null_;
        }
        
        bool success = true;
        element = true_;
        try {
            long nbinstructions = current_body->size();
            if (nbinstructions == 4) {
                element = current_body->index(3)->eval(lisp);
                success = element->Boolean();
            }
            else {
                element = true_;
                success = true;
                for (i = 3; i < nbinstructions && element->type != l_return && success; i++) {
                    element->release();
                    element = current_body->index(i)->eval(lisp);
                    success = element->Boolean();
                }
            }
            if (element->type == l_return) {
                current_body = element->eval(lisp);
                element->release();
                //This version protects 'e' from being destroyed in the stack.
                arguments->release();
                lisp->resetStack();
                lisp->check_end_trace(tr, localtrace);
                return lisp->pop(current_body);
            }
        }
        catch (Error* err) {
#ifndef LISPE_WASM_NO_EXCEPTION
        err->release();
#endif
            success = false;
        }
        if (success) {
            arguments->release();
            lisp->resetStack();
            lisp->check_end_trace(tr, localtrace);
            //This version protects 'e' from being destroyed in the stack.
            return lisp->pop(element);
        }
        element->release();
        current_body = NULL;
        if (ilabel < sz) {
            lisp->clear_top_stack();
            current_body = subfunction->second[ilabel++];
        }
        else {
            if (sublabel != v_null) {
                sublabel = v_null;
                ilabel = 1;
                //We check, if we have a rollback function
                subfunction = functions.find(sublabel);
                if (subfunction != functions.end()) {
                    lisp->clear_top_stack();
                    current_body = subfunction->second[0];
                    sz = subfunction->second.size();
                }
            }
        }
    }
    arguments->release();
    lisp->pop(null_);
    lisp->check_end_trace(tr, localtrace);
    wstring message = L"Error: Could not find a match for function: '";
    message += lisp->asString(function_label);
    message += L"'";
    return lisp->check_error(this, new Error(message), idxinfo);
}

/*
 ------------------------------------------------------------------------
 Evaluation of defprol rules
 ------------------------------------------------------------------------
*/

Element* List_prolog_eval::eval(LispE* lisp) {
    List* arguments = lisp->provideList();
    Element* element;
    Element* current_body;
    
    long i;
    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    long nbarguments = liste.size()-1;
    int16_t ilabel = -1;
    int16_t sublabel = -1;
    char match;
    char depth = lisp->depths[function_label] - 1;

    lisp->checkState(this);
    char localtrace = lisp->trace;

    try {
        for (i = 1; i <= nbarguments; i++) {
            element = liste[i]->eval(lisp);
            
            ilabel = lisp->extractdynamiclabel(element, depth);
            if (ilabel > l_final && element->type != t_data) {
                match = lisp->getDataStructure(ilabel)->check_match(lisp,element);
                if (match != check_ok) {
                    element->release();
                    throw &lisp->delegation->predicate_error;
                }
            }
            //We keep the track of the first element as it used as an index to gather pattern methods
            if (i == 1)
                sublabel = ilabel;
            arguments->append(element->duplicate_constant(lisp));
        }
    }
    catch (Error* err) {
        arguments->release();
#ifndef LISPE_WASM_NO_EXCEPTION
        err->release();
#endif
        return null_;
    }

    char tr = lisp->check_trace_in_function();

    current_body = NULL;
    int16_t space = lisp->delegation->getPatternMethods(function_label, lisp->current_space);
    if (space == -1) {
        arguments->release();
        wstring message = L"Error: Could not find a match for function: '";
        message += lisp->asString(function_label);
        message += L"'";
        return lisp->check_error(this, new Error(message), idxinfo);
    }

    auto& functions = lisp->delegation->method_pool[space]->at(function_label);
    auto subfunction = functions.find(sublabel);
    if (subfunction == functions.end()) {
        sublabel = v_null;
        //We check, if we have a rollback function
        subfunction = functions.find(sublabel);
        if (subfunction == functions.end()) {
            arguments->release();
            lisp->resetStack();
            lisp->check_end_trace(tr, localtrace);
            return null_;
        }
    }

    current_body = subfunction->second[0];

    ilabel = 1;
    match = 0;
    long sz = subfunction->second.size();
    lisp->push(current_body);
    Element* copying;
    Element* ele;
    
    List* final_result = lisp->provideList();
    bool end_of_execution = false;
    
    while (current_body != NULL) {
        match = false;
        while (!match && current_body != NULL) {
            element = current_body->index(2);
            if (element->size() == nbarguments) {
                //if (lisp->current_instance)
                //    lisp->current_instance->store_variables(lisp->topstack());

                match = true;
                copying = null_;
                for (i = 0; i < nbarguments && match; i++) {
                    ele = element->index(i);
                    if (ele->isUnifiable()) {
                        copying = arguments->liste[i]->fullcopy();
                        match = ele->unify(lisp, copying, true);
                    }
                    else {
                        copying = null_;
                        match = ele->unify(lisp,arguments->liste[i], true);
                    }
                }
                
                if (match) {
                    lisp->setstackfunction(current_body, space);
                    break;
                }
                copying->release();
                lisp->clear_top_stack();
            }
            current_body = NULL;
            if (ilabel < sz)
                current_body = subfunction->second[ilabel++];
            else {
                if (sublabel != v_null) {
                    sublabel = v_null;
                    ilabel = 1;
                    //We check, if we have a rollback function
                    subfunction = functions.find(sublabel);
                    if (subfunction != functions.end()) {
                        current_body = subfunction->second[0];
                        sz = subfunction->second.size();
                    }
                }
            }
        }
        if (!match) {
            lisp->pop();
            arguments->release();
            lisp->resetStack();
            lisp->check_end_trace(tr, localtrace);
            return final_result;
        }
        
        bool success = true;
        element = true_;
        try {
            long nbinstructions = current_body->size();
            if (nbinstructions == 4) {
                element = current_body->index(3)->eval(lisp);
                success = element->Boolean();
            }
            else {
                element = true_;
                success = true;
                for (i = 3; i < nbinstructions && element->type != l_return && success; i++) {
                    element->release();
                    element = current_body->index(i)->eval(lisp);
                    if (element->label() == v_cut)
                        end_of_execution = true;
                    else
                        success = element->Boolean();
                }
            }
            if (element->type == l_return) {
                current_body = element->eval(lisp);
                element->release();
                element = current_body;
                break;
            }
        }
        catch (Error* err) {
#ifndef LISPE_WASM_NO_EXCEPTION
        err->release();
#endif
            success = false;
        }
        if (success)
            final_result->append(element);
        else
            element->release();
        if (end_of_execution)
            break;
        current_body = NULL;
        if (ilabel < sz) {
            lisp->clear_top_stack();
            current_body = subfunction->second[ilabel++];
        }
        else {
            if (sublabel != v_null) {
                sublabel = v_null;
                ilabel = 1;
                //We check, if we have a rollback function
                subfunction = functions.find(sublabel);
                if (subfunction != functions.end()) {
                    lisp->clear_top_stack();
                    current_body = subfunction->second[0];
                    sz = subfunction->second.size();
                }
            }
        }
    }
    arguments->release();
    lisp->pop(null_);
    lisp->resetStack();
    lisp->check_end_trace(tr, localtrace);
    return final_result;
}

/*
 ------------------------------------------------------------------------
 Evaluation of defpat rule  in an eval
 ------------------------------------------------------------------------
*/

Element* List::eval_pattern(LispE* lisp, List* body) {
    List_pattern_eval lpe(this, body);
    return lpe.eval(lisp);
}

/*
 ------------------------------------------------------------------------
 Evaluation of defpred rule in an eval
 ------------------------------------------------------------------------
*/

Element* List::eval_predicate(LispE* lisp, List* body) {
    List_predicate_eval lpe(this, body);
    return lpe.eval(lisp);
}

/*
 ------------------------------------------------------------------------
 Evaluation of defprol rule in an eval
 ------------------------------------------------------------------------
*/

Element* List::eval_prolog(LispE* lisp, List* body) {
    List_prolog_eval lpe(this, body);
    return lpe.eval(lisp);
}

//------------------------------------------------------------------------------------------
#ifdef LISPE_WASM_NO_EXCEPTION
void List::evalthread(LispE* lisp, List* body) {
    Element* element = terminal_;
    
    long nbarguments = body->size();
    
    while (element == terminal_) {
        if (nbarguments == 4)
            element = body->liste[3]->eval(lisp);
        else {
            element = null_;
            for (long i = 3; i < nbarguments && element != terminal_ && element->type != l_return && !element->isError(); i++) {
                element->release();
                element = body->liste[i]->eval(lisp);
            }
        }
        if (element->isError()) {
            lisp->delegation->setError((Error*)element);
            return;
        }
        if (element->type == l_return) {
            element->eval(lisp)->release();
            element->release();
            //This version protects 'e' from being destroyed in the stack.
            return;
        }
    }
    element->release();
}
#else
void List::evalthread(LispE* lisp, List* body) {
    Element* element = terminal_;
    
    try {
        long nbarguments = body->size();
        
        while (element == terminal_) {
            if (nbarguments == 4)
                element = body->liste[3]->eval(lisp);
            else {
                element = null_;
                for (long i = 3; i < nbarguments && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = body->liste[i]->eval(lisp);
                }
            }
            if (element->type == l_return) {
                element->eval(lisp)->release();
                element->release();
                //This version protects 'e' from being destroyed in the stack.
                return;
            }
        }
    }
    catch (Error* err) {
        lisp->delegation->setError(err);
    }
    element->release();
}
#endif

void Thread_Function::initialize(LispE* thread_lisp) {
    current_thread->evalthread(thread_lisp, current_body);
    thread_ancestor->nbjoined--;
    thread_lisp->delegation->clean_threads();
}

void launchthread(ThreadElement* the) {
    LispE* thread_lisp = the->thread_lisp;
    thread_lisp->thread_body->initialize(thread_lisp);
    //We clean some trailing threads
    the->cleaning = true;
    delete thread_lisp;
}

//Execution of a function as well as the shift of parameters with arguments
Element* List::eval_thread(LispE* lisp, List* body) {
    //We are already running this thread, it is a recursive call
    //we treat as a regular function
    if (lisp->thread_body != NULL && lisp->thread_body->current_body == body)
        return eval_function(lisp, body);

    //We create a new lisp container for the thread
    LispE* thread_lisp = new LispE(lisp, this, body);
    Element* parameters;
    
    long nbarguments = liste.size()-1;
    //the third element is the argument list .
    //we need our body to be the same number
    parameters = body->liste[2];
    long defaultarguments = parameters->argumentsize(nbarguments);
    if (defaultarguments == -1) {
        wstring message = L"Error: Wrong number of arguments in call to: '(";
        message += body->liste[1]->asString(lisp);
        message += L" ";
        message += body->liste[2]->asString(lisp);
        message += L"...)'";
        throw new Error(message);
    }
    
    if (defaultarguments == parameters->size())
        sameSizeNoTerminalArguments_thread(lisp, thread_lisp, body, (List*)parameters);
    else
        differentSizeNoTerminalArguments_thread(lisp, thread_lisp, body, (List*)parameters, nbarguments, defaultarguments);
    
    lisp->check_space(-1);
    //The thread can be of course recursive, but we do not want this recursivity
    //to trigger a new thread at each call...
    //We only create a thread once... The next calls will be executed
    //as regular functions
    ThreadElement* the = new ThreadElement(thread_lisp);
    the->thid = new std::thread(launchthread, the);
    if (the->thid == NULL) {
        delete the;
        throw new Error("Error: Too many threads created. Cannot execute it...");
    }
    
    lisp->delegation->clean_threads(the);
    return True_;
}

//------------------------------------------------------------------------
//This function is only used to compare the number of
//parameters of a function and its arguments
long List::argumentsize(long sz) {
    long listsz = liste.size();
    if (listsz == sz) {
        sz = 0;
        while (sz < listsz && !liste[sz]->isList()) {
            sz++;
        }
        if (sz < listsz) {
            long isz = sz + 1;
            while (isz < listsz && liste[isz]->isList())
                isz++;
            if (isz != listsz)
                throw new Error("Error: default values should always be at the end of the parameter list");
        }
        return sz;
    }
    if (listsz < sz) {
        //If the last argument is a list
        Element* last = liste.back();
        if (listsz && last->isNotEmptyList() && last->index(0)->isEmpty())
            return listsz-1;
        return -1;
    }
    //In this case, the difference in size should only be made up of listings.
    for (long i = listsz -1; i >= sz; i--) {
        if (!liste[i]->isList())
            return -1;
    }
    return sz;
}

/*
 IMPORTANT: function calls accept argument instanciation by name: (call (? x 10) (? y 20)) for (defun call(x y) ..)
 When a (? var value) is executed, it returns the "into_stack_" value.
 */

void List::sameSizeNoTerminalArguments(LispE* lisp, Element* data, List* parameters) {
    Stackelement* s = lisp->providingStack(data);
    // For each of the parameters we record in the stack
    int16_t label;
    //if (lisp->current_instance != NULL)
        //lisp->current_instance->store_variables(s);
        
    long sz = parameters->size();
    try {
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        bool into_stack = false;
        for (long i = 0; i < sz; i++) {
            label = parameters->liste[i]->label();
            //The evaluation must be done on the previous stage of the stack
            data = liste[i+1]->eval(lisp)->duplicate_constant(lisp);
            if (data == into_stack_) {
                into_stack = true;
                continue;
            }
            s->record_argument(data, label);
        }
        
        if (into_stack) {
            for (long i = 0; i < sz; i++) {
                if (!s->check_with_instance(parameters->liste[i]->label()))
                    throw new Error("Error: missing arguments");
            }
        }
    }
    catch (Error* err) {
        lisp->pop(s);
        throw err;
    }
    lisp->pushing(s);
}


Element* Element::duplicate_for_thread() {
    Element* e = duplicate();
    return (e == this && e->status && e->not_protected())?new Error("Error: Cannot use this value in a thread"):e;
}

void List::sameSizeNoTerminalArguments_thread(LispE* lisp, LispE* thread_lisp, Element* data, List* parameters) {
    //We then push a new stack element...
    //We cannot push it before, or the system will not be able to resolve
    //the argument variables...
    //Note that if it is a new thread creation, the body is pushed onto the stack
    //of this new thread environment...
    thread_lisp->push(data);
    Stackelement* current = lisp->top_new_stack;
    lisp->top_new_stack = thread_lisp->top_new_stack;
    bool previous_context = lisp->create_no_pool_element;
    lisp->create_no_pool_element = true;
    long sz = parameters->liste.size();
    // For each of the parameters we record in the stack
    bool into_stack = false;
    try {
        for (long i = 0; i < sz; i++) {
            //if we are dealing with a new thread, variables will be stored onto
            //the stack of this new thread environment
            //containers should be duplicated...
            data = liste[i+1]->eval(lisp)->duplicate_for_thread();
            if (data == into_stack_) {
                into_stack = true;
                continue;
            }
            thread_lisp->record_argument(data, parameters->liste[i]->label());
        }
        if (into_stack) {
            for (long i = 0; i < sz; i++) {
                if (!thread_lisp->checkvariable(parameters->liste[i]->label()))
                    throw new Error("Error: missing arguments");
            }
        }
    }
    catch (Error* err) {
        lisp->top_new_stack = current;
        lisp->create_no_pool_element = previous_context;
        thread_lisp->pop();
        throw err;
    }
    lisp->top_new_stack = current;
    lisp->create_no_pool_element = previous_context;
}

void List::sameSizeTerminalArguments(LispE* lisp, List* parameters) {
    Element* data;
    long sz = parameters->size();
    lisp->terminal_stack_variables = true;
    try {
        // For each of the parameters we record in the stack
        for (long i = 0; i < sz; i++) {
            data = liste[i+1]->eval(lisp);
            //if we are dealing with a new thread, variables will be stored onto
            //the stack of this new thread environment
            if (data == into_stack_) {
                continue;
            }
            lisp->replacestackvalue(data, parameters->liste[i]->label());
        }
    }
    catch (Error* err) {
        lisp->terminal_stack_variables = false;
        throw err;
    }
    lisp->terminal_stack_variables = false;
}

/*
 IMPORTANT: When an into_stack_ has been returned, then raw values are no longer accepted...
 
 (defun call(x y (z 1) (w 10)) ..)
 
 (call 10 20) ; is valid
 (call 10 20 (? w 2)) ; is valid, (? w 2) returns into_stack_
 (call 10 20 (? w 2) 12) ; is illegal
 */
void List::differentSizeNoTerminalArguments(LispE* lisp, Element* data, List* parameters,long nbarguments, long defaultarguments) {
    Stackelement* s = lisp->providingStack(data);
    long sz = parameters->liste.size();
    long i;
    List* l = NULL;
    
    //if (lisp->current_instance != NULL)
        //lisp->current_instance->store_variables(s);

    try {
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        // For each of the parameters we record in the stack
        int16_t label;
        Element* element;
        vecte<int16_t> vars;
        bool into_stack = false;
        for (i = 0; i < sz; i++) {
            data = (i < nbarguments) ? liste[i+1]->eval(lisp) : NULL;
            if (data == into_stack_) {
                vars.push_back(liste[i+1]->index(1)->label());
                into_stack = true;
                continue;
            }
                        
            if (into_stack) {
                if (data != NULL) {
                    data->release();
                    throw new Error("Error: illegal argument.");
                }
                continue;
            }
            
            element = parameters->liste[i];

            switch (element->size()) {
                case 0:
                    label = element->label();
                    if (data == NULL)
                        throw new Error(L"Error: Wrong parameter description");
                    break;
                case 1:
                    label = element->index(0)->label();
                    if (data == NULL)
                        data = null_;
                    break;
                default:
                    if (element->index(0) != emptylist_) {
                        label = element->index(0)->label();
                        if (data == NULL)
                            data = element->index(1)->eval(lisp);
                        data = data->duplicate_constant(lisp);
                        break;
                    }
                    label = element->index(1)->label();
                    //We create a list:
                    l = lisp->provideList();
                    //We store the rest of the arguments in it...
                    if (data != NULL) {
                        l->append(data);
                        i++;
                        while (i < nbarguments) {
                            l->append(liste[i+1]->eval(lisp));
                            i++;
                        }
                    }
                    data = l;
                    i = parameters->liste.size();
            }

            if (label <= l_final) {
                data->release();
                throw new Error(L"Error: Wrong parameter description");
            }


            vars.push_back(label);
            //if we are dealing with a new thread, variables will be stored onto
            //the stack of this new thread environment
            s->record_argument(data, label);
        }
        
        if (into_stack) {
            for (i = 0; vars.last < sz; i++) {
                element = parameters->liste[i];
                data = null_;
                switch (element->size()) {
                    case 0:
                        label = element->label();
                        if (!vars.check(label))
                            throw new Error("Error: missing argument");
                        continue;
                    case 1:
                        label = element->index(0)->label();
                        if (vars.check(label))
                            continue;
                        break;
                    default:
                        if (element->index(0) != emptylist_) {
                            label = element->index(0)->label();
                            if (vars.check(label))
                                continue;
                            data = element->index(1)->eval(lisp);
                            break;
                        }
                        throw new Error("Error: Illegal argument");
                }

                if (label <= l_final) {
                    data->release();
                    throw new Error(L"Error: Wrong parameter description");
                }

                //if we are dealing with a new thread, variables will be stored onto
                //the stack of this new thread environment
                vars.push_back(label);
                s->record_argument(data, label);
            }
        }
    }
    catch (Error* err) {
        lisp->pop(s);
        if (l != NULL)
            l->release();
        throw err;
    }
    lisp->pushing(s);
}

void List::differentSizeNoTerminalArguments_thread(LispE* lisp, LispE* thread_lisp, Element* data, List* parameters,long nbarguments, long defaultarguments) {
    
    //We create a new stage in the local stack for the new thread
    thread_lisp->push(data);
    
    Stackelement* current = lisp->top_new_stack;
    lisp->top_new_stack = thread_lisp->top_new_stack;

    bool previous_context = lisp->create_no_pool_element;
    lisp->create_no_pool_element = true;

    long i;
    List* l = NULL;
    long sz = parameters->liste.size();
    
    try {
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        // For each of the parameters we record in the stack
        int16_t label;
        Element* element;
        vecte<int16_t> vars;
        bool into_stack = false;
        for (i = 0; i < sz; i++) {
            if (i < nbarguments) {
                data = liste[i+1]->eval(lisp);
                if (data == into_stack_) {
                    vars.push_back(liste[i+1]->index(1)->label());
                    into_stack = true;
                    continue;
                }
            }
            else
                data = NULL;
            
            if (into_stack) {
                if (data != NULL) {
                    data->release();
                    throw new Error("Error: illegal argument.");
                }
                continue;
            }

            element = parameters->liste[i];
            
            switch (element->size()) {
                case 0:
                    label = element->label();
                    if (data == NULL)
                        throw new Error(L"Error: Wrong parameter description");
                    data = data->duplicate_for_thread();
                    break;
                case 1:
                    label = element->index(0)->label();
                    if (data == NULL)
                        data = null_;
                    break;
                default:
                    if (element->index(0) != emptylist_) {
                        label = element->index(0)->label();
                        if (data == NULL)
                            data = element->index(1)->eval(lisp);
                        data = data->duplicate_for_thread();
                        break;
                    }
                    label = element->index(1)->label();
                    //We create a list:
                    l = lisp->provideList();
                    //We store the rest of the arguments in it...
                    if (data != NULL) {
                        l->append(data->duplicate_for_thread());
                        i++;
                        while (i < nbarguments) {
                            l->append(liste[i+1]->eval(lisp));
                            i++;
                        }
                    }
                    data = l;
                    i = parameters->liste.size();
            }

            if (label <= l_final) {
                data->release();
                throw new Error(L"Error: Wrong parameter description");
            }

            vars.push_back(label);
            //if we are dealing with a new thread, variables will be stored onto
            //the stack of this new thread environment
            thread_lisp->record_argument(data, label);
        }
        
        if (into_stack) {
            for (i = 0; vars.last < sz; i++) {
                element = parameters->liste[i];
                data = null_;

                switch (element->size()) {
                    case 0:
                        label = element->label();
                        if (!vars.check(label))
                            throw new Error("Error: missing argument");
                        continue;
                    case 1:
                        label = element->index(0)->label();
                        if (vars.check(label))
                            continue;
                        break;
                    default:
                        if (element->index(0) != emptylist_) {
                            label = element->index(0)->label();
                            if (vars.check(label))
                                continue;
                            data = element->index(1)->eval(lisp);
                            break;
                        }
                        throw new Error("Error: Illegal argument");
                }

                if (label <= l_final) {
                    throw new Error(L"Error: Wrong parameter description");
                }

                //if we are dealing with a new thread, variables will be stored onto
                //the stack of this new thread environment
                vars.push_back(label);
                thread_lisp->record_argument(data, label);
            }
        }
    }
    catch (Error* err) {
        lisp->top_new_stack = current;
        lisp->create_no_pool_element = previous_context;
        if (l != NULL)
            l->release();
        thread_lisp->pop();
        throw err;
    }
    lisp->top_new_stack = current;
    lisp->create_no_pool_element = previous_context;
}

//In this case, we record in the current stack
void List::differentSizeTerminalArguments(LispE* lisp, List* parameters, long nbarguments, long defaultarguments) {
    List* l = NULL;
    int16_t label;
    Element* element;
    Element* data;
    long sz = parameters->liste.size();
    lisp->terminal_stack_variables = true;
    
    try {
        vecte<int16_t> vars;
        bool into_stack = false;
        long i;
        for (i = 0; i < sz; i++) {
            data = (i < nbarguments) ? liste[i+1]->eval(lisp) : NULL;
            if (data == into_stack_) {
                vars.push_back(liste[i+1]->index(1)->label());
                into_stack = true;
                continue;
            }

            if (into_stack) {
                if (data != NULL) {
                    data->release();
                    throw new Error("Error: illegal argument.");
                }
                continue;
            }

            element = parameters->liste[i];
            
            //This is the zone when arguments can be implicit
            switch (element->size()) {
                case 0:
                    label = element->label();
                    if (data == NULL)
                        throw new Error(L"Error: Wrong parameter description");
                    break;
                case 1:
                    label = element->index(0)->label();
                    if (data == NULL)
                        data = null_;
                    break;
                default:
                    if (element->index(0) != emptylist_) {
                        label = element->index(0)->label();
                        if (data == NULL)
                            data = element->index(1)->eval(lisp);
                        break;
                    }
                    label = element->index(1)->label();
                    //We create a list:
                    l = lisp->provideList();
                    //We store the rest of the arguments in it...
                    if (data != NULL) {
                        l->append(data);
                        i++;
                        while (i < nbarguments) {
                            l->append(liste[i+1]->eval(lisp));
                            i++;
                        }
                    }
                    data = l;
                    i = parameters->liste.size();
            }

            if (label <= l_final) {
                data->release();
                throw new Error(L"Error: Wrong parameter description");
            }

            vars.push_back(label);
            lisp->replacestackvalue(data, label);
        }
        
        if (into_stack) {
            for (i = 0; vars.last < sz; i++) {
                element = parameters->liste[i];
                data = null_;
                switch (element->size()) {
                    case 0:
                        label = element->label();
                        if (!vars.check(label))
                            throw new Error("Error: missing argument");
                        continue;
                    case 1:
                        label = element->index(0)->label();
                        if (vars.check(label))
                            continue;
                        break;
                    default:
                        if (element->index(0) != emptylist_) {
                            label = element->index(0)->label();
                            if (vars.check(label))
                                continue;
                            data = element->index(1)->eval(lisp);
                            break;
                        }
                        throw new Error("Error: Illegal argument");
                }

                if (label <= l_final) {
                    throw new Error(L"Error: Wrong parameter description");
                }

                //if we are dealing with a new thread, variables will be stored onto
                //the stack of this new thread environment
                vars.push_back(label);
                lisp->replacestackvalue(data, label);
            }
        }
    }
    catch (Error* err) {
        if (l != NULL)
            l->release();
        lisp->terminal_stack_variables = false;
        throw err;
    }
    lisp->terminal_stack_variables = false;
}

Element* List::eval_function(LispE* lisp, List* body) {
    // It is either a lambda or a function
    //otherwise it's an error
    Element* parameters;
    Element* element;
    
    long nbarguments = liste.size()-1;
    //the third element is the argument list .
    //we need our body to be the same number
    parameters = body->liste[2];
    long defaultarguments = parameters->argumentsize(nbarguments);
    if (defaultarguments == -1) {
        wstring message = L"Error: Wrong number of arguments in call to: '(";
        message += body->liste[1]->asString(lisp);
        message += L" ";
        message += body->liste[2]->asString(lisp);
        message += L"...)'";
        throw new Error(message);
    }
    
    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    
    //((lambda (x y) (if (eq x 0) (* 2 y) (self (- x 1) (+ y 2)))) 20 1)
    //( (lambda (x) (if (eq x 1) 1 (+ x (self (- x 1))))) 20)
    
    //terminal helps detects terminal recursion...
    //A terminal recursion can be processed in a quasi iterative way.
    //When a if or cond is processed, their last element is automatically set to terminal
    //When it corresponds to a function call, then it is processed here
    //We do not create any new stack element and we store our arguments back into the stack
    //with their new values in case of terminal recursion.
    if (terminal && lisp->called() == body) {
        if (defaultarguments == parameters->size())
            sameSizeTerminalArguments(lisp, (List*)parameters);
        else
            differentSizeTerminalArguments(lisp, (List*)parameters, nbarguments, defaultarguments);
        return terminal_;
    }
    
    if (defaultarguments == parameters->size())
        sameSizeNoTerminalArguments(lisp, body, (List*)parameters);
    else
        differentSizeNoTerminalArguments(lisp, body, (List*)parameters, nbarguments, defaultarguments);
        
    lisp->check_space(-1);
    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.

    long i;
    nbarguments = body->size();
    try {
        lisp->setStack();
        do {
            if (nbarguments == 4)
                element = body->liste[3]->eval(lisp);
            else {
                element = null_;
                for (i = 3; i < nbarguments && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = body->liste[i]->eval(lisp);
                }
            }
            if (element->type == l_return) {
                parameters = element->eval(lisp);
                element->release();
                //This version protects 'e' from being destroyed in the stack.
                lisp->resetStack();
                return lisp->pop(parameters);
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        lisp->resetStack();
        lisp->pop();
        throw err;
    }
    
    //This version protects 'e' from being destroyed in the stack.
    lisp->resetStack();
    return lisp->pop(element);
}

Element* List::eval_library_pattern_function(LispE* lisp, List* body) {
    List_library_pattern_eval lpe(this, body);
    return lpe.eval(lisp);
}

Element* List::eval_library_function(LispE* lisp, List* body) {
    // It is either a lambda or a function
    //otherwise it's an error
    Element* parameters;
    Element* element;
    
    long nbarguments = liste.size() - 1;
    //the third element is the argument list .
    //we need our body to be the same number
    parameters = body->liste[2];
    long defaultarguments = parameters->argumentsize(nbarguments);
        
    if (defaultarguments == parameters->size())
        sameSizeNoTerminalArguments(lisp, body, (List*)parameters);
    else
        differentSizeNoTerminalArguments(lisp, body, (List*)parameters, nbarguments, defaultarguments);
        
    lisp->check_space(-1);
    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.
    try {
        lisp->setStack();
        element = body->liste[3]->eval(lisp);
    }
    catch (Error* err) {
        lisp->resetStack();
        lisp->pop();
        throw err;
    }
    
    lisp->resetStack();
    //This version protects 'e' from being destroyed in the stack.
    return lisp->pop(element);
}
    

//Execution of a function as well as the shift of parameters with arguments
//For this specific lambda, we do not create a stack
Element* List::eval_lambda_min(LispE* lisp) {
    Element* stackfunction = lisp->exchangestackfunction(this);
    Element* element;
    int16_t nb_instructions = size();
    
    try {
        if (nb_instructions == 3)
            element = liste[2]->eval(lisp);
        else {
            element = null_;
            for (int16_t i = 2; i < nb_instructions && element->type != l_return; i++) {
                element->release();
                element = liste[i]->eval(lisp);
            }
        }
        if (element->type == l_return) {
            Element* e = element->eval(lisp);
            element->release();
            lisp->setstackfunction(stackfunction);
            //This version protects 'e' from being destroyed in the stack.
            return e;
        }
    }
    catch (Error* err) {
        lisp->setstackfunction(stackfunction);
        throw err;
    }
    
    lisp->setstackfunction(stackfunction);
    //This version protects 'e' from being destroyed in the stack.
    return element;
}

//Execution of a function as well as the shift of parameters with arguments
Element* List::eval_lambda(LispE* lisp, List* body) {
    // It is either a lambda or a function
    //otherwise it's an error
    List* parameters;
    Element* element;
    
    long nbarguments = liste.size() - 1;
    
    parameters = (List*)body->liste[1];
    if (nbarguments != parameters->size()) {
        wstring message = L"Error: Wrong number of arguments in: '(lambda ";
        message += parameters->asString(lisp);
        message += L"...)'";
        throw new Error(message);
    }
        
    

    //We calculate our values in advance, in the case of a recursive call, we must
    //use current values on the stack
    
    //((lambda (x y) (if (eq x 0) (* 2 y) (self (- x 1) (+ y 2)))) 20 1)
    //( (lambda (x) (if (eq x 1) 1 (+ x (self (- x 1))))) 20)
    
    //terminal helps detects terminal recursion...
    //A terminal recursion can be processed in a quasi iterative way.
    //When a if or cond is processed, their last element is automatically set to terminal
    //When it corresponds to a function call, then it is processed here
    //We do not create any new stack element and we store our arguments back into the stack
    //with their new values in case of terminal recursion.
    if (terminal && lisp->called() == body) {
        sameSizeTerminalArguments(lisp, parameters);
        return terminal_;
    }
    
    // For each of the parameters we record in the stack
    int16_t label;
    long i;
    vecte<Element*> recorded(nbarguments);
    vecte<int16_t> labels(nbarguments);
    
    try {
        lisp->setStack();
        //We then push a new stack element...
        //We cannot push it before, or the system will not be able to resolve
        //the argument variables...
        //Note that if it is a new thread creation, the body is pushed onto the stack
        //of this new thread environment...
        for (i = 0; i < nbarguments; i++) {
            label = parameters->liste[i]->label();
            //The evaluation must be done on the previous stage of the stack
            element = liste[i+1]->eval(lisp);
            recorded.push_raw(lisp->record_or_replace(element, label));
            labels.push_raw(label);
        }
    }
    catch (Error* err) {
        lisp->resetStack();
        //We must call reset_in_stack from end to begin
        //the status should popped out from Stack::status
        for (i = labels.size() - 1; i >= 0; i--)
            lisp->reset_in_stack(recorded[i], labels[i]);
        throw err;
    }

    //This is a very specific case, we do not want to go into recursion
    //but handle the call properly as an iteration
    //We do not try to execute the code again, we simply returns back to the original loop.

    nbarguments = body->size();
    Element* stackfunction = lisp->exchangestackfunction(body);
    try {
        do {
            if (nbarguments == 3)
                element = body->liste[2]->eval(lisp);
            else {
                element = null_;
                for (i = 2; i < nbarguments && element != terminal_ && element->type != l_return; i++) {
                    element->release();
                    element = body->liste[i]->eval(lisp);
                }
            }
            if (element->type == l_return) {
                Element* e = element->eval(lisp);
                element->release();
                //We must call reset_in_stack from end to begin
                //the status should popped out from Stack::status
                for (i = labels.size() - 1; i >= 0; i--)
                    lisp->reset_in_stack(recorded[i], labels[i], e);
                lisp->resetStack();
                return e;
            }
        }
        while (element == terminal_);
    }
    catch (Error* err) {
        lisp->resetStack();
        lisp->setstackfunction(stackfunction);
        for (i = labels.size() - 1; i >= 0; i--)
            lisp->reset_in_stack(recorded[i], labels[i]);
        throw err;
    }
    
    for (i = labels.size() - 1; i >= 0; i--)
        lisp->reset_in_stack(recorded[i], labels[i], element);
    lisp->setstackfunction(stackfunction);
    lisp->resetStack();
    return element;
}

//Execution of a function as well as the shift of parameters with arguments
Element* List::eval_data(LispE* lisp, Element* data) {
    List* values = lisp->provideList();
    values->append(data->index(0));
    Element* element;
    long nbarguments = liste.size()-1;
    try {
        for (long i = 1; i <= nbarguments; i++) {
            element = liste[i]->eval(lisp);
            values->append(element->duplicate_constant(lisp));
        }
    }
    catch (Error* err) {
        values->clear();
        delete values;
        throw err;
    }
    
    char res = data->check_match(lisp,values);
    if (res != check_ok) {
        values->clear();
        delete values;
        if (res == check_mismatch)
            throw new Error(L"Error: Size mismatch between argument list and data structure definition");
        else {
            std::wstringstream message;
            message << L"Error: Mismatch on argument: " << (int)res;
            message << " (" << lisp->asString(data->index((int)res)->label()) << " required)";
            throw new Error(message.str());
        }
    }
    values->type = t_data;
    return values;
}

//Execution of a function as well as the shift of parameters with arguments
Element* List::evalfunction(LispE* lisp, Element* body) {
    int16_t label = body->function_label(lisp);
    switch(label) {
        case l_defpat:
            return eval_pattern(lisp, (List*)body);
        case l_defpred:
            return eval_predicate(lisp, (List*)body);
        case l_defprol:
            return eval_prolog(lisp, (List*)body);
        case l_dethread:
            return eval_thread(lisp, (List*)body);
        case l_deflib:
            return eval_library_function(lisp, (List*)body);
        case l_deflibpat:
            return eval_library_pattern_function(lisp, (List*)body);
        case l_defun:
            return eval_function(lisp, (List*)body);
        case l_lambda:
            return eval_lambda(lisp, (List*)body);
        case l_class:
            return create_class_instance(lisp, (List_class_definition*)body);
        case l_data:
            return eval_data(lisp, lisp->getDataStructure(label));
        case t_class_instance:
            return execute_instance_function(lisp, (List_instance*)body);
        default:
            throw new Error("Unknown function call");
    }
}

Element* List::eval_call_function(LispE* lisp) {
    if (lisp->trace) {
        Element* body = liste[0]->eval(lisp);

        //We also retrieve its label (which is l_defun or l_defpat or...)
        int16_t label = body->index(0)->label();
        char tr = debug_next;
        if (label == l_defun || label == l_defpat || label == l_lambda || label == l_defpred || label == l_defprol) {
            if (lisp->trace == debug_inside_function)
                lisp->stop_at_next_line(debug_next);
            else {
                if (lisp->trace == debug_next) {
                    lisp->trace = debug_none;
                }
            }
        }
        
        body = evalfunction(lisp, body);
        
        if (lisp->trace != debug_goto)
            lisp->stop_at_next_line(tr);
        return body;
    }
    return evalfunction(lisp, liste[0]->eval(lisp));
}


//eval_call_function is usually called once, except in the case of a trace
//In other cases, we promote the call to a specific: t_pattern, t_self or t_function call
//which do not check anything else anymore...
Element* Listincode::eval_call_function(LispE* lisp) {
    Element* body = liste[0]->eval(lisp);

    if (lisp->delegation->trace_on) {
        if (lisp->trace) {
            //We also retrieve its label (which is l_defun or l_defpat or...)
            int16_t label = body->index(0)->label();
            char tr = debug_next;
            if (label == l_defun || label == l_defpat || label == l_lambda || label == l_defpred || label == l_defprol) {
                if (lisp->trace == debug_inside_function)
                    lisp->stop_at_next_line(debug_next);
                else {
                    if (lisp->trace == debug_next) {
                        lisp->trace = debug_none;
                    }
                }
            }
            
            body = evalfunction(lisp, body);
            
            if (lisp->trace != debug_goto)
                lisp->stop_at_next_line(tr);
            return body;
        }
        else
            return evalfunction(lisp, body);
    }

    if (lisp->threaded())
        return evalfunction(lisp, body);
    
    int16_t label = body->function_label(lisp);
    switch(label) {
        case l_defpred:
            liste[0] = new List_predicate_eval(this, (List*)body);
            lisp->storeforgarbage(liste[0]);
            return liste[0]->eval(lisp);
        case l_defprol:
            liste[0] = new List_prolog_eval(this, (List*)body);
            lisp->storeforgarbage(liste[0]);
            return liste[0]->eval(lisp);
        case l_defpat:
            liste[0] = new List_pattern_eval(this, (List*)body);
            lisp->storeforgarbage(liste[0]);
            return liste[0]->eval(lisp);
        case l_defun:
            liste[0] = new List_function_eval(lisp, this, (List*)body, lisp->current_space);
            lisp->storeforgarbage(liste[0]);
            return liste[0]->eval(lisp);
        case l_class:
            return create_class_instance(lisp, (List_class_definition*)body);
        case l_deflib:
            liste[0] = new List_library_eval(this, (List*)body);
            lisp->storeforgarbage(liste[0]);
            return liste[0]->eval(lisp);
        case l_dethread:
            return eval_thread(lisp, (List*)body);
        case l_lambda:
            liste[0] = new Atomefonction(body, t_lambda);
            lisp->storeforgarbage(liste[0]);
            return eval_lambda(lisp, (List*)body);
        case l_data: {
            List_data_eval lst(this);
            return lst.eval(lisp);
        }
        case t_class_instance:
            return execute_instance_function(lisp, (List_instance*)body);
        default: {
            throw new Error("Unknown function call");
        }
    }
}

Element* Listincode::eval_call_self(LispE* lisp) {
    if (lisp->delegation->trace_on) {
        Element* body = lisp->called();
        if (lisp->trace) {
            //We also retrieve its label (which is l_defun or l_defpat or...)
            int16_t label = body->index(0)->label();
            char tr = debug_next;
            if (label == l_defun || label == l_defpat || label == l_lambda || label == l_defpred || label == l_defprol) {
                if (lisp->trace == debug_inside_function)
                    lisp->stop_at_next_line(debug_next);
                else {
                    if (lisp->trace == debug_next) {
                        lisp->trace = debug_none;
                    }
                }
            }
            
            body = evalfunction(lisp, body);
            
            if (lisp->trace != debug_goto)
                lisp->stop_at_next_line(tr);
            return body;
        }
        else
            return evalfunction(lisp, body);
    }

    return evalfunction(lisp, lisp->called());
}


Element* List::eval_call_self(LispE* lisp) {
    return evalfunction(lisp,lisp->called());
}

Element* List::evalt_list(LispE* lisp) {
    Element* first_element = liste[0];
        

    Element* second_element = null_;

    try {
        if (first_element->isInstruction())
            return eval(lisp);
        
        first_element = first_element->eval(lisp);
        if (!first_element->size()) {
            if (lisp->checkDataStructure(first_element->label())) {
                List_data_eval lst(this);
                return lst.eval(lisp);
            }
            return first_element;
        }
        
        //Execution of a lambda a priori, otherwise it's an error
        second_element = evalfunction(lisp, first_element);
        first_element->release();
    }
    catch (Error* err) {
        first_element->release();
        second_element->release();
        throw err;
    }
    return second_element;
}


//--------------------------------------------------------------------------------
// The main evaluation function, the one that evaluates instructions or functions
//--------------------------------------------------------------------------------
bool List::eval_Boolean(LispE* lisp, int16_t instruction) {
    return (this->*lisp->delegation->evals[instruction])(lisp)->Boolean();
}

bool List_eval::eval_Boolean(LispE* lisp, int16_t instruction) {
    Element* e = eval(lisp);
    bool b = e->Boolean();
    e->release();
    return b;
}

bool List_function_eval::eval_Boolean(LispE* lisp, int16_t instruction) {
    Element* e = eval(lisp);
    bool b = e->Boolean();
    e->release();
    return b;
}

bool List_call_lambda::eval_Boolean(LispE* lisp, int16_t instruction) {
    Element* e = eval(lisp);
    bool b = e->Boolean();
    e->release();
    return b;
}

bool List_predicate_eval::eval_Boolean(LispE* lisp, int16_t instruction) {
    Element* e = eval(lisp);
    bool b = e->Boolean();
    e->release();
    return b;
}

bool List_prolog_eval::eval_Boolean(LispE* lisp, int16_t instruction) {
    Element* e = eval(lisp);
    bool b = e->Boolean();
    e->release();
    return b;
}

bool List_pattern_eval::eval_Boolean(LispE* lisp, int16_t instruction) {
    Element* e = eval(lisp);
    bool b = e->Boolean();
    e->release();
    return b;
}

bool List_library_eval::eval_Boolean(LispE* lisp, int16_t instruction) {
    Element* e = eval(lisp);
    bool b = e->Boolean();
    e->release();
    return b;
}

bool List_library_pattern_eval::eval_Boolean(LispE* lisp, int16_t instruction) {
    Element* e = eval(lisp);
    bool b = e->Boolean();
    e->release();
    return b;
}

Element* List::evall_void(LispE* lisp) {
    return null_;
}


Element* List::evall_emptylist(LispE* lisp) {
    return emptylist_;
}

Element* List::eval_no_fail(LispE* lisp) {
    try {
        Element* e = (this->*lisp->delegation->evals[lisp->checkState(this, liste.size())])(lisp);
        lisp->resetStack();
        return e;
    }
    catch (Error* err) {
        return err;
    }
}

Element* List::eval(LispE* lisp) {
    try {
        Element* e = (this->*lisp->delegation->evals[lisp->checkState(this, liste.size())])(lisp);
        lisp->resetStack();
        return e;
    }
    catch (Error* err) {
        return lisp->check_error(this, err, -1);
    }
}

Element* List_eval::eval(LispE* lisp) {
    return (this->*met)(lisp);
}

//This the function that is called when an eval is done on a list of elements where a function is present
//(eval '(cons 'a 'b)), we first create a List_cons_eval object that will "borrow" the current list content
//We then evaluate.
Element* List::eval_list_instruction(LispE* lisp) {
    List* l = lisp->borrowing((Listincode*)this, size());
    Element* e;
    try {
        e = l->eval(lisp);
    }
    catch(Error* err) {
        lisp->relax(l);
        return lisp->check_error(this, err, -1);
    }
    lisp->relax(l);
    return e;
}

Element* LispE::check_error(List* l, Error* err, int idxinfo) {
    delegation->set_error_context(err, idxinfo);
    if (err != delegation->_THEEND) {
        resetStack();
        throw err;
    }

    if (checkLispState()) {
        if (isthreadError()) {
            if (!isThread)
                delegation->throwError();
            return n_null;
        }
        if (hasStopped())
            throw delegation->_THEEND;
        return n_null;
    }
    return l->eval_error(this);
}

Element* Listincode::eval(LispE* lisp) {
    try {
        lisp->checkPureState(this);
        Element* e = (this->*lisp->delegation->evals[lisp->checkBasicState(this)])(lisp);
        lisp->resetStack();
        return e;
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
}

Element* List_execute::eval(LispE* lisp) {
    try {
        lisp->checkPureState(this);
        Element* e = (this->*method)(lisp);
        lisp->resetStack();
        return e;
    }
    catch (Error* err) {
        return lisp->check_error(this, err, idxinfo);
    }
}

//------------------------------------------------------------------------------------------
Element* List_scan_eval::eval(LispE* lisp) {
    //Operation is: (\\ operation l1 l2)
    
    Element* current_list = liste[2]->eval(lisp);
    

    if (!current_list->isList()) {
        current_list->release();
        return lisp->check_error(this, new Error("Error: argument for 'scan' should be a list"), idxinfo);
    }

    Element* op = null_;
    Element* res = null_;
    List* call = NULL;

    long sz = current_list->size();
    if (!sz)
        return emptylist_;

    bool sb = lisp->set_true_as_one();
    
    try {
        //If we have a slice increment
        if (size() == 4) {
            long increment;
            evalAsInteger(3, lisp, increment);
            if (increment <= 0)
                throw new Error("Error: wrong value for the increment");
            //We will slice our list in slices of size increment
            Element* slice;
            long nb_elements;
            call = new List_scan_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            Element* e;
            res = lisp->provideList();
            for (long i = 0; i < sz; i += increment) {
                //We need an empty tensor or an empty matrix, when dealing with tensors or matrices
                slice = current_list->pureInstance();
                for (nb_elements = 0; nb_elements < increment && (i+nb_elements) < sz; nb_elements++) {
                    slice->append(current_list->index(i + nb_elements));
                }
                slice->setShape();
                call->in_quote(2, slice);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
    
        //if l1 is a matrix, we recursively call the function on each sublist
        if (current_list->isTensor()) {
            call = new List_scan_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            vecte<long> shape;
            current_list->getShape(shape);
            Element* e;
            Element* lines;
            
            res = current_list->newTensor(true, lisp);
            for (long i = 0; i < shape[0]; i++) {
                lines = current_list->newTensor(false, lisp, (List*)current_list->index(i));
                call->in_quote(2, lines);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            res->setShape();
            
            current_list->release();
            call->force_release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        if (current_list->checkListofTensor()) {
            call = new List_backscan_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            call->in_quote(2, current_list);
            //current_list will be cleaned by call if necessary
            current_list = null_;
            res = call->eval(lisp);
            call->force_release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return res;
        }

        lisp->checkState(this);
        op = liste[1]->eval(lisp);

        if (op->isLambda() || op->isList()) {
            res = op->scan(lisp, current_list, sz);
            current_list->release();
            op->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        res = lisp->provideList();
        op = eval_body_as_argument_min(lisp, op, P_TWO|P_THREE);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);
        
        bool monadic = op->check_arity(lisp, P_TWO);
        
        Element* e;
        
        if (lisp->delegation->comparators.check(op->label0())) {
            call = lisp->provideCall(op, 2);
            for (long i = 0; i < sz - 1; i++) {
                call->in_quote(1, current_list->value_on_index(lisp, i));
                call->in_quote(2, current_list->index(i + 1));
                e = call->eval(lisp);
                res->append(e);
            }
        }
        else {
            call = lisp->provideCall(op, 1);
            
            e = current_list->value_on_index(lisp, (long)0);
            res->append(e);
            
            call->in_quote(1, e);
            
            if (!monadic) {
                call->append(lisp->quoted());
                for (long i = 1; i < sz; i++) {
                    call->in_quote(2, current_list->index(i));
                    e = call->eval(lisp);
                    res->append(e);
                    call->in_quote(1, e);
                }
            }
            else {
                for (long i = 1; i < sz; i++) {
                    call->in_quote(1, current_list->index(i));
                    e = call->eval(lisp);
                    res->append(e);
                }
            }
        }
        call->force_release();
    }
    catch (Error* err) {
        if (call != NULL)
            call->force_release();
        else
            op->release();
        lisp->reset_to_true(sb);
        res->release();
        current_list->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    current_list->release();
    lisp->reset_to_true(sb);
    lisp->resetStack();
    return res;
}
//------------------------------------------------------------------
Element* List::backscan(LispE* lisp, Element* current_list, long sz) {
    Element* res = current_list->newInstance();

    if (size() == 0)
        return res;
    
    //this is a filter, the first list
    long j = sz - 1;
    long nb;
    try {
        for (long i = size() - 1; i >= 0 ; i--) {
            nb = liste[i]->asInteger();
            if (!nb)
                res->append(zero_value);
            else {
                if (j <= -1)
                    throw new Error("Error: List size mismatch");

                while (nb > 0) {
                    res->append(current_list->index(j));
                    nb--;
                }
                j--;
            }
        }
        if (j <= -1)
            throw new Error("Error: List size mismatch");
    }
    catch (Error* err) {
        res->release();
        throw err;
    }
    return res;
}

Element* List_lambda_eval::backscan(LispE* lisp, Element* current_list, long sz) {
    if (labels.size() != 2)
        throw new Error("Error: Wrong number of arguments");
    
    List* res = lisp->provideList();

    Element* rarg1 = this;
    Element* rarg2 = this;
    
    int16_t arg1 = labels[0];
    int16_t arg2 = labels[1];

    try {
        sz--;
        Element* e = current_list->index(sz)->copying(false);

        //if there is already a variable with this name on the stack
        //we record it to restore it later...
        rarg1 = lisp->record_or_replace(e, arg1);
        rarg2 = lisp->record_or_replace(null_, arg2);

        res->append(e);
        
        for (long i = sz-1; i >= 0; i--) {
            lisp->replacestackvalue(current_list->index(i), arg2);
            e = eval_lambda_min(lisp);
            if (e->type == l_return)
                break;
            e = e->copying(false);
            res->append(e);
            lisp->replacestackvalue(e, arg1);
        }
        
        lisp->reset_in_stack(rarg2, arg2);
        lisp->reset_in_stack(rarg1, arg1);
    }
    catch (Error* err) {
        if (rarg2 != this)
            lisp->reset_in_stack(rarg2, arg2);
        if (rarg1 != this)
            lisp->reset_in_stack(rarg1, arg1);

        res->release();
        throw err;
    }
    return res;
}

Element* List_backscan_eval::eval(LispE* lisp) {
    //Operation is: (-\\ operation l1 l2)
    Element* current_list = liste[2]->eval(lisp);
    if (!current_list->isList()) {
        current_list->release();
        throw new Error("Error: argument for 'backscan' should be a list");
    }

    Element* op = null_;

    Element* res = null_;
    List* call = NULL;

    long sz = current_list->size();
    if (!sz)
        return null_;

    bool sb = lisp->set_true_as_one();
    try {
        //If we have a slice increment
        if (size() == 4) {
            long increment;
            evalAsInteger(3, lisp, increment);
            if (increment <= 0)
                throw new Error("Error: wrong value for the increment");
            //We will slice our list in slices of size increment
            Element* slice;
            long nb_elements;
            call = new List_backscan_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            Element* e;
            res = lisp->provideList();
            for (long i = sz - increment; i >= 0; i -= increment) {
                //We need an empty tensor or an empty matrix, when dealing with tensors or matrices
                slice = current_list->pureInstance();
                for (nb_elements = 0; nb_elements < increment && (i+nb_elements) < sz; nb_elements++) {
                    slice->append(current_list->index(i + nb_elements));
                }
                slice->setShape();
                call->in_quote(2, slice);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        //if l1 is a matrix, we recursively call the function on each sublist out of the transposed matrix
        if (current_list->isTensor() || current_list->checkListofTensor()) {
            call = new List_backscan_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            vecte<long> shape;
            current_list->getShape(shape);
            //We extract our columns
            Element* e;
            Element* columns = NULL;
            
            res = current_list->newTensor(true, lisp);
            for (long i = 0; i < shape[1]; i++) {
                columns = current_list->newTensor(true, lisp);
                for (long j = 0; j < shape[0]; j++) {
                    columns->append(current_list->index(j)->index(i));
                }
                columns->setShape();
                call->in_quote(2, columns);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            if (current_list->type == t_list) {
                columns = current_list->index(0)->newTensor(lisp, (List*)res);
                res->release();
                res = columns;
            }
            else
                res->setShape();
            
            call->force_release();
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return res;
        }
        
        lisp->checkState(this);
        
        op = liste[1]->eval(lisp);
        
        if (op->isLambda() || op->isList()) {
            res = op->backscan(lisp, current_list, sz);
            op->release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }

        res = lisp->provideList();
        
        op = eval_body_as_argument_min(lisp, op, P_TWO|P_THREE);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);
        
        Element* e;
        
        if (lisp->delegation->comparators.check(op->label0())) {
            call = lisp->provideCall(op, 2);
            for (long i = sz - 1; i > 0; i--) {
                call->in_quote(1, current_list->value_on_index(lisp, i));
                call->in_quote(2, current_list->index(i - 1));
                e = call->eval(lisp);
                res->append(e);
            }
        }
        else {
            bool monadic = op->check_arity(lisp, P_TWO);
            
            call = lisp->provideCall(op, 1);
            
            sz--;
            Element* e = current_list->value_on_index(lisp, sz);
            res->append(e);
            call->in_quote(1, e);
            
            if (!monadic) {
                call->append(lisp->quoted());
                for (long i = sz-1; i >= 0; i--) {
                    call->in_quote(2, current_list->index(i));
                    e = call->eval(lisp);
                    res->append(e);
                    call->in_quote(1, e);
                }
            }
            else {
                for (long i = sz-1; i >= 0; i--) {
                    call->in_quote(1, current_list->index(i));
                    e = call->eval(lisp);
                    res->append(e);
                }
            }
        }
        call->force_release();
        current_list->release();
        lisp->reset_to_true(sb);
    }
    catch (Error* err) {
        if (call != NULL)
            call->force_release();
        else
            op->release();
        res->release();
        lisp->reset_to_true(sb);
        current_list->release();
        return lisp->check_error(this, err, idxinfo);
    }
    lisp->resetStack();
    return res;
}

//------------------------------------------------------------------------------------
Element* List::reduce(LispE* lisp, Element* current_list, long sz) {
    if (sz == 1) {
        Element* op = current_list->value_on_index(lisp, (long)0);
        lisp->resetStack();
        return op;
    }
    
    Element* res = current_list->newInstance();

    long lsz = size();
    
    if (lsz == 0)
        return res;
    
    try {
        long j = 0;
        long nb;
        
        for (long i = 0; i < lsz; i++) {
            nb = liste[i]->asInteger();
            if (!nb) {
                j++;
                continue;
            }
            
            if (j >= sz)
                throw new Error("Error: List size mismatch");
            
            while (nb > 0) {
                res->append(current_list->index(j));
                nb--;
            }
            j++;
        }
    }
    catch (Error* err) {
        res->release();
        throw err;
    }

    return res;
}


Element* List_lambda_eval::reduce(LispE* lisp, Element* current_list, long sz) {
    if (labels.size() != 2)
        throw new Error("Error: Wrong number of arguments");
    
    int16_t arg1 = 0;
    int16_t arg2 = 0;
    Element* rarg1 = this;
    Element* rarg2 = this;
    try {
        arg1 = labels[0];
        arg2 = labels[1];
        
        Element* element = current_list->index(0)->copying(false);
        rarg1 = lisp->record_or_replace(element, arg1);
        rarg2 = lisp->record_or_replace(current_list->index(1), arg2);
        
        element = eval_lambda_min(lisp);
        if (element->type != l_return) {
            lisp->replacestackvalue(element, arg1);
            
            for (long i = 2; i < sz; i++) {
                lisp->replacestackvalue(current_list->index(i), arg2);
                element = eval_lambda_min(lisp);
                if (element->type == l_return)
                    break;
                lisp->replacestackvalue(element, arg1);
            }
        }
        element->increment();
        lisp->reset_in_stack(rarg2, arg2);
        lisp->reset_in_stack(rarg1, arg1);
        element->decrementkeep();
        return element;
    }
    catch (Error* err) {
        if (rarg2 != this)
            lisp->reset_in_stack(rarg2, arg2);
        if (rarg1 != this)
            lisp->reset_in_stack(rarg1, arg1);
        throw err;
    }
    
    return null_;
}

Element* List_reduce_eval::eval(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
        
    Element* current_list = null_;
    Element* op = null_;
    Element* res = null_;
    List* call = NULL;
    bool sb = false;
    long sz = 0;
    
    try {
        lisp->checkState(this);
        if (listsz == 2) {
            //This is a copy
            current_list = liste[1]->eval(lisp);
            op = current_list->fullcopy();
            if (op != current_list)
                current_list->release();
            lisp->resetStack();
            return op;
        }
        
        current_list = liste[2]->eval(lisp);
        
        if (!current_list->isList())
            throw new Error("Error: argument for 'reduce' should be a list");
        
        sz = current_list->size();
        if (!sz) {
            lisp->resetStack();
            return null_;
        }
        
        sb = lisp->set_true_as_one();
        
        //If we have a slice increment
        if (listsz == 4) {
            // (setq r (integers 1 3 2 7 8 9 11 2 4 5))

            long increment;
            evalAsInteger(3, lisp, increment);
            if (increment <= 0)
                throw new Error("Error: wrong value for the increment");
            //We will slice our list in slices of size increment
            Element* slice;
            long nb_elements;
            call = new List_reduce_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            Element* e;
            res = lisp->provideList();
            for (long i = 0; i < sz; i += increment) {
                //We need an empty tensor or an empty matrix, when dealing with tensors or matrices
                slice = current_list->pureInstance();
                for (nb_elements = 0; nb_elements < increment && (i+nb_elements) < sz; nb_elements++) {
                    slice->append(current_list->index(i + nb_elements));
                }
                slice->setShape();
                call->in_quote(2, slice);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        // (// '+ (rho 5 4 3 2 (iota 120)))
        //if l1 is a matrix, we recursively call the function on each sublist
        if (current_list->isTensor()) {
            call = new List_reduce_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            vecte<long> shape;
            current_list->getShape(shape);
            //We extract our columns
            Element* e;
            Element* lines;
            
            res = current_list->newTensor(true, lisp);
            
            for (long i = 0; i < shape[0]; i++) {
                lines = current_list->newTensor(false, lisp, (List*)current_list->index(i));
                call->in_quote(2, lines);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            res->setShape();
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        if (current_list->checkListofTensor()) {
            call = new List_backreduce_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            call->in_quote(2, current_list);
            //It will be cleaned with call
            current_list = null_;
            res = call->eval(lisp);
            call->force_release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }

        op = liste[1]->eval(lisp);

        if (op->isLambda() || op->isList()) {
            res = op->reduce(lisp, current_list, sz);
            current_list->release();
            op->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return res;
        }

        op = eval_body_as_argument_min(lisp, op, P_TWO|P_THREE);
        
        call = lisp->provideCall(op, 2);
        call->in_quote(1, current_list->value_on_index(lisp, (long)0));
        call->in_quote(2, current_list->index(1));
        
        Element* e = call->eval(lisp);
        
        if (lisp->delegation->comparators.check(op->label0())) {
            for (long i = 1; i < current_list->size() - 1 && e == one_value; i++) {
                call->in_quote(1, current_list->value_on_index(lisp, i));
                call->in_quote(2, current_list->index(i + 1));
                e = call->eval(lisp);
            }
            
            call->force_release();
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return e;
        }
        
        call->in_quote(1, e);
        for (long i = 2; i < current_list->size(); i++) {
            call->in_quote(2, current_list->index(i));
            e = call->eval(lisp);
            call->in_quote(1, e);
        }
        e->increment();
        call->force_release();
        e->decrementkeep();
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        return e->release_but_last();
    }
    catch (Error* err) {
        if (call != NULL)
            call->force_release();
        else
            op->release();
        res->release();
        current_list->release();
        lisp->reset_to_true(sb);
        return lisp->check_error(this, err, idxinfo);
    }
    return null_;
}
//------------------------------------------------------------------------------------
Element* List::backreduce(LispE* lisp, Element* current_list, long sz) {
    if (sz == 1) {
        Element* op = current_list->value_on_index(lisp, (long)0);
        lisp->resetStack();
        return op;
    }

    Element* res = current_list->newInstance();

    if (size() == 0)
        return res;

    try {
        //this is a filter, the first list
        long j = sz - 1;
        long nb;
        
        for (long i = size() - 1; i >= 0; i--) {
            nb = liste[i]->asInteger();
            if (!nb) {
                j--;
                continue;
            }
            if (j <= -1)
                throw new Error("Error: List size mismatch");
            while (nb > 0) {
                res->append(current_list->index(j));
                nb--;
            }
            j--;
        }
    }
    catch (Error* err) {
        res->release();
        throw err;
    }

    return res;
}

Element* List_lambda_eval::backreduce(LispE* lisp, Element* current_list, long sz) {
    if (labels.size() != 2)
        throw new Error("Error: Wrong number of arguments");
    
    int16_t arg1 = 0;
    int16_t arg2 = 0;
    Element* rarg1 = this;
    Element* rarg2 = this;

    current_list = current_list->reverse(lisp);

    try {
        
        arg1 = labels[0];
        arg2 = labels[1];
        
        Element* element = current_list->index(0)->copying(false);
        
        rarg1 = lisp->record_or_replace(element, arg1);
        rarg2 = lisp->record_or_replace(current_list->index(1), arg2);
        
        element = eval_lambda_min(lisp);
        if (element->type != l_return) {
            lisp->replacestackvalue(element, arg1);
            
            for (long i = 2; i < sz; i++) {
                lisp->replacestackvalue(current_list->index(i), arg2);
                element = eval_lambda_min(lisp);
                if (element->type == l_return)
                    break;
                lisp->replacestackvalue(element, arg1);
            }
        }
        
        element->increment();
        
        lisp->reset_in_stack(rarg2, arg2);
        lisp->reset_in_stack(rarg1, arg1);
        current_list->release();
        
        element->decrementkeep();
        return element;
    }
    catch (Error* err) {
        if (rarg2 != this)
            lisp->reset_in_stack(rarg2, arg2);
        if (rarg1 != this)
            lisp->reset_in_stack(rarg1, arg1);
        current_list->release();
        throw err;
    }
    return null_;
}

Element* List_backreduce_eval::eval(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (-// operation l1)
    
    Element* current_list = null_;
    Element* op = null_;
    Element* res = null_;
    List* call = NULL;
    long sz = 0;
    bool sb = false;
    
    try {
        lisp->checkState(this);
        if (listsz == 2) {
            //This is a copy
            current_list = liste[1]->eval(lisp);
            op = current_list->reverse(lisp);
            if (op != current_list)
                current_list->release();
            lisp->resetStack();
            return op;
        }
        
        current_list = liste[2]->eval(lisp);
        
        if (!current_list->isList())
            throw new Error("Error: argument for 'backreduce' should be a list");
        
        sz = current_list->size();
        if (!sz) {
            lisp->resetStack();
            return null_;
        }
        
        sb = lisp->set_true_as_one();
        
        //If we have a slice increment
        if (listsz == 4) {
            long increment;
            evalAsInteger(3, lisp, increment);
            if (increment <= 0)
                throw new Error("Error: wrong value for the increment");
            //We will slice our list in slices of size increment
            Element* slice;
            long nb_elements;
            call = new List_backreduce_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            Element* e;
            res = lisp->provideList();
            for (long i = sz - increment; i >= 0; i -= increment) {
                //We need an empty tensor or an empty matrix, when dealing with tensors or matrices
                slice = current_list->pureInstance();
                for (nb_elements = 0; nb_elements < increment && (i+nb_elements) < sz; nb_elements++) {
                    slice->append(current_list->index(i + nb_elements));
                }
                slice->setShape();
                call->in_quote(2, slice);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        if (current_list->isTensor() || current_list->checkListofTensor()) {
            call = new List_backreduce_eval();
            call->append(liste[0]);
            call->append(liste[1]);
            call->append(lisp->quoted());
            vecte<long> shape;
            current_list->getShape(shape);
            //We extract our columns
            Element* e;
            Element* columns = NULL;
            
            // (-// '+ (rho 5 4 3 2 (iota 120)))
            // (-// '+ (rho 4 3 2 (iota 24)))
            res = current_list->newTensor(true, lisp);
            for (long i = 0; i < shape[1]; i++) {
                columns = current_list->newTensor(true, lisp);
                for (long j = 0; j < shape[0]; j++) {
                    columns->append(current_list->index(j)->index(i));
                }
                columns->setShape();
                call->in_quote(2, columns);
                e = call->eval(lisp);
                res->append(e);
                e->release();
            }
            if (current_list->type == t_list) {
                columns = current_list->index(0)->newTensor(lisp, (List*)res);
                res->release();
                res = columns;
            }
            else
                res->setShape();
            
            call->force_release();
            current_list->release();
            lisp->resetStack();
            lisp->reset_to_true(sb);
            return res;
        }
        
        op = liste[1]->eval(lisp);
        
        if (op->isLambda() || op->isList()) {
            res = op->backreduce(lisp, current_list, sz);
            op->release();
            current_list->release();
            lisp->resetStack();
            return res;
        }
        
        op = eval_body_as_argument_min(lisp, op, P_TWO|P_THREE);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);
        
        Element* e = current_list->reverse(lisp);
        current_list->release();
        current_list = e;
        
        call = lisp->provideCall(op, 2);
        
        call->in_quote(1, current_list->value_on_index(lisp, (long)0));
        call->in_quote(2, current_list->index(1));
        
        e = null_;
        e = call->eval(lisp);
        
        if (lisp->delegation->comparators.check(op->label0())) {
            for (long i = 1; i < current_list->size() - 1 && e == one_value; i++) {
                call->in_quote(1, current_list->value_on_index(lisp, i));
                call->in_quote(2, current_list->index(i + 1));
                e = call->eval(lisp);
            }
            
            call->force_release();
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return e;
        }

        
        call->in_quote(1, e);
        for (long i = 2; i < current_list->size(); i++) {
            call->in_quote(2, current_list->index(i));
            e = call->eval(lisp);
            call->in_quote(1, e);
        }
        e->increment();
        call->force_release();
        e->decrementkeep();
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        return e->release_but_last();
    }
    catch (Error* err) {
        if (call != NULL)
            call->force_release();
        else
            op->release();
        current_list->release();
        lisp->reset_to_true(sb);
        return lisp->check_error(this, err, idxinfo);
    }
    return null_;
}

//------------------------------------------------------------------------------------

Element* List_maplist_eval::eval(LispE* lisp) {
    //Operation is: (// operation l1)
    
    Element* current_list = null_;
    Element* op = liste[1];
    Element* container = NULL;
    
    bool sb = lisp->set_true_as_one();
    void* iter = NULL;
    List* call = NULL;
    
    try {
        lisp->checkState(this);
        if (listesize == 4) {
            current_list = liste[3]->eval(lisp);
            choice = current_list->Boolean();
            current_list->release();
        }
    
        //if choice == no, then it means that the output is always a List object
        //So we cannot build a tensor as output
        char check_tensor = !choice;
        
        current_list = liste[2]->eval(lisp);
        long listsz = current_list->size();
        if (!listsz) {
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return emptylist_;
        }
         
        Element* e = op;
        if (op->is_quote())
            e = op->eval(lisp);
                    
        /*
         The first element is "quoted" to avoid it to be evaluated later
         otherwise we might have an error later.
         Basically, if we have: (map 'string '(ab cd ef))
         
         We are going to create as many calls to "string"" as there are elements in the list:
         
         (string ab)
         (string cd) etc..
         
         We don't want ab, cd to be evaluated or it will yield an error
         Instead we produce:
         
         (string 'ab)
         (string 'cd) etc...
         
         Each argument is "quoted".
         We then use "in_quote" to replace the current element in the quote with a new one.
         */

        int16_t ps = 1;
        if (e->isList()) {
            if (e->size())
                call = lisp->provideCallforTWO(e, ps);
            else
                throw new Error("Error: empty list not accepted here");
        }
        else {
            op = eval_body_as_argument(lisp, op);
            if (op->is_straight_eval())
                call = (List*)op;
            else
                call = new List_eval(lisp, op);
            call->append(lisp->quoted());
        }
        
        iter = current_list->begin_iter();
        Element* nxt = current_list->next_iter_exchange(lisp, iter);
        //Replacing the element in position 1, which is quoted with nxt
        call->in_quote(ps, nxt);
        //"met" is a List function, hence the weird call: call->*met, which consists of executing
        //this method within the current List object: call
        e = call->eval(lisp);
        if (choice) {
            switch (e->type) {
                case t_string:
                    container = lisp->provideStrings();
                    break;
                case t_stringbyte:
                    container = new Stringbytes();
                    break;
                case t_integer:
                    container = lisp->provideIntegers();
                    break;
                case t_float:
                    container = lisp->provideFloats();
                    break;
                case t_number:
                    container = lisp->provideNumbers();
                    break;
                default:
                    container = lisp->provideList();
            }
        }
        else
            container = lisp->provideList();
        
        check_tensor |= e->isPureList();
        e = e->copying(false);
        container->append(e);
        e->release();
        nxt = current_list->next_iter_exchange(lisp, iter);
        while (nxt != emptyatom_) {
            call->in_quote(ps, nxt);
            e = call->eval(lisp)->copying(false);
            check_tensor |= e->isPureList();
            check_tensor |= !e->is_same_tensor(container->last(lisp));
            container->append(e);
            e->release();
            nxt = current_list->next_iter_exchange(lisp, iter);
        }
        call->force_release();
        current_list->clean_iter(iter);
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        
        if (check_tensor == a_valuelist || check_tensor == a_tensor) {
            nxt = container->index(0)->newTensor(lisp, (List*)container);
            if (nxt != container) {
                container->release();
                return nxt;
            }
        }
        return container;
    }
    catch (Error* err) {
        if (call != NULL)
            call->force_release();
        if (iter != NULL)
            current_list->clean_iter(iter);
        lisp->reset_to_true(sb);
        current_list->release();
        if (container != NULL)
            container->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    return emptylist_;
}

Element* List_filterlist_eval::eval(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
    
    Element* current_list = null_;
    Element* op = null_;
    Element* result = null_;
    
    bool sb = lisp->set_true_as_one();
    int16_t ps = 1;
    List* call = NULL;
    Element* save_variable = this;
    int16_t label = -1;
    void* iter =  NULL;
    bool choice = (liste[0]->label() == l_filterlist);
    
    try {
        lisp->checkState(this);
        if (listsz == 4) {
            current_list = liste[3]->eval(lisp);
            choice = current_list->Boolean();
            current_list->release();
        }
        current_list = liste[2]->eval(lisp);
        
        if (choice) {
            switch (current_list->type) {
                case t_floats:
                    result = lisp->provideFloats();
                    break;
                case t_numbers:
                    result = lisp->provideNumbers();
                    break;
                case t_shorts:
                    result = new Shorts();
                    break;
                case t_integers:
                    result = lisp->provideIntegers();
                    break;
                case t_strings:
                    result = lisp->provideStrings();
                    break;
                case t_stringbytes:
                    result = new Stringbytes();
                    break;
                case t_string:
                    result = lisp->provideString();
                    break;
                case t_stringbyte:
                    result = new Stringbyte();
                    break;
                case t_llist:
                    result = new LList(&lisp->delegation->mark);
                    break;
                default:
                    result = lisp->provideList();
            }
        }
        else
            result = lisp->provideList();

        listsz = current_list->size();
        if (!listsz) {
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return result;
        }
        
        op = liste[1];
        Element* e;
        //if choice == no, then it means that the output is always a List object
        //So we cannot build a tensor as output
        char check_tensor = !choice;
        
        if (op->isLambda()) {
            if (!op->index(1)->size())
                throw new Error("Error: Wrong number of arguments");
            label = op->index(1)->index(0)->label();
            if (label < l_final)
                throw new Error("Error: Wrong argument");

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            //if there is already a variable with this name on the stack
            //we record it to restore it later...
            save_variable = lisp->record_or_replace(nxt, label);
            
            e = op->eval_lambda_min(lisp);
            if (e->type != l_return) {
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    result->append(e);
                }
                e->release();
                nxt = current_list->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    lisp->replacestackvalue(nxt, label);
                    e = op->eval_lambda_min(lisp);
                    if (e->type == l_return)
                        break;
                    if (e->Boolean()) {
                        e->release();
                        e = nxt->copying(false);
                        check_tensor |= e->isPureList();
                        check_tensor |= !e->is_same_tensor(result->last(lisp));
                        result->append(e);
                    }
                    e->release();
                    nxt = current_list->next_iter_exchange(lisp, iter);
                }
            }
            current_list->clean_iter(iter);
            lisp->reset_in_stack(save_variable, label);
        }
        else {
            e = op;
            if (op->is_quote())
                e = op->eval(lisp);

            //(filterlist '(< _ 10) (iota 10))
            //(filterlist '(< 10 _) (iota 10)) <=> (filterlist '(< 10) (iota 10))
            //where _ is the slot filling for our variable

            if (e->isList()) {
                if (e->size())
                    call = lisp->provideCallforTWO(e, ps);
                else
                    throw new Error("Error: empty list not accepted here");
            }
            else {
                op = eval_body_as_argument(lisp, op);
                if (op->is_straight_eval())
                    call = (List*)op;
                else
                    call = new List_eval(lisp, op);
                ps = 1;
                call->append(lisp->quoted());
            }
                        
            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);
            while (nxt != emptyatom_) {
                call->in_quote(ps, nxt);
                e = call->eval(lisp);
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    if (check_tensor == a_tensor && result->size())
                        check_tensor |= !e->is_same_tensor(result->last(lisp));
                    result->append(e);
                }
                e->release();
                nxt = current_list->next_iter_exchange(lisp, iter);
            }
            current_list->clean_iter(iter);
            call->force_release();
        }
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        if (check_tensor == a_valuelist || check_tensor == a_tensor) {
            current_list = result->index(0)->newTensor(lisp, (List*)result);
            if (current_list != result) {
                result->release();
                return current_list;
            }
        }
        return result;
    }
    catch (Error* err) {
        if (op->isLambda()) {
            if (save_variable != this)
                lisp->reset_in_stack(save_variable, label);
        }
        else {
            if (call != NULL)
                call->force_release();
        }

        lisp->reset_to_true(sb);
        if (iter != NULL)
            current_list->clean_iter(iter);
        current_list->release();
        result->release();
        return lisp->check_error(this, err, idxinfo);
    }

    return emptylist_;
}

Element* List_takelist_eval::eval(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
    
    Element* current_list = null_;
    Element* op = null_;
    Element* result = null_;
    
    bool sb = lisp->set_true_as_one();
    int16_t ps = 1;
    List* call = NULL;
    Element* save_variable = this;
    int16_t label = -1;
    void* iter = NULL;
    bool choice = (liste[0]->label() == l_takelist);
    
    try {
        lisp->checkState(this);
        if (listsz == 4) {
            current_list = liste[3]->eval(lisp);
            choice = current_list->Boolean();
            current_list->release();
        }
        current_list = liste[2]->eval(lisp);
        
        if (choice) {
            switch (current_list->type) {
                case t_floats:
                    result = lisp->provideFloats();
                    break;
                case t_numbers:
                    result = lisp->provideNumbers();
                    break;
                case t_shorts:
                    result = new Shorts();
                    break;
                case t_integers:
                    result = lisp->provideIntegers();
                    break;
                case t_strings:
                    result = lisp->provideStrings();
                    break;
                case t_string:
                    result = lisp->provideString();
                    break;
                case t_stringbytes:
                    result = new Stringbytes();
                    break;
                case t_stringbyte:
                    result = new Stringbyte();
                    break;
                case t_llist:
                    result = new LList(&lisp->delegation->mark);
                    break;
                default:
                    result = lisp->provideList();
            }
        }
        else
            result = lisp->provideList();

        listsz = current_list->size();
        if (!listsz) {
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return result;
        }
        
        op = liste[1];
        Element* e;
        //if choice == no, then it means that the output is always a List object
        //So we cannot build a tensor as output
        char check_tensor = !choice;
        
        if (op->isLambda()) {
            if (!op->index(1)->size())
                throw new Error("Error: Wrong number of arguments");
            label = op->index(1)->index(0)->label();
            if (label < l_final)
                throw new Error("Error: Wrong argument");

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            //if there is already a variable with this name on the stack
            //we record it to restore it later...
            save_variable = lisp->record_or_replace(nxt, label);
            
            e = op->eval_lambda_min(lisp);
            if (e->type != l_return) {
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    result->append(e);
                }
                else
                    listsz = 0; //we force to stop now...
                
                e->release();
                nxt = current_list->next_iter_exchange(lisp, iter);
                while (nxt != emptyatom_) {
                    lisp->replacestackvalue(nxt, label);
                    e = op->eval_lambda_min(lisp);
                    if (e->type == l_return)
                        break;
                    if (e->Boolean()) {
                        e->release();
                        e = nxt->copying(false);
                        check_tensor |= e->isPureList();
                        check_tensor |= !e->is_same_tensor(result->last(lisp));
                        result->append(e);
                    }
                    else {
                        e->release();
                        break;
                    }
                    e->release();
                    nxt = current_list->next_iter_exchange(lisp, iter);
                }
            }
            current_list->clean_iter(iter);
            lisp->reset_in_stack(save_variable, label);
        }
        else {
            e = op;
            if (op->is_quote())
                e = op->eval(lisp);

            //(takelist '(< _ 10) (iota 10))
            //(takelist '(< 10 _) (iota 10)) <=> (takelist '(< 10) (iota 10))
            //where _ is the slot filling for our variable

            if (e->isList()) {
                if (e->size())
                    call = lisp->provideCallforTWO(e, ps);
                else
                    throw new Error("Error: empty list not accepted here");
            }
            else {
                op = eval_body_as_argument(lisp, op);
                if (op->is_straight_eval())
                    call = (List*)op;
                else
                    call = new List_eval(lisp, op);
                ps = 1;
                call->append(lisp->quoted());
            }

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            while (nxt != emptyatom_) {
                call->in_quote(ps, nxt);
                e = call->eval(lisp);
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    if (check_tensor == a_tensor && result->size())
                        check_tensor |= !e->is_same_tensor(result->last(lisp));
                    result->append(e);
                    e->release();
                }
                else {
                    e->release();
                    break;
                }
                nxt = current_list->next_iter_exchange(lisp, iter);
            }
            current_list->clean_iter(iter);
            call->force_release();
        }
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        if (check_tensor == a_valuelist || check_tensor == a_tensor) {
            current_list = result->index(0)->newTensor(lisp, (List*)result);
            if (current_list != result) {
                result->release();
                return current_list;
            }
        }
        return result;
    }
    catch (Error* err) {
        if (op->isLambda()) {
            if (save_variable != this)
                lisp->reset_in_stack(save_variable, label);
        }
        else {
            if (call != NULL)
                call->force_release();
        }

        if (iter != NULL)
            current_list->clean_iter(iter);
        lisp->reset_to_true(sb);
        current_list->release();
        result->release();
        return lisp->check_error(this, err, idxinfo);
    }

    return emptylist_;
}

Element* List_droplist_eval::eval(LispE* lisp) {
    long listsz = liste.size();
    //Operation is: (// operation l1)
    
    Element* current_list = null_;
    Element* op = null_;
    Element* result = null_;
    
    bool sb = lisp->set_true_as_one();
    int16_t ps = 1;
    List* call = NULL;
    Element* save_variable = this;
    int16_t label = -1;
    void* iter = NULL;
    bool choice = (liste[0]->label() == l_droplist);
    
    try {
        lisp->checkState(this);
        if (listsz == 4) {
            current_list = liste[3]->eval(lisp);
            choice = current_list->Boolean();
            current_list->release();
        }
        current_list = liste[2]->eval(lisp);
        
        if (choice) {
            switch (current_list->type) {
                case t_floats:
                    result = lisp->provideFloats();
                    break;
                case t_numbers:
                    result = lisp->provideNumbers();
                    break;
                case t_shorts:
                    result = new Shorts();
                    break;
                case t_integers:
                    result = lisp->provideIntegers();
                    break;
                case t_strings:
                    result = lisp->provideStrings();
                    break;
                case t_string:
                    result = lisp->provideString();
                    break;
                case t_stringbytes:
                    result = new Stringbytes();
                    break;
                case t_stringbyte:
                    result = new Stringbyte();
                    break;
                case t_llist:
                    result = new LList(&lisp->delegation->mark);
                    break;
                default:
                    result = lisp->provideList();
            }
        }
        else
            result = lisp->provideList();
        

        listsz = current_list->size();
        if (!listsz) {
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return result;
        }
        
        op = liste[1];
        
        Element* e;
        bool add = false;

        //if choice == no, then it means that the output is always a List object
        //So we cannot build a tensor as output
        char check_tensor = !choice;
        
        if (op->isLambda()) {
            if (!op->index(1)->size())
                throw new Error("Error: Wrong number of arguments");
            label = op->index(1)->index(0)->label();
            if (label < l_final)
                throw new Error("Error: Wrong argument");

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            //if there is already a variable with this name on the stack
            //we record it to restore it later...
            save_variable = lisp->record_or_replace(nxt, label);
            
            e = op->eval_lambda_min(lisp);
            if (e->type != l_return) {
                if (e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    result->append(e);
                    add = true;
                }
                e->release();
                nxt = current_list->next_iter_exchange(lisp, iter);
                
                while (nxt != emptyatom_) {
                    lisp->replacestackvalue(nxt, label);
                    e = op->eval_lambda_min(lisp);
                    if (e->type == l_return)
                        break;
                    if (add || e->Boolean()) {
                        e->release();
                        e = nxt->copying(false);
                        check_tensor |= e->isPureList();
                        check_tensor |= !e->is_same_tensor(result->last(lisp));
                        result->append(e);
                        add = true;
                    }
                    e->release();
                    nxt = current_list->next_iter_exchange(lisp, iter);
                }
            }
            current_list->clean_iter(iter);
            lisp->reset_in_stack(save_variable, label);
        }
        else {
            e = op;
            if (op->is_quote())
                e = op->eval(lisp);

            //(droplist '(< _ 10) (iota 10))
            //(droplist '(< 10 _) (iota 10)) <=> (droplist '(< 10) (iota 10))
            //where _ is the slot filling for our variable

            if (e->isList()) {
                if (e->size())
                    call = lisp->provideCallforTWO(e, ps);
                else
                    throw new Error("Error: empty list not accepted here");
            }
            else {
                op = eval_body_as_argument(lisp, op);
                if (op->is_straight_eval())
                    call = (List*)op;
                else
                    call = new List_eval(lisp, op);
                ps = 1;
                call->append(lisp->quoted());
            }

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            while (nxt != emptyatom_) {
                call->in_quote(ps, nxt);
                e = call->eval(lisp);
                if (add || e->Boolean()) {
                    e->release();
                    e = nxt->copying(false);
                    check_tensor |= e->isPureList();
                    if (check_tensor == a_tensor && result->size())
                        check_tensor |= !e->is_same_tensor(result->last(lisp));
                    result->append(e);
                    add = true;
                }
                e->release();
                nxt = current_list->next_iter_exchange(lisp, iter);
            }
            current_list->clean_iter(iter);
            call->force_release();
        }
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        if (check_tensor == a_valuelist || check_tensor == a_tensor) {
            current_list = result->index(0)->newTensor(lisp, (List*)result);
            if (current_list != result) {
                result->release();
                return current_list;
            }
        }
        return result;
    }
    catch (Error* err) {
        if (op->isLambda()) {
            if (save_variable != this)
                lisp->reset_in_stack(save_variable, label);
        }
        else {
            if (call != NULL)
                call->force_release();
        }

        if (iter != NULL)
            current_list->clean_iter(iter);
        lisp->reset_to_true(sb);
        current_list->release();
        result->release();
        return lisp->check_error(this, err, idxinfo);
    }
    return emptylist_;
}

Element* List_scanlist_eval::eval(LispE* lisp) {
    long listsz = liste.size();
    
    Element* current_list = null_;
    Element* op = null_;
    Element* result = null_;
    
    bool sb = lisp->set_true_as_one();
    int16_t ps = 1;
    List* call = NULL;
    Element* save_variable = this;
    int16_t label = -1;
    void* iter = NULL;
    
    try {
        lisp->checkState(this);
        current_list = liste[2]->eval(lisp);

        listsz = current_list->size();
        if (!listsz) {
            current_list->release();
            lisp->reset_to_true(sb);
            lisp->resetStack();
            return result;
        }
        
        op = liste[1];
                
        if (op->isLambda()) {
            if (!op->index(1)->size())
                throw new Error("Error: Wrong number of arguments");
            label = op->index(1)->index(0)->label();
            if (label < l_final)
                throw new Error("Error: Wrong argument");

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            //if there is already a variable with this name on the stack
            //we record it to restore it later...
            save_variable = lisp->record_or_replace(nxt, label);
            while (nxt != emptyatom_) {
                lisp->replacestackvalue(nxt, label);
                result = op->eval_lambda_min(lisp);
                if (result != null_)
                    break;
                nxt = current_list->next_iter_exchange(lisp, iter);
            }
            current_list->clean_iter(iter);
            lisp->reset_in_stack(save_variable, label);
        }
        else {
            Element* e = op;
            if (op->is_quote())
                e = op->eval(lisp);

            if (e->isList()) {
                if (e->size())
                    call = lisp->provideCallforTWO(e, ps);
                else
                    throw new Error("Error: empty list not accepted here");
            }
            else {
                op = eval_body_as_argument(lisp, op);
                if (op->is_straight_eval())
                    call = (List*)op;
                else
                    call = new List_eval(lisp, op);
                ps = 1;
                call->append(lisp->quoted());
            }

            iter = current_list->begin_iter();
            Element* nxt = current_list->next_iter_exchange(lisp, iter);

            while (nxt != emptyatom_) {
                call->in_quote(ps, nxt);
                result = call->eval(lisp);
                if (result != null_)
                    break;
                nxt = current_list->next_iter_exchange(lisp, iter);
            }
            current_list->clean_iter(iter);
            call->force_release();
        }
        current_list->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        return result;
    }
    catch (Error* err) {
        if (op->isLambda()) {
            if (save_variable != this)
                lisp->reset_in_stack(save_variable, label);
        }
        else {
            if (call != NULL)
                call->force_release();
        }

        if (iter != NULL)
            current_list->clean_iter(iter);
        lisp->reset_to_true(sb);
        current_list->release();
        result->release();
        return lisp->check_error(this, err, idxinfo);
    }
    return emptylist_;
}

//------------------------------------------------------------------------------------------
// (° '* '(2 3 4)  '(1 2 3 4))
// (° '* (rho 2 3 '(4 5 6 9)) (rho 3 3 (iota 10)))
Element* List_outerproduct_eval::eval(LispE* lisp) {
    //Operation is: (° operation l1 l2)
    Element* l1 = liste[2]->eval(lisp);
    
    Element* l2 = null_;
    Element* op = null_;
    Element* res = null_;
    List* call = NULL;
    bool sb = lisp->set_true_as_one();

    try {
        lisp->checkState(this);
        l2 = liste[3]->eval(lisp);
        
        if (!l1->isList() || !l2->isList())
            throw new Error("Error: arguments for '°' are lists");
        
        if (l1 == l2) {
            switch (l2->type) {
                case t_floats:
                    l2 = lisp->provideFloats();
                    ((Floats*)l2)->liste = ((Floats*)l1)->liste;
                    break;
                case t_numbers:
                    l2 = lisp->provideNumbers();
                    ((Numbers*)l2)->liste = ((Numbers*)l1)->liste;
                    break;
                case t_shorts:
                    l2 = new Shorts();
                    ((Shorts*)l2)->liste = ((Shorts*)l1)->liste;
                    break;
                case t_integers:
                    l2 = lisp->provideIntegers();
                    ((Integers*)l2)->liste = ((Integers*)l1)->liste;
                    break;
                case t_strings:
                    l2 = lisp->provideStrings();
                    ((Strings*)l2)->liste = ((Strings*)l1)->liste;
                    break;
                case t_stringbytes:
                    l2 = new Stringbytes();
                    ((Stringbytes*)l2)->liste = ((Stringbytes*)l1)->liste;
                    break;
            }
        }

        op = eval_body_as_argument(lisp, liste[1], P_THREE);
        if (op->type == l_equal)
            op = lisp->provideAtom(l_equalonezero);
        
        call = lisp->provideCall(op, 2);
        
        vecte<long> size;
        l1->getShape(size);
        l2->getShape(size);
        char char_tensor = 0;
        res = lisp->provideList();
        ((List*)res)->combine(lisp, res, l1, l2, call, char_tensor);
        if (char_tensor == a_tensor || char_tensor == a_valuelist) {
            Element* nxt = res->index(0)->newTensor(lisp, (List*)res);
            if (nxt != res) {
                res->release();
                res = nxt;
            }
        }
        
        call->force_release();
        l1->release();
        l2->release();
        lisp->reset_to_true(sb);
        lisp->resetStack();
        return res;
    }
    catch (Error* err) {
        if (call != NULL)
            call->force_release();
        lisp->reset_to_true(sb);
        l1->release();
        l2->release();
        res->release();
        return lisp->check_error(this, err, idxinfo);
    }

    return null_;
}

Element* List_rank_eval::eval(LispE* lisp) {
    Element* element = liste[1]->eval(lisp);
    
    long listsz = liste.size();

    Element* e = null_;
    Element* lst = null_;
    vecte<long> positions;
    long i, v;
    
    try {
        lisp->checkState(this);
        for (i = 2; i < listsz; i++) {
            e = liste[i]->eval(lisp);
            if (e->isNumber()) {
                v = e->asInteger();
                positions.push_back(v);
            }
            else
                positions.push_back(-1);
            _releasing(e);
        }
        
        while (positions.size() > 1 && positions.back() < 0)
            positions.pop_back();

        lst = element->rank(lisp, positions);
        element->release();
        if (lst == NULL) {
            lisp->resetStack();
            return emptylist_;
        }
    }
    catch (Error* err) {
        lst->release();
        element->release();
        e->release();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return lst;
}

//------------------------------------------------------------------------------------------
Element* List_insert_eval::eval(LispE* lisp) {
    Element* container = liste[1]->eval(lisp);

    int16_t lstsize = liste.size();
    Element* second_element = null_;
    Element* third_element = NULL;
    List* comparison = NULL;

    try {
        lisp->checkState(this);
        //We insert a value in a list
        second_element = liste[2]->eval(lisp);
        long ix = 0;
        if (lstsize == 3) {
            ix = container->default_insertion();
        }
        else {
            third_element = liste[3]->eval(lisp);
            if (third_element->isNumber())
                ix = third_element->asInteger();
            else {
                third_element = eval_body_as_argument_min(lisp, third_element, P_THREE);
                
                comparison = lisp->provideCall(third_element, 2);
                comparison->in_quote(1, second_element);
                
                Element* result = container->insert_with_compare(lisp, second_element, *comparison);
                container->release();
                comparison->force_release();
                lisp->resetStack();
                return result;
            }
        }
        
        third_element = container->insert(lisp, second_element, ix);
        second_element->release();
        if (third_element != container) {
            container->release();
            lisp->resetStack();
            return third_element;
        }
    }
    catch (Error* err) {
        container->release();
        if (comparison != NULL)
            comparison->force_release();
        else {
            second_element->release();
            if (third_element != NULL)
                third_element->release();
        }
        return lisp->check_error(this, err, idxinfo);
    }

    lisp->resetStack();
    return container;
}
//------------------------------------------------------------------------------------------

Element* List_zipwith_eval::eval(LispE* lisp) {
    Element* container = liste[2]->eval(lisp);
    if (!container->isList()) {
        container->release();
        throw new Error(L"Error: 'zipwith' only accepts lists as arguments");
    }
    
    int16_t listsize = size();
    if (liste.back()->type == v_null)
        listsize--;

    List* call = NULL;
    
    List* lists = lisp->provideList();
    Element* function = liste[1];
    
    long i, lsz, j = 0, szl = container->size();
    
    if (container->type == t_llist)
        lists->append(new Iter_llist((LList*)container, szl));
    else
        lists->append(container);
    
    
    try {
        lisp->checkState(this);
        //We combine different lists together with an operator
        //First element is the operation
        for (i = 3; i < listsize; i++) {
            container = liste[i]->eval(lisp);
            if (container->isList() && szl == container->size()) {
                if (container->type == t_llist)
                    lists->append(new Iter_llist((LList*)container, szl));
                else
                    lists->append(container);
                container = null_;
            }
            else
                throw new Error(L"Error: 'zipwith' only accepts lists of same size as arguments");
        }
        
        if (!szl) {
            lists->release();
            lisp->resetStack();
            return emptylist_;
        }
        
        lsz = lists->size();
        
        ITEM& item = *lists->liste.items;
        
        function = eval_body_as_argument(lisp, function, _arity(lsz+1));
        if (function->is_straight_eval())
            call = (List*)function;
        else {
            call = lisp->provideList();
            call->append(function);
        }
        
        methodEval met = lisp->delegation->evals[function->type];
        for (i = 0; i < lsz; i++)
            call->append(lisp->quoted(item[i]->index(0)));
        
        Element* value = (call->*met)(lisp);
        if (choose) {
            switch (value->type) {
                case t_string:
                    container = lisp->provideStrings();
                    break;
                case t_stringbyte:
                    container = new Stringbytes();
                    break;
                case t_integer:
                    container = lisp->provideIntegers();
                    break;
                case t_float:
                    container = lisp->provideFloats();
                    break;
                case t_number:
                    container = lisp->provideNumbers();
                    break;
                default:
                    container = lisp->provideList();
            }
        }
        else
            container = lisp->provideList();
        container->append(value);
        value->release();
        for (j = 1; j < szl; j++) {
            for (i = 0; i < lsz; i++)
                call->in_quote(i + 1, item[i]->index(j));
            value = (call->*met)(lisp);
            container->append(value);
            value->release();
        }
        
        call->force_release();
        lists->release();
    }
    catch (Error* err) {
        container->increment();
        if (call != NULL)
            call->force_release();
        lists->release();
        container->decrement();
        return lisp->check_error(this, err, idxinfo);
    }
    
    lisp->resetStack();
    return container;
}


Element* List_sort_eval::eval(LispE* lisp) {
    Element* comparator = liste[1]->eval(lisp);
    Element* container = null_;

    comparator = eval_body_as_argument_min(lisp, comparator, P_THREE, true);

    try {
        lisp->checkState(this);
        //First element is the comparison function OR an operator
        container = liste[2]->eval(lisp);
        if (!container->isList())
            throw new Error(L"Error: the second argument should be a list for 'sort'");
    }
    catch (Error* err) {
        comparator->force_release();
        container->release();
        return lisp->check_error(this, err, idxinfo);
    }

    
    List* complist;
    switch (container->type) {
        case t_floats:
        case t_numbers:
        case t_shorts:
        case t_integers:
        case t_strings:
        case t_stringbytes: { // (sort (\(x y) (< y x)) (iota 10))
            try {
                complist = NULL;
                if (!comparator->isList())
                    throw new Error("Error: Expecting a list");
                
                complist = (List*)comparator;
                complist->append(null_);
                complist->append(null_);
                
                container->sorting(lisp, complist);
                
                complist->force_release();
                lisp->resetStack();
                return container;
            }
            catch (Error* err) {
                if (complist != NULL) {
                    complist->liste[1] = null_;
                    complist->liste[2] = null_;
                    complist->force_release();
                }
                container->release();
                return lisp->check_error(this, err, idxinfo);
            }
        }
        case t_llist: {
            List* l = (List*)container->asList(lisp, lisp->provideList());
            if (l->size() <= 1) {
                l->release();
                comparator->force_release();
                lisp->resetStack();
                return container;
            }
            
            complist = lisp->provideCall(comparator, 2);

            complist->in_quote(1, l->index(0));
            complist->in_quote(2, l->index(0));
            try {
                if (complist->eval(lisp)->Boolean()) {
                    throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
                }
            }
            catch (Error* err) {
                container->release();
                complist->force_release();
                return lisp->check_error(this, err, idxinfo);
            }

            l->liste.sorting(lisp, complist);
            u_link* it = ((LList*)container)->liste.begin();
            for (long i = 0; i < l->size(); i++) {
                it->value = l->index(i);
                it = it->next();
            }
            
            l->release();
            complist->force_release();
            lisp->resetStack();
            return container;
        }
        default: {
            List* l = (List*)container;
            if (l->size() <= 1) {
                comparator->force_release();
                lisp->resetStack();
                return container;
            }
            
            complist = lisp->provideCall(comparator, 2);

            complist->in_quote(1, l->index(0));
            complist->in_quote(2, l->index(0));
            try {
                if (complist->eval(lisp)->Boolean()) {
                    throw new Error(L"Error: The comparison must be strict for a 'sort': (comp a a) must return 'nil'.");
                }
            }
            catch (Error* err) {
                container->release();
                complist->force_release();
                return lisp->check_error(this, err, idxinfo);
            }
            l->liste.sorting(lisp, complist);
            complist->force_release();
            lisp->resetStack();
            return container;
        }
    }
}
