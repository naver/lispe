/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_transducer.cxx


/*
 This the template for new 'extensions' in lispe
 */

#include "lispe.h"
#include "automaton.h"

#ifdef WIN32
#if (_MSC_VER >= 1900)
#pragma comment(lib, "legacy_stdio_definitions.lib")
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif
#endif


//We use this instruction to return a description of the instruction
//Indeed, just do: (print transducer_example) to get this information
typedef enum {trans_trans, trans_load,trans_add,trans_build,trans_store,trans_compilergx,trans_lookup,
    trans_lookdown,trans_parse,trans_factorize, trans_flags} trans_action;


class Automaton : public Element {
public:
    DoubleSideAutomaton* automaton;
    string name;
    bool modified;

    Automaton(short ty) : Element(ty) {
        automaton = NULL;
        modified = false;
    }
    
    wstring asString() {
        wstring n;
        s_utf8_to_unicode(n, USTR(name), name.size());
        return n;
    }

    bool Boolean() {
        if (automaton == NULL)
            return false;
        return true;
    }

    Element* methodInitial(LispE* lisp) {
        
        Element* filename = lisp->get(U"filename");
        if (filename == null_)
            return this;
        
        automaton = new DoubleSideAutomaton;
        string sname = filename->toString(lisp);
        if (!automaton->load(sname)) {
            sname += " cannot be loaded";
            throw new Error(sname);
        }
        
        automaton->fillencoding(false);
        automaton->sorting();
        automaton->start.shuffle();
        return this;
    }

    Element* methodLoad(LispE* lisp) {
        
        Element* filename = lisp->get(U"filename");
        if (automaton != NULL)
            delete automaton;
        automaton = new DoubleSideAutomaton;
        string sname = filename->toString(lisp);
        if (!automaton->load(sname)) {
            sname += " cannot be loaded";
            throw new Error(sname);
        }
        
        automaton->fillencoding(false);
        automaton->sorting();
        automaton->start.shuffle();
        return true_;
    }

    Element* methodStore(LispE* lisp) {

        if (automaton == NULL)
            return null_;

        string name = lisp->get(U"filename")->toString(lisp);
        automaton->normalize = lisp->get(U"normalize")->Boolean();
        automaton->encoding_table = lisp->get(U"latintable")->asInteger();

        if (automaton->store(name))
            return true_;

        return null_;

    }

    Element* methodAdd(LispE* lisp) {
        if (automaton == NULL)
            automaton = new DoubleSideAutomaton;

        Element* ke = lisp->get(U"value");
        automaton->normalize = lisp->get(U"normalize")->Boolean();
        automaton->encoding_table = lisp->get(U"latintable")->asInteger();

        if (ke->type == t_dictionary) {
            hmap<string,string> values;
            Dictionary* d = (Dictionary*)ke;
            string key;
            u_ustring wkey;
            for (auto& a: d->dictionary) {
                wkey = a.first;
                s_unicode_to_utf8(key, wkey);
                values[key] = a.second->toString(lisp);
            }
            automaton->addmap(values);
            return true_;
        }

        throw new Error("Error: expecting a dictionary as input");
    }

    Element* methodBuild(LispE* lisp) {
        string input = lisp->get(U"inputfile")->toString(lisp);
        string output = lisp->get(U"outputfile")->toString(lisp);
        bool norm  = lisp->get(U"normalize")->Boolean();
        long latinencoding = lisp->get(U"latintable")->asInteger();
        
        if (automaton != NULL)
            delete automaton;

        automaton = new DoubleSideAutomaton();
        return booleans_[compileAutomaton(*automaton, input, output, latinencoding, norm)];
    }

    Element* methodFactorize(LispE* lisp) {
        if (automaton == NULL)
            return null_;

        automaton->factorize(0);
        
        return true_;
    }

    Element* methodParse(LispE* lisp) {
        if (automaton == NULL)
            return null_;
        string words = lisp->get(U"sentence")->toString(lisp);
        words+=" ";
        
        long option = lisp->get(U"option")->asInteger();
        long threshold = lisp->get(U"threshold")->asInteger();
        short flags = lisp->get(U"flags")->asInteger();

        charReadString currentreader(words);
        if ((option&1)==1)
            currentreader.addoffsets = true;
        if ((option&2)==2)
            currentreader.addfeatures = true;

        currentreader.begin();
        vector<string> kvs;
        List* result = new List;
        Strings* inter;
        Element* e;
        std::stringstream str;
        while (!currentreader.end()) {
            if (automaton->process(currentreader, kvs, option, threshold, flags)) {
                if (kvs.size()==0)
                    continue;
                
                if (option == 1) { //no features, but offsets...
                    str << currentreader.cbegin;
                    kvs.push_back(str.str());
                    str.str("");
                    str << currentreader.cend;
                    kvs.push_back(str.str());
                    str.str("");
                }
                
                inter = new Strings;
                for (long i = 0; i < kvs.size(); i++) {
                    inter->append(kvs[i]);
                }
                result->append(inter);
                kvs.clear();
            }
        }
        return result;
    }

    Element* methodLookup(LispE* lisp) {
        if (automaton == NULL)
            return null_;

        wstring word = lisp->get(U"word")->asString(lisp);

        long threshold = lisp->get(U"threshold")->asInteger();
        short flags = lisp->get(U"flags")->asInteger();

        vector<string> kvs;
        automaton->up(word, kvs, threshold, flags);
        if (kvs.size() == 0)
            return emptylist_;
        Strings* inter = new Strings;
        for (long i = 0; i < kvs.size(); i++) {
            inter->append(kvs[i]);
        }

        return inter;
    }

    Element* methodLookdown(LispE* lisp) {
        
        if (automaton == NULL)
            return null_;
        
        wstring word = lisp->get(U"word")->asString(lisp);
        char lemma = lisp->get(U"lemma")->asInteger();
        
        vector<string> kvs;
        automaton->down(word, kvs, lemma);
        Strings* inter = new Strings;
        for (long i = 0; i < kvs.size(); i++) {
            inter->append(kvs[i]);
        }
        
        return inter;
    }

    Element* methodCompilergx(LispE* lisp) {
        if (automaton == NULL)
            automaton = new DoubleSideAutomaton();
        
        Element* regular = lisp->get(U"regular");
        
        if (regular == null_) {
            automaton->regulars();
            return true_;
        }
        
        string expression = regular->toString(lisp);
        
        agnostring rgx(expression);
        Element* kfeat = lisp->get(U"vector");
        if (!kfeat->isList())
            throw new Error("Error: We expect the second argument to be a list.");

        string name = lisp->get(U"name")->toString(lisp);
        
        //we first transform each of our features into indexes...
        vector<uint32_t> indexes;
        string s;
        for (int i = 0; i < kfeat->size(); i++) {
            s = kfeat->index(i)->toString(lisp);
            if (s[0] != '\t')
                s = "\t" + s;
            indexes.push_back(automaton->index(s));
        }

        if (!automaton->start.parse(*automaton, rgx, indexes))
            throw new Error("Error: Wrong regular expression");

        if (name != "")
            automaton->store(name);

        return true_;
    }
};


class Lispe_transducer : public Element {
public:
    trans_action action;
    
    Lispe_transducer(short ty, trans_action a) : Element(ty) {
        action =a ;
    }
    
    Element* eval(LispE* lisp) {
        Automaton* automaton = NULL;
        if (action != trans_trans && action != trans_flags) {
            Element* trans = lisp->get(U"trans");
            if (trans->type != type)
                throw new Error("Error: missing transducer");
            automaton = (Automaton*)trans;
        }
        switch (action) {
            case trans_trans:
                automaton = new Automaton(type);
                return automaton->methodInitial(lisp);
            case trans_load: {
                return automaton->methodLoad(lisp);
            }
            case trans_add: {
                return automaton->methodAdd(lisp);
            }
            case trans_build: {
                return automaton->methodBuild(lisp);
            }
            case trans_store:  {
                return automaton->methodStore(lisp);
            }
            case trans_compilergx: {
                return automaton->methodCompilergx(lisp);
            }
            case trans_lookup: {
                return automaton->methodLookup(lisp);
            }
            case trans_lookdown: {
                return automaton->methodLookdown(lisp);
            }
            case trans_parse: {
                return automaton->methodParse(lisp);
            }
            case trans_factorize: {
                return automaton->methodFactorize(lisp);
            }
            case trans_flags: {
                wstring w = asString(lisp);
                return lisp->provideString(w);
            }
        }
        return null_;
    }
    
    wstring asString(LispE* lisp) {
        switch (action) {
            case trans_trans:
                return L"create a transducer object";
            case trans_load: {
                return L"Load a transducer";
            }
            case trans_add: {
                return L"add a dictionary to the automaton";
            }
            case trans_build: {
                return L"Build a transducer file out of a text file containing on the first line surface form, then on next line lemma+features";
            }
            case trans_store:  {
                return L"store an automaton";
            }
            case trans_compilergx: {
                return L"Compile a regular expression combined with a vector and store it with name. If no parameters compile the 'regular' expressions for numbers.";
            }
            case trans_lookup: {
                return L"lookup for a word with a threshold and flags";
            }
            case trans_lookdown: {
                return L"lookdow for the surface form matching a word+pos+features. lemma is optional: if set to 1 or 2 then the string to look for is only a lemma. If set to 2, it also returns the features with the surface form";
            }
            case trans_parse: {
                return L"parse a sentence based on lexicon content";
            }
            case trans_factorize: {
                return L"factorize the arcs and states of the automaton.";
            case trans_flags:
                std::wstringstream str;
                str << L"The available edit distance flags are:" << endl
                << L"a_first: Allow flags to apply to the first character of the string" << endl
                << L"a_change: Allow a character to be changed" << endl
                << L"a_delete: Allow a character to be deleted" << endl
                << L"a_insert: Allow the insertion of a character" << endl
                << L"a_switch: Allow two consecutive characters to be switched" << endl
                << L"a_nocase: case is not taking into account" << endl
                << L"a_repetition: Allow characters to be repeated in sequence" << endl
                << L"a_vowel: Allow to match against accented vowels" << endl
                << L"a_surface: Return the surface form in lookdown" << endl
                << L"a_longest: longest match" << endl
                << L"a_offsets: offsets" << endl
                << L"a_features: features" << endl;
                return str.str();
            }
        }
        return L"";
    }
    
};

extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {
    //We first create the body of the function
    inittableutf8(lisp->handlingutf8);
    wstring w = L"automaton";
    short type_automaton = lisp->encode(w);
    lisp->extension("deflib transducer((filename))", new Lispe_transducer(type_automaton, trans_trans));
    lisp->extension("deflib transducer_load (trans filename)", new Lispe_transducer(type_automaton, trans_load));
    lisp->extension("deflib transducer_add(trans value (normalize true) (latintable 1))", new Lispe_transducer(type_automaton, trans_add));
    lisp->extension("deflib transducer_build(trans inputfile outputfile (normalize true) (latintable 1))", new Lispe_transducer(type_automaton, trans_build));
    lisp->extension("deflib transducer_store(trans name (normalize true) (latintable 1))", new Lispe_transducer(type_automaton, trans_store));
    lisp->extension("deflib transducer_compilergx(trans (regular nil) (vector nil) (name nil))", new Lispe_transducer(type_automaton, trans_compilergx));
    lisp->extension("deflib transducer_lookup(trans word (threshold 0) (flags 0))", new Lispe_transducer(type_automaton, trans_lookup));
    lisp->extension("deflib transducer_lookdown(trans word (lemma 0))", new Lispe_transducer(type_automaton, trans_lookdown));
    lisp->extension("deflib transducer_parse(trans sentence (option 0) (threshold 0) (flags 0))", new Lispe_transducer(type_automaton, trans_parse));
    lisp->extension("deflib transducer_factorize(trans)", new Lispe_transducer(type_automaton, trans_factorize));
    lisp->extension("deflib transducer_flags()", new Lispe_transducer(type_automaton, trans_flags));

    string code = "(setg a_first 1)\n";
    code += "(setg a_change 2)\n";
    code += "(setg a_delete 4)\n";
    code += "(setg a_insert 8)\n";
    code += "(setg a_switch 16)\n";
    code += "(setg a_nocase 32)\n";
    code += "(setg a_repetition 64)\n";
    code += "(setg a_vowel 128)\n";
    code += "(setg a_surface 256)\n";
    code += "(setg a_longest 512)\n";
    code += "(setg a_offsets 1)\n";
    code += "(setg a_features 2)\n";
    
    lisp->execute(code);

    return true;
}
}


