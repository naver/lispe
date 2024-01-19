/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  main.cxx
//
//

#include "editor.h"
#include "lispe.h"

#if (_MSC_VER >= 1900)
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif

//-------------------------------------------------------------------------------------------
extern UTF8_Handler special_characters;
//-------------------------------------------------------------------------------------------
// LispE Calls
//-------------------------------------------------------------------------------------------
class lispe_editor : public interpreter_editor {
    public:

    //--------------------------------------------------
    //This is the section, which is dedicated to LispE
    LispE* lispe;
    //--------------------------------------------------

    lispe_editor() {
        lispe = NULL;
        //see enum file_types in jag.h
        filetype = lisp_type;
    }

    //setpathname is used to define the type of file according to its suffix
    //see implementation in jag.h
    void setpathname(string path) {
        thecurrentfilename =  path;
    }

    //Return true if the interpreter exists
    bool interpreter_active() {
        return (lispe != NULL);
    }

    //Stop the execution of your interpreter    
    void stopExecution() {
        if (lispe != NULL) {
            lispe->stop();
        }
    }

    //Initialisation of your interpreter
    void init_interpreter(bool reinitialize, bool setpath) {
        if (lispe == NULL) {
            lispe = new LispE(&special_characters);
            lispe->arguments(arguments);
            if (setpath)
                lispe->set_pathname(thecurrentfilename);
            return;
        }
        
        if (reinitialize) {
            if (lispe != NULL)
                delete lispe;
            lispe = new LispE(&special_characters);
            lispe->arguments(arguments);
            if (setpath)
                lispe->set_pathname(thecurrentfilename);
        }
    }

    //Run a program
    bool runcode() {
        cout << m_red;
        Element* e = lispe->execute(current_code, thecurrentfilename);
        std::cout << e->toString(lispe) << std::endl;
        e->release();
        cout << m_current;
        return true;
    }

    //Run a line of code
    bool execute_code(wstring& c) {
        string code = convert(c);
        
        init_interpreter(false, true);
        bool storecode = true;
        //Seulement un nom de variable
        if (code.find("(") == -1 && code.find(")") == -1) {
            code = "(print "+ code + ")";
            storecode = false;
        }
        
        if (!editmode)
            addcommandline(c);
        
        line = L"";
        posinstring = 0;
        
        cout << m_red;
        Element* e = lispe->execute(code);
        std::cout << e->toString(lispe) << std::endl;
        if (!e->isError() && storecode && thecurrentfilename == "") {
            current_code += code;
            current_code += "\n";
        }
        e->release();
        cout << m_current;
        
        
        return true;
    }

    //Load code from a file name
    void load_code(string& n) {
        lispe->load(thecurrentfilename);
    }
};

//-------------------------------------------------------------------------------------------
//Version avec éditeur jag intégré

int main(int argc, char *argv[]) {
    vector<string> arguments;
    vector<string> newcolors;
    
    const char* coloring = m_blue;
    
    string args;
    string code;
    string file_name;
    string rgx;
    string codeinitial;
    string codefinal;
    bool darkmode = false;
    bool vt100 = false;
    bool mouse_on = false;
    
#ifdef __apple_build_version__
    char path[2048];
    strcpy(path, "/usr/local/lib/lispe");
    setenv("LISPEPATH", path, 1);
    mouse_on = true;
#endif
    
#ifdef WIN32
    mouse_on = true;
#endif
    
    long i;
    for (i = 1; i < argc; i++) {
        args = argv[i];
        if (args == "-h" || args == "--help" || args == "-help" || args == "--h") {
            cout << endl << m_red << "Different types of operation: " << m_current << endl << endl;
            
            cout << m_red << "    Interactive mode" << m_current << endl;
            cout << "    lispe"<< endl << endl;
            cout << m_red << "    dark mode" << m_current << endl;
            cout << "    lispe -b"<< endl << endl;
            cout << m_red << "    VT100 mouse mode" << m_current << endl;
            cout << "    lispe -vt100"<< endl << endl;
            cout << m_red << "    Activates mouse" << m_current << endl;
            cout << "    lispe -m"<< endl << endl;
            cout << m_red << "    This help" << m_current << endl;
            cout << "    lispe -h|-help|--h|--help"<< endl << endl;
            cout << m_red << "    Execution of 'filename'" << m_current << endl;
            cout << "    lispe filename"<< endl<< endl;
            cout << m_red << "    Execution of 'filename' with an argument list" << m_current << endl;
            cout << "    lispe filename arg1 arg2"<< endl<< endl;
            cout << m_red << "    Edit 'filename' with optional list of arguments" << m_current << endl;
            cout << "    lispe -e filename arg1 arg2"<< endl<< endl;
            cout << m_red << "    -syncolor (Colors for: strings, definition, instruction, quote, comments, call), 3 digits each" << m_current << endl;
            cout << "        -syncolor no (no colors)" << endl;
            cout << "        -syncolor att fg bg att fg bg att fg bg att fg bg att fg bg att fg bg" << endl << endl;
            cout << endl << endl;
            return -1;
        }

        if (args == "-syncolor") {
            if (i < argc) {
                args = argv[i + 1];
                if (args == "dark") {
                    i++;
                    darkmode = true;
                    continue;
                }
                if (args == "no") {
                    for (long j = 0; j < nbdenomination - 1; j++)
                        newcolors.push_back(m_current);
                    newcolors.push_back(m_selectgray);
                    i++;
                    continue;
                }
            }
            
            long nb = 3*nbdenomination;
            if ((i + nb) >= argc) {
                cerr << "There should be: "<< nb << " values, 3 digits for each denomination: string, definition, instruction, quote, comments, call, selection" << endl;
                exit(-1);
            }
            i++;
            nb += i;
            long col;
            char cp = 0;
            stringstream color;
            color <<  "\033[";
            while (i < nb) {
                args = argv[i++];
                col = convertinginteger(args);
                color << col;
                cp++;
                if (cp == 3) {
                    cp = 0;
                    color << "m";
                    newcolors.push_back(color.str());
                    color.str("");
                    color << "\033[";
                }
                else
                    color << ";";
            }
            --i;
            continue;
        }

        if (args == "-m") {
            mouse_on = true;
            continue;
        }

        if (args == "-vt100") {
            vt100 = true;
            continue;
        }
        
        if (args == "-mg") {
            if (i >= argc - 1) {
                cerr << "Missing code for option '-mg'" << endl;
                exit(-1);
            }
            long v = convertingfloathexa(argv[++i]);
            if (v >= 0)
                margin_value_reference = v;
            continue;
        }
        
        //Lecture du pipe dans args
        if (args == "-a") {
            while (!std::cin.eof()) {
                getline(std::cin, code);
                arguments.push_back(code);
            }
#ifndef WIN32
            freopen("/dev/tty", "rw", stdin);
#endif
            std::cin.clear();
            continue;
        }
        
        if (args == "-b") {
            darkmode = true;
            coloring = m_blueblack;
            continue;
        }
                

        if (args == "-e") {
            JAGEDITOR = new lispe_editor();
            JAGEDITOR->setnoprefix();
            JAGEDITOR->vt100 = vt100;
            JAGEDITOR->activate_mouse = mouse_on;
            string a_file_name;
            if (i < argc - 1) {
                i++;
                a_file_name = argv[i];
                JAGEDITOR->setpathname(a_file_name);
                while (i < argc) {
                    arguments.push_back(argv[i]);
                    i++;
                }
            }
            JAGEDITOR->loadfile(a_file_name);
            wstring w;
            string line = "open ";
            line += a_file_name;
            w = JAGEDITOR->wconvert(line);
            JAGEDITOR->addcommandline(w);
            line = "";
            JAGEDITOR->launchterminal(darkmode, 3, arguments, newcolors);
            return 0;
        }
        
        if (file_name == "")
            file_name = argv[i];
        arguments.push_back(argv[i]);
    }
    
    if (file_name != "") {
        LispE lisp(&special_characters);
        lisp.arguments(arguments);
        string the_file = file_name;
        Element* e = lisp.n_null;
#ifdef LISPE_WASM
        e = lisp.load(the_file);
#else
        try {
            e = lisp.load(the_file);
        }
        catch(Error* err) {
            e = err;
        }
#endif
        std::cout << e->toString(&lisp) << std::endl;
        e->release();
        return 0;
    }
    
    JAGEDITOR = new lispe_editor();
    JAGEDITOR->setnoprefix();
    JAGEDITOR->vt100 = vt100;
    JAGEDITOR->launchterminal(darkmode, 2, arguments, newcolors);
}


