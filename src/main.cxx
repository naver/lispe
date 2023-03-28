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

#include "lispeditor.h"
#if (_MSC_VER >= 1900)
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif

//-------------------------------------------------------------------------------------------
extern UTF8_Handler special_characters;
//-------------------------------------------------------------------------------------------
#ifdef DEBUGG
//Minimale version without the internal editor
int main(int argc, char *argv[]) {
    LispE lisp;
    Element* e;
    
    if (argc == 2) {
        string pathname = argv[1];
        e = lisp.load(pathname);
        std::cout << e->toString(&lisp) << std::endl;
        e->release();
        return 0;
    }
    
    
    string code;
    
    cout << endl << "Lisp Elémentaire (" << LispVersion() << ")" << endl << endl;
    
    while (true) {
        std::cout << "> ";
        while (getline(std::cin, code)) {
            s_trim(code);
            if (code != "") {
                if (code == "fin") {
                    cout << "Fin" << endl;
                    exit(-1);
                }
                if (code[0] != '(' && code.back() != ')') {
                    code  = "(print " + code + ")";
                }
                e = lisp.execute(code);
                std::cout << e->toString(&lisp) << std::endl;
                e->release();
                code = "";
            }
            std::cout << "> ";
        }
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << ">> try again !!!" << endl;
    }
}
#else
void execute_pipe(string& code, string& codeinitial, string& codefinal, string& rgx, bool with_file) {
    LispE lisp(&special_characters);
    Element* e;
    
    if (codeinitial != "") {
        e = lisp.eval(codeinitial);
        if (e->isError()) {
            std::cerr << e->toString(&lisp);
            exit(-1);
        }
    }
    u_ustring u_code = U"(setq accu1 0) (setq accu2 0) (setq accu3 0) (setq accu4 0) (setq accu5 0) (setq accu6 0) (setq accu7 0) (setq accu8 0) (setq accu9 0)";
    lisp.eval(u_code);
    
    if (with_file) {
        e = lisp.load(code);
        if (e->isError()) {
            std::cerr << e->toString(&lisp);
            exit(-1);
        }
        if (!lisp.checkencoding(U"runpipe")) {
            std::cerr << "Error: function '(defun runpipe() ...)' is not defined" << endl;
            exit(-1);
        }
        code = "(runpipe)";
    }
    
    string line_in;
    vector<string> fields;
    string blanc(" ");
    int16_t l;
    long i;
    std::stringstream setq_list;
    std::stringstream setq;
    char rgxtype[] = {0,0,0};
    if (rgx != "") {
        if (rgx[0] == 'p') {
            rgxtype[0] = 'p';
            rgxtype[1] = 'r';
        }
        else
            rgxtype[0] = 'r';
        rgx = rgx.substr(1, rgx.size());
        setq << "(setq rx (" << rgxtype << "gx `" << rgx <<"`))";
        string code = setq.str();
        e = lisp.eval(code);
        if (e->isError()) {
            std::cerr << e->toString(&lisp);
            exit(-1);
        }
        setq.str("");
        setq.clear();
    }
    
    
    while (!std::cin.eof()) {
        getline(std::cin, line_in);
        
        s_trim(line_in);
        if (line_in == "")
            continue;
        
        //Initialisation des champs
        setq << "(setq l0 " << jsonstring(line_in) << ")" << endl;
        setq << "(setq ln " << fields.size() << ")" << endl;
        
        if (rgxtype[0]) {
            setq << "(if (eq (" << rgxtype << "gx_find rx l0) ``) (print) (block ";
        }
        
        fields.clear();
        
        s_split(line_in, blanc, fields);
        setq_list << "(setq ll (list";
        for (i = 0; i < fields.size(); i++) {
            setq_list << " ";
            setq << "(setq l" << i + 1 << " ";
            setq_list << "l" << i + 1;
            line_in = fields[i];
            noconvertingfloathexa(STR(line_in), l);
            if (l == line_in.size()) {
                setq << line_in << ")";
            }
            else
                setq << jsonstring(line_in) << ")";
            setq << endl;
        }
        setq_list << "))";
        
        for (i = fields.size(); i < 16; i++)
            setq << "(setq l" << i + 1 << "\"\")" << endl;
        
        setq << setq_list.str() << endl << code << endl;
        
        if (rgxtype[0])
            setq << "))" << endl;
        
        string code = setq.str();
        e = lisp.eval(code);
        
        line_in = e->toString(&lisp);
        if (line_in != "" && codefinal == "")
            std::cout << line_in << endl;
        e->release();
        
        setq_list.str("");
        setq_list.clear();
        setq.str("");
        setq.clear();
    }
    if (codefinal != "") {
        e = lisp.eval(codefinal);
        line_in = e->toString(&lisp);
        if (line_in != "")
            std::cout << line_in << endl;
        e->release();
    }
}

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
            cout << m_red << "    Launch the debugger. '-n' is optional" << m_current << endl;
            cout << "    lispe -d filename -n line_number arg1 arg2"<< endl<< endl;
            cout << m_red << "    Edit 'filename' with optional list of arguments" << m_current << endl;
            cout << "    lispe -e filename arg1 arg2"<< endl<< endl;
            cout << m_red << "    The entire pipe is stored in '_args'" << m_current << endl;
            cout << "    lispe -a:"<< endl;
            cout << "        - lispe -a: " << coloring << "gives back the hand in the interactive interpreter"<< m_current << endl;
            cout << "        - lispe filename -a: " << coloring << "execute 'filename' with _args containing the output of the 'pipe'"<< m_current << endl << endl;
            cout << m_red << "    Execution of 'code' on a pipe output: ls -al | lisp -p '(+ l2 l3)'" << m_current << endl;
            cout << "    lispe -pb/-pe/-p 'code' arg1 arg2:"<< endl;
            cout << "        - '-pb' " << coloring << "makes it possible to execute an initial code (must be placed before '-p')" << m_current << endl;
            cout << "        - '-pe' " << coloring << "makes it possible to execute a final code (must be placed before '-p')" << m_current << endl;
            cout << m_red << "        The lines are automatically cut along the spaces into variables:" << m_current << endl;
            cout << "            - accu1, accu2,..., accu9: " << coloring << "Nine accumulators (= 0 at startup)" << m_current << endl;
            cout << "            - ln: " << coloring << "is the number of fields" << m_current << endl;
            cout << "            - ll: " << coloring << "is the list of fields" << m_current << endl;
            cout << "            - l0: " << coloring << "is the full line" << m_current << endl;
            cout << "            - l1, l2, l3...: " << coloring << "each variable corresponds to a field in the split line" << m_current << endl << endl;
            cout << m_red << "    Execution of 'filename' on a pipe output: ls -al | lisp -P file" << m_current << endl;
            cout << "    lispe -P filename.lisp arg1 arg2:"<< endl;
            cout << "        - '-P' "<< coloring << "has the same variables as '-p'" << m_current << endl;
            cout << "        - '-P filename' " << coloring << "must contain the function: (defun runpipe()...)" << m_current << endl << endl;
            cout << "    lispe -r 'rgx': "<< coloring << "Condition (posix regular expressions) must appears right before '-p' or '-P'" << m_current << endl;
            cout << "    lispe -R 'rgx': " << coloring << "Condition (internal regular expressions) must appears right before '-p' or '-P'" << m_current << endl;
            cout << endl;
            cout << m_red << "    -syncolor (Colors for: strings, definition, instruction, quote, comments, call), 3 digits each" << m_current << endl;
            cout << "        -syncolor no (no colors)" << endl << endl;
            cout << "        -syncolor att fg bg att fg bg att fg bg att fg bg att fg bg att fg bg" << endl << endl;
            cout << m_red << "    Read: 'https://github.com/naver/lispe/wiki/7.-Shell' for more information" << m_current << endl;
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
        
        if (args == "-pb") {
            if (i >= argc - 1) {
                cerr << "Missing code for option '-pb'" << endl;
                exit(-1);
            }
            codeinitial = argv[++i];
            continue;
        }
        
        if (args == "-pe") {
            if (i >= argc - 1) {
                cerr << "Missing code for option '-pe'" << endl;
                exit(-1);
            }
            codefinal = argv[++i];
            continue;
        }
        
        if (args == "-r") {
            if (i >= argc - 1) {
                cerr << "Missing regular expression for option  '-r'" << endl;
                exit(-1);
            }
            rgx = "p";
            rgx += argv[++i];
            continue;
        }
        
        if (args == "-R") {
            if (i >= argc - 1) {
                cerr << "Missing regular expression for option '-R'" << endl;
                exit(-1);
            }
            rgx = "r";
            rgx += argv[++i];
            continue;
        }
        
        //Lecture du pipe
        if (args == "-p") {
            if (i >= argc - 1) {
                cerr << "Missing code for option '-p'" << endl;
                exit(-1);
            }
            i++;
            code = argv[i];
            while (i < argc) {
                arguments.push_back(argv[i]);
                i++;
            }
            execute_pipe(code, codeinitial, codefinal, rgx, false);
            exit(0);
        }
        
        if (args == "-P") {
            if (i >= argc - 1) {
                cerr << "Missing program file for option '-P'" << endl;
                exit(-1);
            }
            i++;
            file_name = argv[i];
            execute_pipe(file_name,codeinitial, codefinal, rgx, true);
            exit(0);
        }
        
        if (args == "-d") {
            JAGEDITOR = new lispe_editor();
            JAGEDITOR->setnoprefix();
            JAGEDITOR->vt100 = vt100;
            JAGEDITOR->activate_mouse = mouse_on;
            string a_file_name;
            string line;
            if (i < argc - 1) {
                i++;
                a_file_name = Normalizefilename(argv[i++]);
                arguments.push_back(a_file_name);
                JAGEDITOR->setpathname(a_file_name);
                while (i < argc) {
                    line = argv[i++];
                    if (line == "-n") {
                        if (i < argc) {
                            line = argv[i++];
                            long n = convertinginteger(line);
                            ((lispe_editor*)JAGEDITOR)->editor_breakpoints[a_file_name][n] = true;
                        }
                        continue;
                    }
                    arguments.push_back(line);
                }
            }
            else {
                cerr << "Missing file name to start debugging" << endl;
                exit(-1);
            }
            JAGEDITOR->loadfile(a_file_name);
            wstring w;
            line = "debug ";
            line += a_file_name;
            w = JAGEDITOR->wconvert(line);
            JAGEDITOR->addcommandline(w);
            line = "";
            JAGEDITOR->launchterminal(darkmode, 4, arguments, newcolors);
            return 0;
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
#endif

