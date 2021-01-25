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


//-------------------------------------------------------------------------------------------
#ifdef DEBUG
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
    LispE lisp;
    Element* e;
    
    if (codeinitial != "") {
        e = lisp.eval(codeinitial);
        if (e->isError()) {
            std::cerr << e->toString(&lisp);
            exit(-1);
        }
    }

    lisp.eval("(setq accu1 0) (setq accu2 0) (setq accu3 0) (setq accu4 0) (setq accu5 0) (setq accu6 0) (setq accu7 0) (setq accu8 0) (setq accu9 0)");

    if (with_file) {
        e = lisp.load(code);
        if (e->isError()) {
            std::cerr << e->toString(&lisp);
            exit(-1);
        }
        if (!lisp.checkencoding(L"runpipe")) {
            std::cerr << "Error: function '(defun runpipe() ...)' is not defined" << endl;
            exit(-1);
        }
        code = "(runpipe)";
    }
    
    string line_in;
    vector<string> fields;
    string blanc(" ");
    short l;
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
        e = lisp.eval(setq.str());
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
        
        
        e = lisp.eval(setq.str());
        
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

    string args;
    string code;
    string file_name;
    string rgx;
    string codeinitial;
    string codefinal;
    bool darkmode = false;
    
    long i;
    for (i = 1; i < argc; i++) {
        args = argv[i];
        if (args == "-h" || args == "--help" || args == "-help" || args == "--h") {
            cout << endl << m_red << "Different types of operation: " << m_current << endl << endl;

            cout << m_red << "    Interactive mode" << m_current << endl;
            cout << "    lispe"<< endl << endl;
            cout << m_red << "    This help" << m_current << endl;
            cout << "    lispe -b"<< endl << endl;
            cout << m_red << "    dark mode" << m_current << endl;
            cout << "    lispe -h|-help|--h|--help"<< endl << endl;
            cout << m_red << "    Execution of 'program' with optional list of arguments" << m_current << endl;
            cout << "    lispe program arg1 arg2"<< endl<< endl;
            cout << m_red << "    Launch the debugger. '-n' is optional" << m_current << endl;
            cout << "    lispe -d program -n line_number arg1 arg2"<< endl<< endl;
            cout << m_red << "    Edit 'program' with optional list of arguments" << m_current << endl;
            cout << "    lispe -e program arg1 arg2"<< endl<< endl;
            cout << m_red << "    The entire pipe is stored in '_args'" << m_current << endl;
            cout << "    lispe -a:"<< endl;
            cout << "        - lispe -a:" << m_blue << "gives back the hand in the interactive interpreter"<< endl;
            cout << "        - lispe " << m_blue<< "program -a execute 'program' with _args containing the output of the 'pipe'"<< endl << endl;
            cout << m_red << "    Execution of 'code' on a pipe output: ls -al | lisp -p '(+ l2 l3)'" << m_current << endl;
            cout << "    lispe -pb/-pe/-p 'code' arg1 arg2:"<< endl;
            cout << "        - '-pb' " << m_blue << "allows to execute an initial code (must be placed before '-p')" << m_current << endl;
            cout << "        - '-pe' " << m_blue << "allows to execute a final code (must be placed before '-p')" << m_current << endl;
            cout << m_red << "        The lines are automatically cut along the spaces into variables:" << m_current << endl;
            cout << "            - accu1, accu2,..., accu9: " << m_blue << "Nine accumulators (= 0 at startup)" << m_current << endl;
            cout << "            - ln: " << m_blue << "is the number of fields" << m_current << endl;
            cout << "            - ll: " << m_blue << "is the list of fields" << m_current << endl;
            cout << "            - l0: " << m_blue << "is the full line" << m_current << endl;
            cout << "            - l1, l2, l3...: " << m_blue << "each variable corresponds to a field in the split line" << m_current << endl << endl;
            cout << m_red << "    Execution of program file on a pipe output: ls -al | lisp -P file" << m_current << endl;
            cout << "    lispe -P program.lisp arg1 arg2:"<< endl;
            cout << "        - '-P' "<< m_blue << "has the same variables as '-p'. m_current" << m_current << endl;
            cout << "        - '-P' 'program.lisp' " << m_blue << "must contain the function: (defun runpipe()...)" << m_current << endl << endl;
            cout << "    lispe -r 'rgx': "<< m_blue << "Condition (posix regular expressions) must appears right before '-p' or '-P'" << m_current << endl;
            cout << "    lispe -R 'rgx': " << m_blue << "Condition (internal regular expressions) must appears right before '-p' or '-P'" << m_current << endl;
            cout << endl << endl;
            return -1;
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
            JAGEDITOR->launchterminal(darkmode, 4, arguments);
            return 0;
        }

        if (args == "-e") {
            JAGEDITOR = new lispe_editor();
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
            JAGEDITOR->launchterminal(darkmode, 3, arguments);
            return 0;
        }
        
        if (file_name == "")
            file_name = argv[i];
        arguments.push_back(argv[i]);
    }
    
    if (file_name != "") {
        LispE lisp;
        lisp.arguments(arguments);
        string the_file = file_name;
        Element* e = lisp.load(the_file);
        std::cout << e->toString(&lisp) << std::endl;
        e->release();
        return 0;
    }
    
    JAGEDITOR = new lispe_editor();
    JAGEDITOR->launchterminal(darkmode, 2, arguments);
}
#endif
