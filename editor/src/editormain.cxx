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
//Important the next file contains the creation of the global variable: special_characters, which is used in many
//places of the editor... It should be systematically included in the main of your application.
#include "special_characters.h"

#if (_MSC_VER >= 1900)
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif

//-------------------------------------------------------------------------------------------
//Change the class name my_editor to what you fancy most...
class my_editor : public interpreter_editor {
    public:

    my_editor() {
        //Change the message that is displayed when launching the editor
        title_string = "This is myeditor";
        // do  you initialisation here
    }

    //Initialisation of your interpreter
    void init_interpreter(bool reinitialize, string filename) {
        //See lispemain.cxx for an example of how to set this method
        if (reinitialize)
            cout << m_red << "Reinitializing your interpreter" << endl;
        else
            cout << m_red << "Creating your interpreter ONLY if it is not available yet" << endl;
    }

    //Load code from a file into your own code before executing run_code()
    //This function can left empty if you execute your code from within run_code directly
    void load_code(string& filename) {
        cout << m_red;
        cout << "Loading your code here:" << filename << endl;
        cout << m_current;

    }

    //Run a program
    bool run_code() {
        string cmd;
        if (thecurrentfilename == "")
        {
            //We simply access the code lines that are stored in memory
            cmd = the_code();
        }
        else
        {
            //Or we try to execute a filename...
            //Note that before executing run_code, load_code has been executed first
            cmd = thecurrentfilename;
        }
        cout << m_red;
        cout << cmd << endl;
        cout << m_current;

        cout << m_red;
        cout << "Running your code here" << endl;
        cout << m_current;
        return true;
    }

    //Run a line of code
    bool execute_code(wstring& c) {
        //If no interpreter exists, we create one
        //The second parameter records in history this line of code
        init_interpreter(false, thecurrentfilename);
        string code = convert(c);
        cout << m_red;
        cout << "Executing your line of code here:" << code << endl;
        cout << m_current;
        return true;
    }

    //If you want to interpret unix commands, preceded with a "!"
    //The editor executes this method
    wstring unix_command(wstring line) {
        //We remove the "!"
        wstring code = line.substr(1, line.size() - 1);

        //We then generate the code corresponding to a system call in your language
        long doublequotes = line.find(L"\"");        
        if (doublequotes != -1) {
            //The we add a "\" in front of each of these double quotes
            //To avoid producing an unbalanced string
            line = s_wreplacestring(line, L"\"", L"\\\"");
        }

        long iequal = line.find(L"=");
        if (iequal != -1) {
            //This is a case such as: !v = ls
            code = line.substr(iequal + 1, line.size() - iequal);
            code = s_trim(code);
            line = line.substr(1, iequal - 1);
            //We generate here: v = system_command("ls")
            line = line + L" = ";
            line += L"system_command(\"";
            line += code;
            line += L"\")";
        }
        else {
            //we generate from !ls --> system_command("ls")
            line = L"system_command(\"";
            line += code;
            line += L"\")";
        }
        
        execute_code(line);    
        return line;
    }

    //Return true if the interpreter exists
    bool interpreter_active() {
        //return true if it is active
        return true;
    }

    //Stop the execution of your interpreter    
    void stop_execution() {
        //Code here
        cout << m_red << "Execution Stops" << endl;
    }


};

//Main
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
            cout << "    interpreter"<< endl << endl;
            cout << m_red << "    dark mode" << m_current << endl;
            cout << "    interpreter -b"<< endl << endl;
            cout << m_red << "    VT100 mouse mode" << m_current << endl;
            cout << "    interpreter -vt100"<< endl << endl;
            cout << m_red << "    Activates mouse" << m_current << endl;
            cout << "    interpreter -m"<< endl << endl;
            cout << m_red << "    This help" << m_current << endl;
            cout << "    interpreter -h|-help|--h|--help"<< endl << endl;
            cout << m_red << "    Execution of 'filename'" << m_current << endl;
            cout << "    interpreter filename"<< endl<< endl;
            cout << m_red << "    Execution of 'filename' with an argument list" << m_current << endl;
            cout << "    interpreter filename arg1 arg2"<< endl<< endl;
            cout << m_red << "    Edit 'filename' with optional list of arguments" << m_current << endl;
            cout << "    interpreter -e filename arg1 arg2"<< endl<< endl;
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
        
        
        if (args == "-b") {
            darkmode = true;
            coloring = m_blueblack;
            continue;
        }
                        
        if (args == "-e") {
            JAGEDITOR = new my_editor();
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
        //Execute your file here
        return 0;
    }
    
    JAGEDITOR = new my_editor();
    JAGEDITOR->setnoprefix();
    JAGEDITOR->vt100 = vt100;
    JAGEDITOR->launchterminal(darkmode, 2, arguments, newcolors);
}


