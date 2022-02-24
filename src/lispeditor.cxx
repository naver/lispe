/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispeditor.cxx
//
//

#include "lispeditor.h"


using std::stringstream;
string paste_from_clipboard();
//--------------------------------------
//This is a stub definition to give jag a value
string coloring_line(editor_lines& lines, long currentline, string& line, vector<string>& colors, file_types) {
    return "";
}
bool evaluate_quotes(wstring& l);
//--------------------------------------
string Normalizefilename(string path) {
    char localpath[4096];
    #ifdef WIN32
    _fullpath(localpath, STR(path), 4096);
    #else
    realpath(STR(path), localpath);
    #endif
    return localpath;
}

#ifdef WIN32
BOOL WINAPI handle_ctrl_c(_In_ DWORD dwCtrlType) {
    if (JAGEDITOR != NULL)
        JAGEDITOR->clear();
    return true;
}
#else
static void handle_ctrl_c(int theSignal) {
    if (JAGEDITOR != NULL)
        JAGEDITOR->clear();
}
#endif

class lispe_editor;
void resizewindow(int theSignal);

void debuggerthread(lispe_editor* call);
void displaying_current_lines(LispE* lisp, long current_file, long current_line, lispe_editor* editor);
void display_variables(LispE* lisp, Element* instructions, lispe_editor* editor, bool full);
void lispe_displaystring(string& code, void*);

extern Chaine_UTF8 special_characters;

//------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------
// The main editor class for handling LispE expressions and debugger
//------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------
void lispe_editor::displaythehelp(long i) {
    mouseoff();
    cout << m_clear << m_home;

    cerr << endl << m_redbold << "Commandes :" << m_current << endl << endl ;

    if (!i || i == 1) {
        cerr << "   - " << m_redbold << "1. Programs:" << m_current << endl;
        cerr << "   \t- " << m_redbold << "create filename:" << m_current << " create a file space with a specific file name" << endl;
        cerr << "   \t- " << m_redbold << "open filename:" << m_current << " load a program (use "<< m_redital << "run" << m_current <<" to execute it)" << endl;
        cerr << "   \t- " << m_redbold << "open:" << m_current << " reload the program (use "<< m_redital << "run" << m_current <<" to execute it)" << endl;
        cerr << "   \t- " << m_redbold << "save filename:" << m_current << " save the buffer content in a file" << endl;
        cerr << "   \t- " << m_redbold << "save:" << m_current << " save the buffer content with the current filename" << endl;
        cerr << "   \t- " << m_redbold << "run filename:" << m_current << " load and run a program filename" << endl;
        cerr << "   \t- " << m_redbold << "run:" << m_current << " execute the current content" << endl;
        cerr << "   \t- " << m_redbold << "args v1 v2...vn:" << m_current << " set a list of values to the argument list" << endl;
        cerr << "   \t- " << m_redbold << "filename:" << m_current << " display the current filename" << endl;
        cerr << "   \t- " << m_redbold << "debug filename:" << m_current << " load and debug a program filename" << endl;
        cerr << "   \t- " << m_redbold << "debug:" << m_current << " debug the current content" << endl;
        cerr << "   \t\t- " << m_redbold << "While Debugging:" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "$: execute the program up to the end" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "!: stop the execution" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "%: display all variables" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "#: toggle between displaying local variables on/off" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "&: toggle between displaying 'print' operations on/off'" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "↓: debug inside a function" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "↑: out of a loop or a function" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "←: set a breakpoint at the current line" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "→: jump to the next breakpoint" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "CR: execute the next line" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "var: display the content of the variable 'var'" << m_current << endl;
        cerr << "   \t\t\t- " << m_redbold << "(...): execute some lisp code" << m_current << endl << endl;
    }

    if (!i || i == 2) {
        cerr << "   - " << m_redbold << "2. Command line mode:" << m_current << endl;
        cerr << "   \t- " << m_redbold << "help:" << m_current << " display the help" << endl;
        cerr << "   \t- " << m_redbold << "help n:" << m_current << " display one of the help sections (from 1 to 5)" << endl;
        cerr << "   \t- " << m_redbold << "cls:" << m_current << " clear screen" << endl;
        cerr << "   \t- " << m_redbold << "history:" << m_current << " display the command history" << endl;
        cerr << "   \t- " << m_redbold << "retrieve filename:" << m_current << " load the command history from a file" << endl;
        cerr << "   \t- " << m_redbold << "store filename:" << m_current << " store the command history in a file" << endl;
        cerr << "   \t- " << m_redbold << "filename:" << m_current << " display the current file name" << endl;
        cerr << "   \t- " << m_redbold << "spaces:" << m_current << " display all the files stored in memory with their file space id" << endl;
        cerr << "   \t- " << m_redbold << "select space:" << m_current << " select a file space" << endl;
        cerr << "   \t- " << m_redbold << "create filename:" << m_current << " create a new empty file space" << endl;
        cerr << "   \t- " << m_redbold << "rm:" << m_current << " clear the buffer content" << endl;
        cerr << "   \t- " << m_redbold << "rm b:e:" << m_current << " remove the lines from b to e (b: or :e is also possible)" << endl;
        cerr << "   \t- " << m_redbold << "list:" << m_current << " list the buffer content" << endl;
        cerr << "   \t- " << m_redbold << "list b:e:" << m_current << " display the buffer content from line b to e (b: or :e is also possible)" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-t:" << m_current << " up in the code listing" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-g:" << m_current << " down in the code listing" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-f:" << m_current << " force the current line to be appended at the end of the code" << endl << endl;
    }

    if (!i || i == 3) {
        cerr << "   - " << m_redbold << "3. edit (space):" << m_current << " edit mode. You can optionally select also a file space" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-b:" << m_current << " add a breakpoint at the current line" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-k:" << m_current << " delete from cursor up to the end of the line" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-d:" << m_current << " delete a full line" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-u:" << m_current << " undo last modification" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-r:" << m_current << " redo last modification" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-f:" << m_current << " find a string" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-n:" << m_current << " find next" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-g:" << m_current << " move to a specific line, '$' is the end of the code" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-l:" << m_current << " reload file from disk" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-t:" << m_current << " reindent the code" << endl;
#ifdef WIN32
        cerr << "   \t- " << m_redbold << "Ctrl+Alt-h:" << m_current << " local help" << endl;
#else
        cerr << "   \t- " << m_redbold << "Ctrl-h:" << m_current << " local help" << endl;
#endif
        cerr << "   \t- " << m_redbold << "Ctrl-w:" << m_current << " write file to disk" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-c:" << m_current << " exit the editor" << endl << endl;
        cerr << "   \t- " << m_redbold << "Alt-x:" << m_current << " cut mouse selection" << endl;
        cerr << "   \t- " << m_redbold << "Alt-c:" << m_current << " copy mouse selection" << endl;
        cerr << "   \t- " << m_redbold << "Alt-v:" << m_current << " paste mouse selection" << endl;
        cerr << "   \t- " << m_redbold << "Alt-+:" << m_current << " indent current line or selected lines to the right" << endl;
        cerr << "   \t- " << m_redbold << "Alt--:" << m_current << " de-indent current line or selected lines to the left" << endl << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-x:" << m_redital << " Combined Commands" << m_current << endl;
        cerr << "   \t\t- " << m_redital << "D:" << m_current << " delete a bloc of lines" << endl;
        cerr << "   \t\t- " << m_redital << "n:" << m_current << " hide/display line numbers" << endl;
        cerr << "   \t\t- " << m_redital << "c:" << m_current << " copy a bloc of lines" << endl;
        cerr << "   \t\t- " << m_redital << "x:" << m_current << " cut a bloc of lines" << endl;
        cerr << "   \t\t- " << m_redital << "v:" << m_current << " paste a bloc of lines" << endl;
        cerr << "   \t\t- " << m_redital << "f:" << m_current << " find with LispE regular expressions" << endl;
        cerr << "   \t\t- " << m_redital << "F:" << m_current << " find with posix regular expressions" << endl;
        cerr << "   \t\t- " << m_redital << "d:" << m_current << " debug the code" << endl;
        cerr << "   \t\t- " << m_redital << "r:" << m_current << " run the code" << endl;
        cerr << "   \t\t- " << m_redital << "w:" << m_current << " write and quit" << endl;
        cerr << "   \t\t- " << m_redital << "l:" << m_current << " load a file" << endl;
        cerr << "   \t\t- " << m_redital << "h:" << m_current << " full help" << endl;
        cerr << "   \t\t- " << m_redital << "m:" << m_current << " toggle mouse on/off" << endl;
        cerr << "   \t\t- " << m_redital << "u:" << m_current << " toggle between top and bottom of the screen" << endl;
        cerr << "   \t\t- " << m_redital << "+:" << m_current << " indent current line or selected lines to the right" << endl;
        cerr << "   \t\t- " << m_redital << "-:" << m_current << " de-indent current line or selected lines to the left" << endl;
        cerr << "   \t\t- " << m_redital << "q:" << m_current << " quit" << endl << endl;
    }

    if (!i || i == 4) {
        cerr << "   - " << m_redbold << "4. System:" << m_current << endl;
        cerr << "   \t- " << m_redbold << "!unix:" << m_current << " what follows the " << m_redital << "'!'" << m_current << " will be executed as a Unix command (ex: "<< m_redital << "!ls" << m_current << ")" << endl;
        cerr << "   \t- " << m_redbold << "!vs=unix:" << m_current << " what follows the " << m_redital << "'='" << m_current << " will be executed as a Unix command ("
             << m_redital << "!vs=ls" << m_current << ")" << endl;
        cerr << "   \t- " << m_redbold << "clear (space):" << m_current << " clear the current environment or a specifc file space" << endl;
        cerr << "   \t- " << m_redbold << "reinit:" << m_current << " clear the buffer content and initialize predeclared variables" << endl;
        cerr << "   \t- " << m_redbold << "Ctrl-d:" << m_current << " end the session and exit tamgu" << endl;
        cerr << "   \t- " << m_redbold << "exit:" << m_current << " end the session and exit tamgu" << endl << endl;
    }

    cerr << endl;
}


void lispe_editor::initlisp(bool reinitialize, bool setpath) {
    if (lispe == NULL) {
        lispe = new LispE;
        lispe->arguments(arguments);
        if (setpath)
            lispe->set_pathname(thecurrentfilename);
        return;
    }

    if (reinitialize) {
        if (lispe != NULL)
            delete lispe;
        lispe = new LispE;
        lispe->arguments(arguments);
        if (setpath)
            lispe->set_pathname(thecurrentfilename);
    }
}

bool checkOtherCases(u_ustring& u) {
    return (u == U"_current" || u == U"_args" || u == U"_pi" || u == U"_e" || u == U"_tau" || u == U"_phi" || u == U"π" || u ==U"τ" || u == U"ℯ" || u == U"ϕ");
}


string lispe_editor::coloringline(string line, long current_pos, bool thread) {
    if (line == "")
        return line;

    if (current_pos >= 0 && current_pos < lines.longlines.size()) {
        char long_line = lines.longlines[current_pos];
        if (long_line == 1) {
            line = colors[0] + line + m_current;
            return line;
        }
        else {
            if (long_line == 2) {
                line = colors[4] + line + m_current;
                return line;
            }
        }
    }
    
    string root;
    if (current_pos > 0 && lines.Status(current_pos) == concat_line) {
        bool q = false;
        if (lines[current_pos].find(L'"') != -1) {
            q = true;
            if (evaluate_quotes(lines[current_pos-1])) {
                long nb = line.find("\"");
                if (nb != -1) {
                    nb++;
                    root = colors[0] + line.substr(0, nb);
                    root += m_current;
                    line = line.substr(nb,line.size());
                }
            }
        }
        current_pos--;
        while (current_pos > 0 && lines.Status(current_pos) == concat_line) current_pos--;
        if (current_pos && (lines[current_pos][0] == '#' || lines[current_pos][0] == ';')) {
            line = colors[4] + line + m_current;
            return line;
        }
    }

    Tokenizer* segments = &parse;
    if (thread)
        segments = new Tokenizer;
    else
        segments->clear();

    string sub = line;
    s_trimleft(sub);
    if (sub[0] == ';' || sub[0] == '#') {
        line = colors[4] + line + m_current;
        return line;
    }

    string substring;

    initlisp(false, true);

    bool add = false;

    bool addlisting = lispe->delegation->add_to_listing;
    lispe->delegation->add_to_listing = false;
    lispe->segmenting(line, *segments);
    lispe->delegation->add_to_listing = addlisting;

    long left, right = -1;
    for (long isegment = segments->tokens.size() - 1, ipos = segments->positions.size() -1; ipos >= 0; ipos-=2, isegment--) {
        left = segments->positions[ipos-1];
        right = segments->positions[ipos];
        sub = line.substr(0, left);
        add = false;
        if (isegment > 0 && segments->types[isegment - 1] == l_quote) {
            sub += colors[3];
            add = true;
        }
        switch (segments->types[isegment]) {
            case t_emptystring:
            case t_string:
                right += 1;
                sub += colors[0];
                add = true;
                break;
            case l_quote:
                sub += colors[3];
                add = true;
                break;
            case l_cadr:
                sub += colors[2];
                add = true;
                break;
            case t_atom: //methods
                if (lispe->is_instruction(segments->tokens[isegment]) || checkOtherCases(segments->tokens[isegment])) {
                    sub += colors[2];
                    add = true;
                }
                else {
                    if (isegment > 0 && segments->tokens[isegment-1] == U"(") {
                        sub += colors[5];
                        add = true;
                    }
                }
                break;
            default:
                add = false;
        }

        if (add) {
            if (right > left)
                sub += line.substr(left, right-left);
            sub += m_current;
            if (right < line.size())
                sub += line.substr(right, line.size() - right);
            line = sub;
        }

    }

    if (thread)
        delete segments;

    if (root != "")
        line = root + line;

    return line;
}

bool lispe_editor::checkcommand(char c) {
    switch (c) {
        case 'r':
            if (emode()) {
                lastline = poslines[0];
                previous_noprefix = noprefix;
                if (lines.size()) {
                    bool dsp = true;
                    handle_ctrl_c(0);
                    line = L"run";
                    posinstring = linesize();
                    pos = 0;
                    editmode = true;
                    clearscreen();

                    handlingcommands(pos, dsp);
                    editmode = false;
                    posinstring = 0;
                    pos = lines.size()-1;
                    line = L"";
                    printline(pos+1);
                }
            }
            return true;
        case 'd':
            if (emode()) {
                lastline = poslines[0];
                previous_noprefix = noprefix;
                if (lines.size()) {
                    bool dsp = true;
                    handle_ctrl_c(0);
                    line = L"debug";
                    posinstring = linesize();
                    pos = 0;
                    editmode = true;
                    clearscreen();
                    handlingcommands(pos, dsp);
                    editmode = false;
                    posinstring = 0;
                    pos = lines.size()-1;
                    line = L"";
                    printline(pos+1);
                }
            }
            return true;
        default:
            return jag_editor::checkcommand(c);
    }
    return false;
}

bool lispe_editor::evallocalcode(string code, bool disp) {
    s_trim(code);
    initlisp(false, true);
    Element* e = lispe->eval(code);
    cout << m_redbold;
    cout << e->toString(lispe) << endl;
    cout << m_current;
    if (e->isError()) {
        e->release();
        return false;
    }
    e->release();
    return true;
}

long lispe_editor::handlingcommands(long pos, bool& dsp) {
    typedef enum {cmd_none, cmd_filename, cmd_spaces, cmd_select, cmd_edit, cmd_run, cmd_debug, cmd_cls, cmd_help, cmd_list,
        cmd_rm, cmd_history, cmd_open, cmd_create, cmd_save, cmd_exit, cmd_load_history, cmd_store_history, cmd_clear, cmd_reinit} thecommands;

    static bool init = false;
    static hmap<wstring, thecommands> commands;

    if (!init) {
        init = true;
        commands[L"filename"] = cmd_filename;
        commands[L"spaces"] = cmd_spaces;
        commands[L"edit"] = cmd_edit;
        commands[L"select"] = cmd_select;
        commands[L"run"] = cmd_run;
        commands[L"debug"] = cmd_debug;
        commands[L"cls"] = cmd_cls;
        commands[L"help"] = cmd_help;
        commands[L"list"] = cmd_list;
        commands[L"rm"] = cmd_rm;
        commands[L"history"] = cmd_history;
        commands[L"retrieve"] = cmd_load_history;
        commands[L"store"] = cmd_store_history;
        commands[L"open"] = cmd_open;
        commands[L"load"] = cmd_open;
        commands[L"read"] = cmd_open;
        commands[L"create"] = cmd_create;
        commands[L"save"] = cmd_save;
        commands[L"exit"] = cmd_exit;
        commands[L"clear"] = cmd_clear;
        commands[L"reinit"] = cmd_reinit;
    }

    cout << endl;
    wstring code;

    long i;

    vector<wstring> v;
    vsplit(line, L" ", v);
    if (v.size() == 0)
        return pos;

    thecommands command = cmd_none;
    if (commands.find(v[0]) != commands.end()) {
        command = commands[v[0]];
    }
    else {
		if (line.size()) {
			if (line[0] == '!' || line[0] == '?') {
				if (line[0] == '!') {
					addcommandline(line);

					//We launch a Unix command...
					code = line.substr(1, line.size() - 1);
					long iquote = line.find(L"\"");
					long iequal = line.find(L"=");
					if (iequal != -1 && (iquote == -1 || iequal < iquote)) {
						code = line.substr(iequal + 1, line.size() - iequal);
						line = line.substr(1, iequal - 1);
						line = L"(setq " + line + L" ";
						line += L"(command \"";
						line += code;
						line += L"\"))";
					}
					else {
						line = L"(command \"";
						line += code;
						line += L"\")";
					}

					Executesomecode(line);

					code = WListing();
					lines.setcode(code, false);
					lines.pop_back();
					code = lines.code();
					LispSetCode(code);
					return lines.size();
				}
			}
		}
    }

    switch(command) {
        case cmd_none:
            break;
        case cmd_filename:
            cout << back << m_redbold << "File: " << m_red << thecurrentfilename << m_current << endl;
            return pos;
        case cmd_spaces:
            if (v.size() == 1) {
                cout << back << m_redbold << "Space:" << endl << endl;
                for (i = 0; i < ifilenames.size(); i++) {
                    if (i == currentfileid)
                        cout << back << m_redbold << "File " << i <<": " << m_red << ifilenames[i] << m_current << " <<< " << endl;
                    else
                        cout << back << m_redbold << "File " << i <<": " << m_red << ifilenames[i] << m_current << endl;
                }
                cout << endl;
                return pos;
            } //if a value is provided, then it works as a select...
        case cmd_select:
            addcommandline(line);
            if (v.size() == 1) {
                cout << back << m_redbold << "Missing space id" << endl;
                return pos;
            }
            i = convertinginteger(v[1]);
            if (i < 0 || i >= ifilenames.size()) {
                cout << back << m_redbold << "space: " << i << " does not exist" << endl;
                return pos;
            }
            if (i != currentfileid) {
                //We backup our current undo/redo section
                undos.storein(editors_undos[currentfileid]);
                redos.storein(editors_redos[currentfileid]);

                currentfileid = i;
                code = codes[i];
                thecurrentfilename = ifilenames[i];

                //we now reactivate the current undo/redo section
                editors_undos[i].storein(undos);
                editors_redos[i].storein(redos);

                lines.setcode(code, false);
                LispSetCode(code);
                posinstring = 0;
                line = L"";
                cout << "Space file: " << thecurrentfilename << " (" << i << ")" << endl;
                pos = 0;
                modified = true;
            }
            return pos;
       case cmd_edit:
		   mouseon();
            noprefix = previous_noprefix;
            
#ifndef WIN32
            signal(SIGWINCH, resizewindow);
            selected_x = -1;
            selected_y = -1;
            selected_pos = -1;
            double_click = 0;
#endif
            if (v.size() == 2) {
                i = convertinginteger(v[1]);
                if (i < 0 || i >= ifilenames.size()) {
                    cout << back << m_redbold << "space: " << i << " does not exist" << endl;
                    return pos;
                }
                if (i != currentfileid) {
                    //We backup our current undo/redo section
                    undos.storein(editors_undos[currentfileid]);
                    redos.storein(editors_redos[currentfileid]);
                    lastlines[currentfileid] = lastline;

                    currentfileid = i;
                    code = codes[i];
                    thecurrentfilename = ifilenames[i];
                    lastline = lastlines[i];

                    //we now reactivate the current undo/redo section
                    editors_undos[i].storein(undos);
                    editors_redos[i].storein(redos);

                    lines.setcode(code, false);
                    modified = true;
                }
            }

            addcommandline(line);
            prefix = ">>";

            if (lines.size() == 0) {
                lines.push(L"");
                poslines.clear();
                poslines.push_back(0);
            }

            editmode = true;
            currentline = 0;
            dsp = false;

            pos = lastline;
            option = x_none;
            posinstring = 0;
            line = lines[lastline];
            displaylist(lastline, row_size);
            movetoline(currentline);
            movetobeginning();
            return pos;
        case cmd_run:
            addcommandline(line);
            mouseoff();
            if (v.size() == 1) {
                if (isempty(current_code))
                    return pos;

                initlisp(true, true);

                line = L"";
                editmode = false;

                debugmode = false;
                runcode();
                return pos;
            }

            if (loadfile(v[1])) {
                initlisp(true, false);
                cout << m_red;
                lispe->load(thecurrentfilename);
                cout << m_current;
            }
            else
                cerr << m_redbold << "Cannot load: " << thecurrentfilename << m_current << endl;
            return pos;
        case cmd_debug:
            mouseoff();
            current_line_debugger = -1;
            current_file_debugger = -1;
            current_thread_id = 0;
            addcommandline(line);
            if (v.size() == 1) {
                if (isempty(current_code))
                    return pos;

                initlisp(true, true);
                for (i = 0; i < ifilenames.size(); i++) {
                    if (ifilenames[i] == thecurrentfilename)
                        continue;
                    lispe->add_pathname(ifilenames[i]);
                }

                //We initialize the breakpoints and the trace mode
                if (editor_breakpoints.size()) {
                    lispe->delegation->breakpoints.clear();
                    for (auto& a: editor_breakpoints) {
                        long idfile = lispe->id_file(a.first);
                        for (auto& e: a.second)
                            lispe->delegation->breakpoints[idfile][e.first] = e.second;
                    }

                    lispe->stop_at_next_line(debug_goto);
                }
                else
                    lispe->stop_at_next_line(debug_next);

                line = L"";
                editmode = false;
                debugmode = true;
                option = x_debug;
                tid = new std::thread(debuggerthread, this);
                return pos;
            }

            if (loadfile(v[1])) {
                initlisp(true, false);
                cout << m_red;
                lispe->load(thecurrentfilename);
                cout << m_current;
                debugmode = true;
            }
            else
                cerr << m_redbold << "Cannot load: " << thecurrentfilename << m_current << endl;
            return pos;
        case cmd_cls:
            clearscreen();
            return pos;
        case cmd_help:
            i = 0;
            if (v.size() == 2) {
                i = v[1][0] - 48;
                if (i < 1 || i > 6)
                    i = 0;
            }

            displaythehelp(i);
            return pos;
        case cmd_list:
        case cmd_rm:
            addcommandline(line);
        {
            code = WListing();
            if (isempty(code))
                return pos;

            if (v.size() >= 2 && v[1].size() > 1) {
                wstring c = v[1];
                long ps = c.find(L":");
                if (ps != -1) {
                    long sz = c.size();
                    sz--;
                    v.pop_back();
                    if (ps == 0) { //:10
                        v.push_back(L":");
                        c.erase(0,1);
                        v.push_back(c);
                    }
                    else {
                        if (ps == sz) {//the end...
                            c.erase(sz, 1);
                            v.push_back(c);
                            v.push_back(L":");
                        }
                        else {//10:20
                            v.push_back(c.substr(0, ps));
                            v.push_back(L":");
                            v.push_back(c.substr(ps+1, c.size()));
                        }
                    }
                }
            }

            string codeindente;
            i = 3;
            string cd = convert(code);
            codeindente = "";
            IndentCode(cd, codeindente, i, true, false);
            code = wconvert(codeindente);
            lines.setcode(code, false);

            if (lines.size() == 0)
                return pos;

            long lastline = lines.numeros.back();
            long beg = 0, end = lastline;
            if (v.size() >= 2) {
                if (v[1] == L":") { //list :20
                                    //we display up to the next element (if it part of it...
                    if (v.size() >= 3) {
                        if (v[2] == L"$")
                            end = lastline;
                        else {
                            end = convertinginteger(v[2]) - 1;
                            if (end > lastline)
                                end = lastline;
                        }
                        pos = lines.getlinenumber(end);
                    }
                }
                else {
                    if (v[1] == L"$")
                        beg = lines.size();
                    else {
                        beg = convertinginteger(v[1]) - 1;
                        if (beg < 0)
                            beg = 0;
                        if (beg > lastline)
                            beg = lastline;
                    }
                    pos = lines.getlinenumber(beg);
                    if (v.size() >= 3) {
                        if (v[2] == L":") { //list 23:
                            if (v.size() >= 4)
                                end = convertinginteger(v[3]) - 1; //list 23:40
                        }
                        else
                            end = convertinginteger(v[2]) - 1; //list 10 23

                        if (end > lastline)
                            end = lastline;
                        pos = lines.getlinenumber(end);
                    }
                    else
                        end = beg; // we display one line only
                }
            }
            else
                pos = lines.getlinenumber(lastline);


            if (command == cmd_list) { //list
                cout << endl;
                displaylist(beg, end);
                cout << endl;
            }
            else {
                beg = lines.getlinenumber(beg)+1;
                end = lines.getlinenumber(end)+2;
                lines.erase(beg, end);
                code = lines.code();
                LispSetCode(code);
            }
        }
            line = L"";
            posinstring = 0;
            return pos;
        case cmd_history:
            cerr << endl;
            if (historyfilename != "")
                cerr << m_redbold << "History:" << historyfilename << m_current << endl;

            for (i = 0; i < commandlines.size(); i++)
                cerr << i+1 << " = " << convert(commandlines[i]) << endl;
            cerr << endl;
            addcommandline(v[0]);
            return pos;
        case cmd_load_history: {
            if (v.size() != 2) {
                if (historyfilename == "") {
                    cerr << m_redbold << "Missing filename.." << m_current << endl;
                    return pos;
                }
            }
            else
                historyfilename = Normalizefilename(convert(v[1]));


            ifstream ld(historyfilename, openMode);
            if (ld.fail()) {
                cerr << m_redbold << "Cannot load:" << historyfilename << m_current << endl;
                return pos;
            }
            string s;
            while (!ld.eof()) {
                getline(ld, s);
                s=s_trim(s);
                if (s!="") {
                    code = wconvert(s);
                    commandlines.push_back(code);
                }
            }
            return pos;
        }
        case cmd_store_history: {
            if (v.size() != 2) {
                if (historyfilename == "") {
                    cerr << m_redbold << "Missing filename.." << m_current << endl;
                    return pos;
                }
            }
            else
                historyfilename = Normalizefilename(convert(v[1]));
            ofstream st(historyfilename, std::ios::binary);
            for (i = 0; i < commandlines.size(); i++)
                st << convert(commandlines[i]) << endl;
            return pos;
        }
        case cmd_open:
            addcommandline(line);

            if (v.size() == 1) {
                if (thecurrentfilename == "") {
                    cerr << m_redbold << "Missing filename.." << m_current << endl;
                    return pos;
                }
            }
            if (loadfile(v[1]))
                cerr << m_red << "ok." << m_current << endl;

            initlisp(false, true);
            return pos;
        case cmd_create:
            addcommandline(line);
            if (v.size() == 1) {
                cerr << m_redbold << "Missing filename.." << m_current << endl;
                return pos;
            }
            addspace(convert(v[1]));
            line = L"";
            return pos;
        case cmd_save:
            addcommandline(line);
            if (v.size() == 1) {
                if (thecurrentfilename == "") {
                    cerr << m_redbold << "Missing filename.." << m_current << endl;
                    return pos;
                }
            }
            else {
                thecurrentfilename = convert(v[1]);
                thecurrentfilename = s_trim(thecurrentfilename);
                thecurrentfilename = Normalizefilename(thecurrentfilename);
            }
        {
            string codeindente;
            string cd = current_code;
            i = 3;
            IndentCode(cd, codeindente, i, true, false);
            ofstream wd(thecurrentfilename, std::ios::binary);
            wd << codeindente;
            wd.close();

            //if this is a first saving of this code...
            if (filenames.find(thecurrentfilename) == filenames.end()) {
                initlisp(false, true);
                currentfileid = ifilenames.size();
                ifilenames.push_back(thecurrentfilename);
                filenames[thecurrentfilename] = currentfileid;
                lastlines.push_back(0);
                code = wconvert(cd);
                codes.push_back(code);

                //We also backup our undo/redo section
                editors_undos.push_back(undos);
                editors_redos.push_back(redos);
            }
        }
            return pos;
        case cmd_exit:
            return !terminate();
        case cmd_clear:
            addcommandline(line);
            if (v.size() == 2) {
                i = convertinginteger(v[1]);
                if (i < 0 || i >= ifilenames.size()) {
                    cout << back << m_redbold << "This file space does not exist" << endl;
                    return pos;
                }
                //In this case, we clear one space...
                string current =  ifilenames[i];
                filenames.erase(current);
                ifilenames.erase(ifilenames.begin()+i);
                codes.erase(codes.begin()+i);
                editors_undos.erase(editors_undos.begin()+i);
                editors_redos.erase(editors_redos.begin()+i);

                //We need to resynchronize the id for each file...
                hmap<string, short>:: iterator it;
                for (it = filenames.begin(); it != filenames.end(); it++) {
                    if (it->second > i)
                        --it->second;
                }
                if (ifilenames.size() != 0) {
                    //if i points to currentfileid, we clear it and replaces it, with position 0
                    if (i == currentfileid) {
                        currentfileid = 0; //we select the first one, by default...
                        code = codes[currentfileid];
                        thecurrentfilename = ifilenames[currentfileid];
                        undos.storein(editors_undos[currentfileid]);
                        redos.storein(editors_redos[currentfileid]);
                        lines.setcode(code, false);
                        LispSetCode(code);
                        posinstring = 0;
                        line = L"";
                        cout << "Space file: " << currentfileid << endl;
                        pos = 0;
                        modified = true;
                    }
                }
            }
            else {
                filenames.clear();
                ifilenames.clear();
                codes.clear();
                editors_undos.clear();
                editors_redos.clear();
            }

            if (!ifilenames.size()) {
                thecurrentfilename = "";
                lines.clear();
            }

            line = L"";
            posinstring = 0;
            initlisp(true, true);
            pos = 0;
            return pos;
        case cmd_reinit:
            addcommandline(line);

            thecurrentfilename = "";
            lines.clear();
            line = L"";
            posinstring = 0;
            initlisp(true, true);
            editor_breakpoints.clear();
            pos = 0;
            return pos;
    }


    //Adding a line into the code
    if (line.size()) {
        Executesomecode(line);
        code = WListing();

        lines.setcode(code,false);
        pos = lines.size();
        return pos;
    }

    return pos;
}

void lispe_editor::clean_breakpoints(long idline) {
    long idfile = lispe->id_file(thecurrentfilename);
    try {
        editor_breakpoints.at(thecurrentfilename).at(idline) = 1 - editor_breakpoints.at(thecurrentfilename).at(idline);
    }
    catch(const std::out_of_range& oor) {
        editor_breakpoints[thecurrentfilename][idline] = true;
    }

    lispe->delegation->breakpoints[idfile][idline] = editor_breakpoints[thecurrentfilename][idline];

    cout << back << m_dore << prefixstring(idline) << m_current;
    movetoposition();
}

void lispe_editor::launchterminal(bool darkmode, char noinit, vector<string>& args) {
    clearscreen();

    arguments = args;

    if (darkmode) {
        colors[2] = m_blueblack;
        colors[5] = m_dark_yellow;
    }

    localhelp << m_red << "^c/q" << m_current << ":cmd line " << m_red << "^xq" << m_current << ":quit";

    option = x_none;
    prefix = ">";
#ifdef WIN32
    cerr << endl << m_redbold << "Lisp Elémentaire (" << LispVersion() << ")" << m_current << endl;
    SetConsoleCtrlHandler(handle_ctrl_c, TRUE);
#else
    cerr << endl << m_redbold << "Lisp Elémentaire (" << LispVersion() << "/" << GCC_VERSION << ")" << m_current << endl;
    signal(SIGINT,handle_ctrl_c);
#endif

    bool dsp = true;

	if (ifilenames.size() > 1) {
		currentfileid = 0;
		thecurrentfilename = ifilenames[0];
		lines.setcode(codes[0], false);
		LispSetCode(codes[0]);
	}

    previous_noprefix = false;

    switch (noinit) {
        case 1:
            prefix = "<>";
            pos = 1;
            line = L"";
            lines.push_back(line);
            poslines.push_back(0);
            line = L"";
            cerr << endl << m_red << "help: display available commands" << m_current << endl << endl;
            printline(pos+1);
            break;
        case 2:
            prefix = "<>";
            cerr << endl << m_red << "help: display available commands" << m_current << endl << endl;
            initlisp(false, false);
            lines.push(L"");
            poslines.push_back(0);
            line = L"";
            printline(1);
            break;
        case 3:
            //switch to edit mode
            pos = 0;
            line = L"edit";
            noprefix = true;
            previous_noprefix = true;
            pos = handlingcommands(pos, dsp);
            break;
        case 4:
            pos = 0;
            //launch debug
            line = L"debug";
            pos = handlingcommands(pos, dsp);
            prefix = "<>";
            pos = 0;
            line = L"";
            printline(pos+1);
            break;
        default:
            prefix = "<>";
            cerr << endl << m_red << "help: display available commands" << m_current << endl << endl;
            printline(pos+1);
    }


    clearst();
    long selection_beginning = 0;
    wstring code;
    wstring b;
    string buffer;

    string buff = paste_from_clipboard();
    copybuffer = wconvert(buff);
    kbuffer = copybuffer;
    buff = "";

    long first = 0, last;

    bool inbuffer = false;
    bool instring = false;

    while (1) {
        buff = getch();


        if (emode()) {
            while (isMouseAction(buff)) {
                handlemousectrl(buff);
                buff =  getch();
            }
        }
        else {
            if (isMouseAction(buff)) {
                continue;
            }
        }

#ifdef WIN32
		if (!buff.size())
			continue;
#endif

        //This specific section below is used to
        //read a string a pass it to "input" in the
        //debugger thread...
        if (reading) {
            reading_a_string(buff);
            continue;
        }

        if (debugmode) {
            if (buff == "$") {
                cout << endl << endl;
                cout.flush();
                debugmode = false;
                lispe = master_lisp;
                lispe->delegation->display_string_function = &lispe_displaystring;
                lispe->releasing_trace_lock();
                lispe->stop_trace();
                line = L"";
                continue;
            }

            if (buff == "!") {
                debugmode = false;
                lispe = master_lisp;
                lispe->stop();
                lispe->stop_trace();
                lispe->releasing_trace_lock();
                line = L"";
                continue;
            }

            if (buff == left) {
                //Adding or removing breakpoints
                //You cannot modify the breakpoints in multi-threading
                if (lispe->threaded())
                    continue;

                string file_name = lispe->name_file(current_file_debugger);
                try {
                    editor_breakpoints.at(file_name).at(current_line_debugger) =
                        1 - editor_breakpoints.at(file_name).at(current_line_debugger);
                    lispe->delegation->breakpoints[current_file_debugger][current_line_debugger] =
                        1 - lispe->delegation->breakpoints[current_file_debugger][current_line_debugger];
                }
                catch(const std::out_of_range& oor) {
                    editor_breakpoints[file_name][current_line_debugger] = true;
                    lispe->delegation->breakpoints[current_file_debugger][current_line_debugger] = true;
                }

                displaying_current_lines(lispe, current_file_debugger, current_line_debugger, this);
                display_indication();
                continue;
            }

            if (buff == right) {
                current_line_debugger = -1;
                current_file_debugger = -1;
                current_thread_id = -1;
                lispe->stop_at_next_line(debug_goto);
                lispe->releasing_trace_lock();
                cout << endl << endl;
                continue;
            }

            if (buff == "%") {
                display_variables(lispe, NULL, this, true);
                display_indication();
                continue;
            }

            if (buff == "&") {
                displaying_print = 1 - displaying_print;
                displaying_current_lines(lispe, current_file_debugger, current_line_debugger, this);
                display_indication();
                continue;
            }

            if (buff == "#") {
                displaying_local_variables = 1 - displaying_local_variables;
                displaying_current_lines(lispe, current_file_debugger, current_line_debugger, this);
                display_indication();
                continue;
            }

            if (buff == down) {
                lispe->stop_at_next_line(debug_inside_function);
                lispe->releasing_trace_lock();
                cout << endl << endl;
                cout.flush();
                continue;
            }

            if (buff == up) {
                lispe->stop_at_next_line(debug_none);
                lispe->releasing_trace_lock();
                cout << endl << endl;
                cout.flush();
                continue;
            }

            if (buff[0] == 10 || buff[0] == 13) {
                if (line.size()) {
                    char tr = lispe->trace;
                    lispe->trace = debug_none;
                    string var = "%";
                    if (line.find(L"(") == -1) {
                        var = "";
                        s_unicode_to_utf8(var, line);
                        line = L"(string " + line + L")";
                    }
                    Element* e = lispe->eval(line);
                    cout << endl << endl << colors[2] << var << ": " <<  e->toString(lispe) << m_red << endl << endl;
                    e->release();
                    lispe->trace = tr;
                    line = L"";
                    displaygo(true);
                    continue;
                }
                lispe->stop_at_next_line(debug_next);
                lispe->releasing_trace_lock();
                cout << endl << endl;
                cout.flush();
                continue;
            }
        }

        if (linematch == -2) {
            displaygo(true);
            movetoposition();
        }
        else {
            if (linematch != -1) {
                printline(linematch+1, lines[linematch], -1);
                movetoposition();
            }
        }

        if (buff == (char*)shift_right) {
            //We select to the right...
            if (selected_pos == -1) {
                selected_pos = pos;
                selected_posnext = pos;
                double_click = 0;
                selection_beginning = posinstring;
                selected_x = posinstring;
                selected_y = posinstring++;
            }

            if (selected_y >= lines[pos].size())
                continue;

            unselectlines(pos, pos, selected_x, selected_y);
            selected_y++;
            posinstring = selected_y;
            selectlines(pos, pos, selected_x, selected_y);
            continue;
        }

        if (buff == (char*)shift_left) {
            //We select to the left...
            if (selected_pos == -1 || selected_y == selection_beginning) {
                continue;
            }

            unselectlines(pos, pos, selected_x, selected_y);
            selected_y--;
            posinstring = selected_y;
            selectlines(pos, pos, selected_x, selected_y);
            continue;
        }

        selection_beginning = 0;

        //We clear the selection
        if (selected_pos != -1 && buff[0] != 24)
            unselectlines(selected_pos, selected_posnext, selected_x, selected_y);

        linematch = -1;
        dsp = true;
        if (checkkeyboard(buff, first, last, dsp, noinit)) {
            double_click = 0;
            if (buff[0] != 24) {
                selected_x = -1;
                selected_y = -1;
                selected_pos = -1;
            }
            continue;
        }

        if (selected_pos == pos) {
            //We are going to replace a sequence of characters
            //we delete it first
            deleteselection();
        }

        double_click = 0;
        selected_x = -1;
        selected_y = -1;
        selected_pos = -1;

#ifdef WIN32
        if (!buff[0] && buff[1] == '#') {
            //Special case for ctrl+alt+h (ctrl-h is backdelete on windows
            if (emode()) {
                option = x_none;
                if (!tooglehelp)
                    displayonlast(localhelp.str(), true);
                else {
                    jag_editor::displaylist(poslines[0]);
                    movetoline(currentline);
                    movetoposition();
                }
                tooglehelp = 1 - tooglehelp;
            }
            continue;
        }
#endif

        if (inbuffer) {
            buffer += buff;
            buff = buffer;
            inbuffer = false;
        }

        if (buff.size() == getbuffsize())
            inbuffer = check_utf8(buff, buffer);

        code = wconvert(buff);
        cleanheaders(code);

        //We keep track of the initial form of the line...
        if (emode())
            undo(lines[pos],pos, u_modif); //The value is negative to indicate a deletion

        //Only one character to add, no need for further inspection, no CR in the string as well
        if (code.size() == 1 || buff.find(10) == -1)
            addabuffer(code, instring);
        else
            handleblock(code);
    }
}

bool lispe_editor::Executesomecode(wstring& c) {
    debugmode = false;

    string code = convert(c);

    initlisp(false, true);
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
//-------------------------------------------------------------------------------------------
// Debug Functions
//-------------------------------------------------------------------------------------------
void displaying_current_lines(LispE* lisp, long current_file, long current_line, lispe_editor* editor) {
#ifdef WIN32
    system("cls");
#else
    cout << m_clear << m_home;
#endif

    if (editor->displaying_print && editor->output_string != "") {
        cout << "----------------------------------------" << endl;
        cout << m_blue << editor->output_string << m_current << endl;
        cout << "----------------------------------------" << endl;
    }

    editor->output_string = "";

    string file_name = lisp->name_file(current_file);
    cout << m_red << "File: " << file_name << m_current << " thread: " << lisp->threadId() << endl;

    bool is_thread = lisp->threaded();

    long line =  current_line - 15;
    if (line < 1)
        line = 1;

    string theline;
    map<long, string>::iterator it = lisp->delegation->listing[current_file].upper_bound(line);
    line = current_line + 15;
    for (; it != lisp->delegation->listing[current_file].end(); it++) {
        if (it->first > line)
            break;

        if (it->first == current_line) {
            if (lisp->delegation->check_breakpoints(current_file, it->first))
                cout << m_selectgray << "[^^" << it->first << "] " << it->second << m_current;
            else
                cout << m_selectgray << "[" << it->first << "] " << it->second << m_current;
        }
        else {
            if (lisp->delegation->check_breakpoints(current_file, it->first))
                cout << "(^^" << it->first << ") " << editor->coloringline(it->second, it->first, is_thread) << m_current;
            else
                cout << "(" << it->first << ") " << editor->coloringline(it->second, it->first, is_thread) << m_current;
        }
    }
    cout << endl;
}

void display_variables(LispE* lisp, Element* instructions, lispe_editor* editor, bool full) {
    vector<Element*> atomes;
    if (instructions == NULL)
        lisp->atomsOnStack(atomes);
    else
        lisp->extractAllAtoms(instructions, atomes);

    Element* value;

    if (atomes.size())
        cout << endl;

    std::map<string, Element*> uniques;
    for (auto& a: atomes) {
        if (a->label() == v_true)
            continue;
        uniques[a->toString(lisp)] = a;
    }
    string thevalue;
    Element* e;
    for (auto& a: uniques) {
        e = a.second;
        value = lisp->getvalue(e->label());
        if (!value->isFunction()) {
            thevalue =  value->toString(lisp);
            if (!full && thevalue.size() > 80) {
                thevalue = thevalue.substr(0,80);
                thevalue += "...";
            }
            cout << editor->colors[2] << e->toString(lisp) << ":" << m_red << thevalue << m_current << endl;
        }
    }
}

bool debug_function_lispe(LispE* lisp, List* instructions, void* o) {

    lispe_editor* editor = (lispe_editor*)o;
    long current_line = lisp->delegation->i_current_line;

    if (editor->current_line_debugger == current_line &&
        editor->current_file_debugger == lisp->delegation->i_current_file &&
        editor->current_thread_id == lisp->threadId()) {
        lisp->delegation->next_stop = true;
        return false;
    }

    bool is_thread = lisp->threaded();
    editor->lock.locking(is_thread);
    //We have been waiting for the previous thread to yield
    //However a stop was issued, we return
    if (lisp->isEndTrace()) {
        editor->lispe = editor->master_lisp;
        editor->lock.unlocking(is_thread);
        return false;
    }

    editor->lispe = lisp;

    editor->current_line_debugger = current_line;
    editor->current_file_debugger = lisp->delegation->i_current_file;
    editor->current_thread_id = lisp->threadId();

    displaying_current_lines(lisp, lisp->delegation->i_current_file, current_line, editor);
    if (editor->displaying_local_variables)
        display_variables(lisp, instructions, editor, false);

    editor->display_indication();

    cout.flush();
    lisp->blocking_trace_lock();
    editor->lispe = editor->master_lisp;
    editor->lock.unlocking(is_thread);
    return true;
}

//We use this version for input to deport input to main thread...
void local_readfromkeyboard(string& code, void* o) {

    lispe_editor* editor = (lispe_editor*)o;
    if (editor->output_string != "") {
        if (editor->displaying_print && editor->output_string != "") {
            cout << "----------------------------------------" << endl;
            cout << m_red << editor->output_string << m_current << endl;
            cout << "----------------------------------------" << endl;
        }
        editor->output_string = "";
    }

    bool is_thread = editor->lispe->threaded();
    editor->printlock.locking(is_thread);
    editor->input_string = code;
    //there is a section in launchterminal, which tests reading
    //to detect if we are dealing with a keyboard input...
    //We need this specific code to avoid a conflict between
    //input from within a thread and getch in the main thread...
    editor->reading = true;
    cout << "> ";
    cout.flush();
    editor->lispe->blocking_trace_lock();
    editor->reading = false;
    code = editor->input_string;
    editor->printlock.unlocking(is_thread);
}

void local_display(string& code, void* o) {
    lispe_editor* editor = (lispe_editor*)o;
    bool is_thread = editor->lispe->threaded();
    editor->printlock.locking(is_thread);
    editor->output_string += code;
    editor->printlock.unlocking(is_thread);
}

//This is the debugger thread
void debuggerthread(lispe_editor* call) {
    call->master_lisp = call->lispe;
    call->displaying_print = true;
    call->displaying_local_variables = true;
    call->lispe->delegation->reading_string_function = local_readfromkeyboard;
    call->lispe->delegation->display_string_function = local_display;
    call->lispe->delegation->reading_string_function_object = (void*)call;
    call->lispe->set_debug_function(debug_function_lispe, call);
    cout << m_red;
    call->runcode();
    call->debugmode = false;
    call->option = x_none;
    cout << back << "program terminated" << endl;
    call->line = L"";
    call->displaygo(true);
    cout.flush();
}

