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

#include "editor.h"

using std::stringstream;
string paste_from_clipboard();
//--------------------------------------
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

class interpreter_editor;
void resizewindow(int theSignal);

//------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------
// The main editor class for handling LispE expressions
//------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------
void interpreter_editor::displaythehelp(long i) {
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
    }
    
    if (!i || i == 2) {
        cerr << "   - " << m_redbold << "2. Command line mode:" << m_current << endl;
        cerr << "   \t- " << m_redbold << "help:" << m_current << " display the help" << endl;
        cerr << "   \t- " << m_redbold << "help n:" << m_current << " display one of the help sections (from 1 to 5)" << endl;
        cerr << "   \t- " << m_redbold << "cls:" << m_current << " clear screen" << endl;
        cerr << "   \t- " << m_redbold << "colors:" << m_current << " display all possible colors in terminal" << endl;
        cerr << "   \t- " << m_redbold << "colors attribute foreground background:" << m_current << " display one color with these values" << endl;
        cerr << "   \t" << m_redbold << "\t- use -1 for attribute, foreground or background to loop on a specific color" << m_current << endl;
        cerr << "   \t- " << m_redbold << "syntax:" << m_current << " display the selected colors for each token type in a program" << endl;
        cerr << "   \t- " << m_redbold << "syntax no:" << m_current << " no colors" << endl;
        cerr << "   \t- " << m_redbold << "syntax dark:" << m_current << " dark mode" << endl;
        cerr << "   \t- " << m_redbold << "syntax full:" << m_current << " full color mode" << endl;
        cerr << "   \t- " << m_redbold << "syntax syncolor:" << m_current << " display the '-syncolor' online command for LispE" << endl;
        cerr << "   \t" << m_redbold << "\tExample: lispe -syncolor 0 31 49 3 0 0 0 34 49 0 90 49 0 32 49 1 91 49" << m_current << endl;
        cerr << "   \t- " << m_redbold << "syntax type att fg bg:" << m_current << " modify the syntactic color associated with 'type'" << endl;
        cerr << "   \t" << m_redbold << "\tExample: syntax definition 3 31 49" << m_current << endl;
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
        cerr << "   \t- " << m_redbold << "Ctrl-l:" << m_current << " load file from disk" << endl;
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
        cerr << "   \t\t- " << m_redital << "r:" << m_current << " run the code" << endl;
        cerr << "   \t\t- " << m_redital << "w:" << m_current << " write and quit" << endl;
        cerr << "   \t\t- " << m_redital << "l:" << m_current << " reload a file" << endl;
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

bool checkOtherCases(u_ustring& u) {
    return (u == U"_current" || u == U"_args" || u == U"_pi" || u == U"_e" || u == U"_tau" || u == U"_phi" || u == U"π" || u ==U"τ" || u == U"ℯ" || u == U"ϕ");
}

//all characters are ascii
u_ustring from_ascii(string s) {
    u_ustring c(s.size(),0);
    for (long i = 0; i < s.size(); i++)
        c[i] = (u_uchar)s[i];
    return c;
}

bool interpreter_editor::checkcommand(char c) {
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
        default:
            return jag_editor::checkcommand(c);
    }
    return false;
}

wstring interpreter_editor::unix_command(wstring line) {
    //We launch a Unix command...
    //We remove the "!"
    return line.substr(1, line.size() - 1);
}

long interpreter_editor::handlingcommands(long pos, bool& dsp) {
    
    typedef enum {cmd_none, cmd_filename, cmd_spaces, cmd_select, cmd_edit, cmd_run, cmd_cls, cmd_help, cmd_list, cmd_syntax, cmd_colors,
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
        commands[L"colors"] = cmd_colors;
        commands[L"syntax"] = cmd_syntax;
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
            if (line[0] == '!') {
                addcommandline(line);
                line = unix_command(line);
                code = WListing();
                lines.setcode(code, false);
                lines.pop_back();
                code = lines.code();
                SetCurrentCode(code);
                return lines.size();
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
                SetCurrentCode(code);
                posinstring = 0;
                line = L"";
                cout << "Space file: " << thecurrentfilename << " (" << i << ")" << endl;
                pos = 0;
                modified = true;
            }
            return pos;
        case cmd_edit:
            if (activate_mouse)
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
            kbuffer = L"";
            for (long i = 0; i< commandlines.size(); i++)
                kbuffer += commandlines[i] + L"\n";
            kbuffer += L"\n";

            addcommandline(line);
            prefix = editor_prefix;
            wprefix = editor_wprefix;
            
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
                
                init_interpreter(true, thecurrentfilename);
                
                line = L"";
                editmode = false;
                
                run_code();
                return pos;
            }
            
            if (loadfile(v[1])) {
                init_interpreter(true, "");
                cout << m_red;
                load_code(thecurrentfilename);
                cout << m_current;
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
                SetCurrentCode(code);
            }
        }
            line = L"";
            posinstring = 0;
            return pos;
        case cmd_syntax:
            addcommandline(line);
            
            if (v.size() == 1) {
                int j;
                cerr << endl << m_redbold << "Denomination\t" << "att\tfg\tbg" <<endl;
                for (i = 0; i < nbdenomination; i++) {
                    cerr << colors[i] << colordenomination[i] << ":\t";
                    if (colordenomination[i].size() <= 6)
                        cerr << "\t";
                    j = 2;
                    while (colors[i][j] != 'm') {
                        if (colors[i][j] == ';')
                            cerr << "\t";
                        else
                            cerr << colors[i][j];
                        j++;
                    }
                    cerr << m_current << endl;
                }
                cerr << endl;
                return pos;
            }
            if (v.size() == 2) {
                if (v[1] == L"no") {
                    colors.clear();
                    for (i = 0; i < nbdenomination - 1; i++)
                        colors.push_back(m_current);
                    cerr << "no colors" << endl;
                    return pos;
                }
                if (v[1] == L"full") {
                    colors.clear();
                    colors.push_back(m_red); //0
                    colors.push_back(m_ital); //1
                    colors.push_back(m_blue); //2
                    colors.push_back(m_gray); //3
                    colors.push_back(m_green); //4
                    colors.push_back(m_yellow); //5
                    colors.push_back(m_selectgray); //6
                    cerr << "colors full" << endl;
                    return pos;
                }
                if (v[1] == L"dark") {
                    colors.clear();
                    colors.push_back(m_red); //0
                    colors.push_back(m_ital); //1
                    colors.push_back(m_blue); //2
                    colors.push_back(m_gray); //3
                    colors.push_back(m_green); //4
                    colors.push_back(m_yellow); //5
                    colors.push_back(m_selectgray); //6
                    switch_darkmode();
                    cerr << "colors dark mode" << endl;
                    return pos;
                }
                int j, nb;
                cerr << "-syncolor ";
                for (i = 0; i < nbdenomination; i++) {
                    j = 2;
                    nb = 0;
                    while (colors[i][j] != 'm') {
                        if (colors[i][j] == ';') {
                            cerr << " ";
                            nb++;
                        }
                        else
                            cerr << colors[i][j];
                        j++;
                    }
                    if (!nb)
                        cerr << " 0 0 ";
                    else
                        if (nb == 1)
                            cerr << " 0 ";
                        else
                            cerr << " ";
                }
                cerr << endl;
                return pos;
            }
            if (v.size() >= 3) {
                char buffer[100];
                //the second word is the denomination
                string s;
                s_unicode_to_utf8(s, v[1]);
                long att = 0, fg = 0, bg = 0;
                for (i = 0; i < nbdenomination; i++) {
                    if (colordenomination[i] == s) {
                        att = convertinginteger(v[2]);
                        if (v.size() > 3) {
                            fg = convertinginteger(v[3]);
                            if (v.size() > 4)
                                bg = convertinginteger(v[4]);
                        }
                        if (fg != 0) {
                            if (bg != 0)
                                sprintf_s(buffer,100,"\033[%ld;%ld;%ldm", att, fg, bg);
                            else
                                sprintf_s(buffer,100,"\033[%ld;%ldm", att, fg);
                        }
                        else
                            sprintf_s(buffer,100,"\033[%ldm", att);
                        
                        colors[i] = buffer;
                        cout << buffer << s << " " << att << " " << fg << " " << bg << m_current << endl;
                        return pos;
                    }
                }
            }
            
            cerr << m_redbold << "colors takes either: string, definition, instruction, quote, comment, call or selection" << m_current << endl;
            return pos;
        case cmd_colors:
            addcommandline(line);
            
        {
            char buffer[100];
            int att = -1;
            int fg = -1;
            int bg = -1;
            int e_att = 0;
            int e_fg = 0;
            int e_bg = 0;
            switch (v.size()) {
                case 4:
                    bg = (int)convertinginteger(v[3]);
                case 3:
                    fg = (int)convertinginteger(v[2]);
                    e_fg = fg;
                case 2:
                    att = (int)convertinginteger(v[1]);
                    e_att = att;
            }
            
            if (bg != -1) {
                for (e_bg = 0; m_clbg[e_bg]; e_bg++) {
                    if (bg == m_clbg[e_bg]) {
                        bg = -10;
                        break;
                    }
                }
            }
            if (bg == -10)
                bg = m_clbg[e_bg+1];
            else {
                e_bg = 0;
                bg = 0;
            }
            
            if (fg != -1) {
                for (e_fg = 0; m_clfg[e_fg]; e_fg++) {
                    if (fg == m_clfg[e_fg]) {
                        fg = -10;
                        break;
                    }
                }
            }
            
            if (fg == -10)
                fg = m_clfg[e_fg+1];
            else {
                e_fg = 0;
                fg = 0;
            }
            
            if (att != -1) {
                for (e_att = 0; m_attr[e_att] != -1; e_att++) {
                    if (att == m_attr[e_att]) {
                        att = -10;
                        break;
                    }
                }
            }
            if (att == -10)
                att = m_attr[e_att+1];
            else {
                e_att = 0;
                att = -1;
            }
            
            for (int i_att = e_att; m_attr[i_att] != att; i_att++) {
                for (int i_fg = e_fg; m_clfg[i_fg] != fg; i_fg++) {
                    for (int i_bg = e_bg; m_clbg[i_bg] != bg; i_bg++) {
                        sprintf_s(buffer, 100,"\033[%d;%d;%dm", m_attr[i_att], m_clfg[i_fg], m_clbg[i_bg]);
                        printf("%s%d,%d,%d:\t%s displaying color%s\n",m_current,m_attr[i_att], m_clfg[i_fg], m_clbg[i_bg], buffer, m_current);
                    }
                }
                cout << m_current;
            }
            return pos;
        }
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
            
            init_interpreter(false, thecurrentfilename);
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
                init_interpreter(false, thecurrentfilename);
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
                hmap<string, int16_t>:: iterator it;
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
                        SetCurrentCode(code);
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
            init_interpreter(true, thecurrentfilename);
            pos = 0;
            return pos;
        case cmd_reinit:
            addcommandline(line);
            
            thecurrentfilename = "";
            lines.clear();
            line = L"";
            posinstring = 0;
            init_interpreter(true, thecurrentfilename);
            editor_breakpoints.clear();
            pos = 0;
            return pos;
    }
    
    
    //Adding a line into the code
    if (line.size()) {
        execute_code_with_history(line);
        code = WListing();
        
        lines.setcode(code,false);
        pos = lines.size();
        return pos;
    }
    
    return pos;
}

void interpreter_editor::launchterminal(bool darkmode, char noinit, vector<string>& args, vector<string>& newcolors) {
    Au_meta::met = &special_characters;
    clearscreen();
    
    arguments = args;
    
    if (darkmode)
        switch_darkmode();
    
    if (newcolors.size() != 0) {
        colors.clear();
        colors = newcolors;
    }
    
    localhelp << m_red << "^c/q" << m_current << ":cmd line " << m_red << "^xq" << m_current << ":quit";
    
    option = x_none;
    prefix = ">";
    
    cerr << endl << m_redbold << title_string << m_current << endl;

#ifdef WIN32
    SetConsoleCtrlHandler(handle_ctrl_c, TRUE);
#else
    signal(SIGINT,handle_ctrl_c);
#endif
    
    bool dsp = true;
    
    if (ifilenames.size() > 1) {
        currentfileid = 0;
        thecurrentfilename = ifilenames[0];
        lines.setcode(codes[0], false);
        SetCurrentCode(codes[0]);
    }
    
    switch (noinit) {
        case 1:
            prefix = cmd_line_prefix;
            pos = 1;
            line = L"";
            lines.push_back(line);
            poslines.push_back(0);
            line = L"";
            cerr << endl << m_red << "help: display available commands" << m_current << endl << endl;
            printline(pos+1);
            break;
        case 2:
            prefix = cmd_line_prefix;
            cerr << endl << m_red << "help: display available commands" << m_current << endl << endl;
            init_interpreter(false, "");
            lines.push(L"");
            poslines.push_back(0);
            line = L"";
            printline(1);
            break;
        case 3:
            //switch to edit mode
            pos = 0;
            line = L"edit";
            pos = handlingcommands(pos, dsp);
            break;
        default:
            prefix = cmd_line_prefix;
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
