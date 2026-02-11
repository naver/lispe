/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  mainasm.cxx

//-------------------------------------------------------------------------------------------
#include <unistd.h>
#include "lispe.h"
#include <emscripten/emscripten.h>

//--------------------------------------------------------------------
typedef enum
{
    console_log,
    console_warn,
    console_group,
    console_error,
    console_debug,
    console_table,
    console_info,
    console_groupEnd,
    console_time,
    console_timeEnd,
    console_clear
} console_action;

void display_the_console(std::string &str, console_action act)
{
    switch (act)
    {
    case console_log:
        EM_ASM({ console.log(UTF8ToString($0)); }, str.c_str());
        break;
    case console_warn:
        EM_ASM({ console.warn(UTF8ToString($0)); }, str.c_str());
        break;
    case console_group:
        EM_ASM({ console.group(UTF8ToString($0)); }, str.c_str());
        break;
    case console_error:
        EM_ASM({ console.error(UTF8ToString($0)); }, str.c_str());
        break;
    case console_debug:
        EM_ASM({ console.debut(UTF8ToString($0)); }, str.c_str());
        break;
    case console_table:
        EM_ASM({ console.table(UTF8ToString($0)); }, str.c_str());
        break;
    case console_info:
        EM_ASM({ console.info(UTF8ToString($0)); }, str.c_str());
        break;
    case console_time:
        EM_ASM({ console.time(UTF8ToString($0)); }, str.c_str());
        break;
    case console_timeEnd:
        EM_ASM({ console.timeEnd(UTF8ToString($0)); }, str.c_str());
        break;
    }
}

class consoleaction : public Element
{
public:
    int16_t l_str;
    console_action reg;
    consoleaction(LispE *lisp, console_action r) : reg(r), Element(l_lib, s_constant)
    {
        u_ustring w = U"str";
        l_str = lisp->encode(w);
    }

    Element *eval(LispE *lisp)
    {
        switch (reg)
        {
        case console_log:
        case console_group:
        case console_error:
        case console_debug:
        case console_table:
        case console_time:
        case console_timeEnd:
        case console_info:
        case console_warn:
        {
            string str = lisp->get(l_str)->toString(lisp);
            display_the_console(str, reg);
            return true_;
        }
        case console_groupEnd:
            EM_ASM({ console.groupEnd(); });
            return true_;
        case console_clear:
            EM_ASM({ console.clear(); });
            return true_;
        }
        return null_;
    }

    wstring asString(LispE *lisp)
    {
        switch (reg)
        {
        case console_log:
            return L"Outputs a message to the web console.";
        case console_warn:
            return L"Outputs a warning message to the web console.";
        case console_group:
            return L"Creates a new inline group in the web console.";
        case console_error:
            return L"Outputs an error message to the web console.";
        case console_debug:
            return L"Outputs a message to the web console, only if the console's logging level is set to 'debug'.";
        case console_table:
            return L"Displays data as a table.";
        case console_info:
            return L"Outputs an informational message to the web console.";
        case console_groupEnd:
            return L"Ends the most recent inline group in the web console.";
        case console_time:
            return L"Starts a timer you can use to track how long an operation takes.";
        case console_timeEnd:
            return L"Stops a timer that was previously started by console_time.";
        case console_clear:
            return L"Clears the console.";
        }

        return L"";
    }
};

// We are also going to implement the body of the call
void moduleconsole(LispE *lisp)
{
    lisp->extension("deflib console_log(str)", new consoleaction(lisp, console_log));
    lisp->extension("deflib console_warn(str)", new consoleaction(lisp, console_warn));
    lisp->extension("deflib console_error (str)", new consoleaction(lisp, console_error));
    lisp->extension("deflib console_group (str)", new consoleaction(lisp, console_group));
    lisp->extension("deflib console_debug (str)", new consoleaction(lisp, console_debug));
    lisp->extension("deflib console_groupEnd ()", new consoleaction(lisp, console_groupEnd));
    lisp->extension("deflib console_clear ()", new consoleaction(lisp, console_clear));
    lisp->extension("deflib console_table (str)", new consoleaction(lisp, console_table));
    lisp->extension("deflib console_time (str)", new consoleaction(lisp, console_time));
    lisp->extension("deflib console_info (str)", new consoleaction(lisp, console_info));
    lisp->extension("deflib console_timeEnd (str)", new consoleaction(lisp, console_timeEnd));
}
