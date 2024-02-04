#include "minilisp.h"
#include <unistd.h>

UTF8_Handler special_characters;
string current_directory;

string execute_unix_command(string cmd)
{
    FILE *fp;
    int status;

    char res[PATH_MAX];

    bool get_current = false;
    if (cmd[0] == 'c' && cmd[1] == 'd' && (!cmd[2] || cmd[2] == ' '))
    {
        // In this case we do a fwrite to force the completion
        get_current = true;
        cmd += ";pwd";
    }

    if (current_directory != "")
    {
        chdir(STR(current_directory));
    }

#ifdef WIN32
    fp = _popen(STR(cmd), "r");
#else
    fp = popen(STR(cmd), "r");
#endif
    if (fp == NULL)
        return "Error: the pipe did not open properly\n";

    string result;
    while (fgets(res, PATH_MAX, fp) != NULL)
    {
        cmd = res;
        result += cmd;
    }

#ifdef WIN32
    status = _pclose(fp);
#else
    status = pclose(fp);
#endif
    if (status == -1)
    {
        result = "Error: when closing the pipe\n";
    }
    else
    {
        if (get_current)
        {
            current_directory = result;
            s_trim(current_directory);
            result = "true\n";
        }
    }
    return result;
}

// Minimale version without the internal editor
int main(int argc, char *argv[])
{
    lisp_mini lisp;
    lisp_element *e;

    string code;

    cout << endl
         << "mini lisp" << endl;

    while (true)
    {
        std::cout << "> ";
        while (getline(std::cin, code))
        {
            s_trim(code);
            if (code != "")
            {
                if (code == "end")
                {
                    cout << "end" << endl;
                    exit(-1);
                }
                if (code[0] != '(' && code.back() != ')')
                {
                    code = "(print " + code + ")";
                }
                e = lisp.run(code);
                string v;
                e->stringvalue(v);
                std::cout << v << std::endl;
                code = "";
            }
            std::cout << "> ";
        }
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << ">> try again !!!" << endl;
    }
}