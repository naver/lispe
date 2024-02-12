#include "minilisp.h"
//Important the next file contains the creation of the global variable: special_characters, which is used in many
//places of the editor... It should be systematically included in the main of your application.
#include "special_characters.h"

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
                stringstream v;
                e->string_to_os(v);
                std::cout << v.str() << std::endl;
                code = "";
            }
            std::cout << "> ";
        }
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << ">> try again !!!" << endl;
    }
}