#include <Python.h>
#include <stdio.h>

#ifdef UNIX
#include <signal.h>
#endif

void appel() {
    // Ensure we're using the correct Python path
    PyRun_SimpleString("import sys");
    PyRun_SimpleString("sys.path.append('/Users/roux/opt/anaconda3/envs/chaine/lib/python3.11/site-packages')");
    PyRun_SimpleString("import numpy");

    printf("Successfully imported numpy\n");
}

void clean_signal() {
    signal(SIGINT, SIG_DFL);
    signal(SIGSEGV, SIG_DFL);
    signal(SIGABRT, SIG_DFL);
    signal(SIGBUS, SIG_DFL);
}

void reset_python_state() {
    PyErr_Clear();
    PyRun_SimpleString("import sys\n"
                       "main = sys.modules['__main__']\n"
                       "main.__dict__.clear()\n");
    PyRun_SimpleString("import sys\n"
                       "mods = {k: v for k, v in sys.modules.items()}\n"
                       "for mod in mods:\n"
                       "    if mod not in ('sys', 'builtins', '__main__', 'numpy'):\n"
                       "        del sys.modules[mod]");
}

int main() {
    // Initialize Python interpreter with additional flags for safety
    Py_Initialize();
    appel();

    reset_python_state();
    appel();
    // Clean up Python interpreter
    
    Py_Finalize();

    return 0;
}
