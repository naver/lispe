#include <Python.h>
#include <stdio.h>
#include <stdlib.h>
#include <ostream>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

using std::string;
using std::cerr;
using std::endl;

#define STR(x) (char*)x.c_str()

string cs_unicode_to_utf8(wchar_t code) {
    char utf[5];
    if (code < 0x0080) {
        utf[0] = (unsigned char)code;
        utf[1] = 0;
        return utf;
    }
    if (code < 0x0800) {
        utf[0] = 0xc0 | (code >> 6);
        utf[1] = 0x80 | (code & 0x3f);
        utf[2] = 0;
        return utf;
    }
    if (code < 0x10000) {
        utf[0] = 0xe0 | (code >> 12);
        utf[1] = 0x80 | ((code >> 6) & 0x3f);
        utf[2] = 0x80 | (code & 0x3f);
        utf[3] = 0;
        return utf;
    }

    utf[0] = 0xF0 | (code >> 18);
    utf[1] = 0x80 | ((code >> 12) & 0x3f);
    utf[2] = 0x80 | ((code >> 6) & 0x3f);
    utf[3] = 0x80 | (code & 0x3f);
    utf[4] = 0;
    return utf;
}
string PyAsString(PyObject* po) {
    string s;
    Py_ssize_t sz = PyUnicode_GetSize(po);
    Py_UNICODE* ibuff = PyUnicode_AsUnicode(po);
    for (int i = 0; i < sz; i++)
        s += cs_unicode_to_utf8(ibuff[i]);
    return s;
}

static string python_error_string() {
    string err;
    PyObject *ptype = NULL;
    PyObject *pvalue = NULL;
    PyObject *ptraceback = NULL;
    PyErr_Fetch(&ptype, &pvalue, &ptraceback);
    if (pvalue != NULL) {
        PyObject* perr = PyObject_Str(pvalue);
        err = PyAsString(perr);
        Py_DECREF(perr);
        Py_XDECREF(pvalue);
    }
    if (ptype != NULL)
        Py_XDECREF(ptype);
    if (ptraceback != NULL)
        Py_XDECREF(ptraceback);

    return err;
}

PyObject* ConvertToPythonString(string s) {
    return PyUnicode_FromStringAndSize(STR(s), s.size());
}

int main(int argc, char *argv[]) {
    
    if (argc != 2) {
        cerr << "Fichier manquant" << std::endl;
        exit(-1);
    }
    
    string pathname = argv[1];
        
    /*
    FILE * fp;
    fp = fopen(STR(pathname), "rb");
    if (fp == NULL) {
        string msg="Error: '";
        msg += pathname;
        msg += "' is unknown";
        cerr << msg << std::endl;
        exit(-1);
    }

    fclose(fp);
     */
    
    Py_Initialize();
    
    char localpath[4096];
    string path = pathname.substr(0, pathname.rfind('/'));
    realpath(STR(path), localpath);
    cerr << localpath << endl;

    std::stringstream code;
    code << "import sys\n";
    code << "sys.path.append('" << localpath << "')\n";

    PyRun_SimpleString(code.str().c_str());
    if (PyErr_Occurred()) {
        string err = "Error: ";
        err += python_error_string();
        cerr << err << std::endl;
        exit(-1);
    }

    //PyRun_SimpleFile(fp, STR(pathname));
    
    //PyRun_InteractiveLoop(stdin, "stdin");
    realpath(STR(pathname), localpath);
    cerr << localpath << endl;

    PyObject* pName = ConvertToPythonString(localpath);
    PyImport_Import(pName);
    
    if (PyErr_Occurred()) {
        string err = "Error: ";
        err += python_error_string();
        cerr << err << std::endl;
    }
    
    Py_Finalize();
}
    
