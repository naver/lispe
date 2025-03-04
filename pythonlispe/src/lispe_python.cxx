/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  lispe_python.cxx


/*
 This the template for new 'extensions' in lispe
 */

#include "lispe.h"

#ifdef WIN32
#if (_MSC_VER >= 1900)
#pragma comment(lib, "legacy_stdio_definitions.lib")
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif
#endif

#include <Python.h>
#include <structmember.h>


#if PY_MAJOR_VERSION >= 3
#define PyCheck_String PyUnicode_Check
#define ConvertToPythonLong(i)    PyLong_FromLong(i)
    #define PyCheck_Number    PyLong_Check
#define PyAsNumber    PyLong_AsLong

PyObject* ConvertToPythonString(string s) {
    return PyUnicode_FromStringAndSize(STR(s), s.size());
}

#if PY_MINOR_VERSION >= 9
string PyAsString(PyObject* po) {
	PyObject* utf8_string = PyUnicode_AsUTF8String(po);
	char* c_string = PyBytes_AsString(utf8_string);
	return c_string;
}
#else
string PyAsString(PyObject* po) {
    string s;
    Py_ssize_t sz = PyUnicode_GET_LENGTH(po);
    Py_UNICODE* ibuff = PyUnicode_AsUnicode(po);
    for (int i = 0; i < sz; i++)
        s += cs_unicode_to_utf8(ibuff[i]);
    return s;
}
#endif
#else
#define ConvertToPythonString(s)  PyString_FromString(s)
    #define ConvertToPythonLong(i)    PyInt_FromLong(i)
        #define PyCheck_Number    PyInt_Check
#define PyAsNumber PyInt_AsLong
#define PyAsString PyString_AsString
#define PyCheck_String PyString_Check

#if PY_MINOR_VERSION==4
#define Py_ssize_t int
#define OLDPYTHON 1
#endif

#endif

#ifdef UNIX
#include <signal.h>
#endif

void clean_signal() {
#ifndef WIN32
    signal(SIGINT,NULL);
#endif
}

static Element* toLispE(LispE* lisp, PyObject* po) {
    if (po == Py_None || PyBool_Check(po) == 1) {
        if (po == Py_True)
            return true_;
        return null_;
    }

    if (PyCheck_Number(po) == 1 || PyLong_Check(po) == 1) {
        long l = PyLong_AsLong(po);
        return lisp->provideInteger(l);
    }

    if (PyFloat_Check(po) == 1) {
        double d = PyFloat_AsDouble(po);
        return lisp->provideNumber(d);
    }

    //A string should contain the double quotes within
    //or it might contain non alphabetical characters...
    if (PyCheck_String(po) == 1) {
        string s = PyAsString(po);
        if (s[0] == '"' && s.back() == '"') {
            s = s.substr(1, s.size()-2);
            return lisp->provideString(s);
        }
        short code = lisp->is_atom(s);
        if (code != -1)
            return lisp->provideAtomOrInstruction(code);
        for (long i = 0; i < s.size(); i++) {
            if (!lisp->handlingutf8->is_a_valid_letter(USTR(s), i))
                return lisp->provideString(s);
        }
        return lisp->provideAtomOrInstruction(s);
    }

    PyObject* pelement;
    Element* e;
    if (PyList_Check(po) == 1) {
        List* kvect = new List;
        try {
            Py_ssize_t tail = PyList_GET_SIZE(po);
            for (Py_ssize_t i = 0; i < tail; i++) {
                //Puis chacun des objets, kj est l'index de l'element
                pelement = PyList_GetItem(po, i);
                e = toLispE(lisp, pelement);
                kvect->append(e);
            }
        }
        catch (Error* err) {
            kvect->release();
            throw err;
        }
        return kvect;
    }

    if (PyDict_Check(po) == 1) {
        Dictionary* kmap = new Dictionary;
        Py_ssize_t pos = 0;
        Element* k = null_;
        PyObject* key;
        u_ustring kval;
        try {
            while (PyDict_Next(po, &pos, &key, &pelement)) {
                if (key != NULL && pelement != NULL) {
                    k = toLispE(lisp, key);
                    e = toLispE(lisp, pelement);
                    kval = k->asUString(lisp);
                    kmap->recording(kval, e);
                    k->release();
                }
            }
        }
        catch(Error* err) {
            k->release();
            kmap->release();
        }
        return kmap;
    }

    #ifdef OLDPYTHON
    if (PyUnicode_Check(po)) {
        Py_ssize_t sz = PyUnicode_GET_DATA_SIZE(po);
        const char* sbuff = PyUnicode_AS_DATA(po);
        string s;
        for (Py_ssize_t i = 0; i < sz; i++) {
            if (sbuff[i])
                s += sbuff[i];
        }
        return lisp->provideString(s);
    }
    #elif PY_MINOR_VERSION >= 9
	if (PyUnicode_Check(po)) {
		string s;
		PyObject* utf8_string = PyUnicode_AsUTF8String(po);
		char* c_string = PyBytes_AsString(utf8_string);
		s = c_string;
		return lisp->provideString(s);
	}
	#else
    if (PyUnicode_Check(po)) {
        Py_ssize_t sz = PyUnicode_GET_LENGTH(po);
        Py_UNICODE* ibuff = PyUnicode_AsUnicode(po);
        string s;
        for (int i = 0; i < sz; i++)
            s += cs_unicode_to_utf8(ibuff[i]);
        return lisp->provideString(s);
    }
	#endif

    PyObject* perr = PyObject_Str(po);
    if (perr != NULL) {
        string s = PyAsString(perr);
        e = lisp->provideString(s);
        Py_DECREF(perr);
        return e;
    }

    return error_;
}

static PyObject* toPython(LispE* lisp, Element* resultat) {

    if (resultat == true_)
        Py_RETURN_TRUE;
    if (resultat == null_)
        Py_RETURN_NONE;


    PyObject* pcourant;

    long i;
    if (resultat->type == t_llist) {
        PyObject* vect = PyList_New(0);
        u_link* u = ((LList*)resultat)->liste.begin();
		i = 0;
        for (; u != NULL; u = u->next()) {
            pcourant = toPython(lisp, u->value);
            PyList_Insert(vect, i, pcourant);
			i++;
            Py_DECREF(pcourant);
        }
        return vect;
    }
    
    if (resultat->isList()) {
        long sz = resultat->size();
        PyObject* vect = PyList_New(0);
        Element* val;
        for (i = 0; i < sz; i++) {
            val = resultat->index(i);
            pcourant = toPython(lisp, val);
            val->release();
            PyList_Insert(vect, i, pcourant);
            Py_DECREF(pcourant);
        }
        return vect;
    }

    if (resultat->type == t_dictionary) {
        PyObject* dico = PyDict_New();
        Dictionary* d = (Dictionary*)resultat;
        u_ustring wkey;
        string key;
        for (auto& a: d->dictionary) {
            pcourant = toPython(lisp, a.second);
            wkey = a.first;
            s_unicode_to_utf8(key, wkey);
            PyDict_SetItemString(dico, STR(key), pcourant);
            Py_DECREF(pcourant);
        }
        return dico;
    }

    if (resultat->type == t_dictionaryn) {
        PyObject* dico = PyDict_New();
        Dictionary_n* d = (Dictionary_n*)resultat;
        PyObject* key;
        for (auto& a: d->dictionary) {
            pcourant = toPython(lisp, a.second);
            key = PyFloat_FromDouble(a.first);
            PyDict_SetItem(dico, key, pcourant);
            Py_DECREF(key);
            Py_DECREF(pcourant);
        }
        return dico;
    }


    if (resultat->label() == t_integer) {
        long l = resultat->asInteger();
        return ConvertToPythonLong(l);
    }

    if (resultat->label() == t_number) {
        double d = resultat->asNumber();
        return PyFloat_FromDouble(d);
    }

    string value = resultat->toString(lisp);
    #ifdef OLDPYTHON
    return PyUnicode_DecodeUTF8(value.c_str(), value.size(), NULL);
    #else
    return PyUnicode_FromString(value.c_str());
    #endif
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

extern "C" {
typedef struct {
    PyObject_HEAD
    LispE* lisp;
} LispEObject;

static void LispE_dealloc(LispEObject* self)
{
    delete self->lisp;
    Py_TYPE(self)->tp_free((PyObject*)self);
}

static PyObject * LispE_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    LispEObject *self;
    self = (LispEObject *)type->tp_alloc(type, 0);
    if (self == NULL) {
        return NULL;
    }

    return (PyObject *)self;
}

static int LispE_init(LispEObject *self, PyObject *args, PyObject *kwds)
{

    self->lisp = new LispE;
    return 0;
}
static PyObject* LispE_load(LispEObject* self, PyObject* args)
{
    int nbelements = PyTuple_Size(args);
    if (nbelements != 1) {
        PyErr_SetString(PyExc_AttributeError, "Wrong parameter, expecting a pathname");
        return ConvertToPythonLong(-1);
    }

    PyObject* pelement = PyTuple_GetItem(args, 0);
    if (!PyCheck_String(pelement)) {
        PyErr_SetString(PyExc_AttributeError, "Expecting a string");
        return ConvertToPythonLong(-4);
    }

    string pathname = PyAsString(pelement);
    Element* e = self->lisp->load(pathname);
    pelement = toPython(self->lisp, e);
    e->release();
    return pelement;
}

static PyObject* LispE_eval(LispEObject* self, PyObject* args)
{
    int nbelements = PyTuple_Size(args);
    if (nbelements != 1) {
        PyErr_SetString(PyExc_AttributeError, "Wrong parameter, expecting a string");
        return ConvertToPythonLong(-1);
    }


    PyObject* pelement = PyTuple_GetItem(args, 0);
    if (!PyCheck_String(pelement)) {
        PyErr_SetString(PyExc_AttributeError, "Expecting a string");
        return ConvertToPythonLong(-4);
    }

    string code = PyAsString(pelement);
    Element* e = self->lisp->execute(code);
    pelement = toPython(self->lisp, e);
    e->release();
    return pelement;
}

static PyObject* LispE_execute(LispEObject* self, PyObject* args)
{
    int nbelements = PyTuple_Size(args);
    if (nbelements < 2) {
        PyErr_SetString(PyExc_AttributeError, "Expecting some parameters");
        return ConvertToPythonLong(-1);
    }

    PyObject* pelement = PyTuple_GetItem(args, 0);
    if (!PyCheck_String(pelement)) {
        PyErr_SetString(PyExc_AttributeError, "Expecting a string");
        return ConvertToPythonLong(-2);
    }

    //The first element is an atom...
    Listincode code;
    string function_code = PyAsString(pelement);
    code.append(self->lisp->provideAtom(self->lisp->encode(function_code)));

    Element* e;
    try {
        for (int i = 1; i < nbelements; i++) {
            pelement = PyTuple_GetItem(args, i);
            e = toLispE(self->lisp, pelement);
            code.append(e);
        }
        e = code.eval(self->lisp);
    }
    catch(Error* err) {
        e = err;
    }
    
    pelement = toPython(self->lisp, e);
    e->release();
    return pelement;
}


static PyMethodDef LispE_methods[] = {
    {"load", (PyCFunction)LispE_load, METH_VARARGS,"Load a LispE program"},
    {"eval", (PyCFunction)LispE_eval, METH_VARARGS,"Evaluate a LispE program"},
    {"execute", (PyCFunction)LispE_execute, METH_VARARGS,"Execute a list of instructions"},
    {NULL}  /* Sentinel */
};

static PyTypeObject LispEType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "pylispe.lisp",             /* tp_name */
    sizeof(LispEObject),             /* tp_basicsize */
    0,                         /* tp_itemsize */
    (destructor)LispE_dealloc, /* tp_dealloc */
    0,                         /* tp_print */
    0,                         /* tp_getattr */
    0,                         /* tp_setattr */
    0,                         /* tp_reserved */
    0,                         /* tp_repr */
    0,                         /* tp_as_number */
    0,                         /* tp_as_sequence */
    0,                         /* tp_as_mapping */
    0,                         /* tp_hash  */
    0,                         /* tp_call */
    0,                         /* tp_str */
    0,                         /* tp_getattro */
    0,                         /* tp_setattro */
    0,                         /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT |
        Py_TPFLAGS_BASETYPE,   /* tp_flags */
    "LispE objects",           /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,                         /* tp_richcompare */
    0,                         /* tp_weaklistoffset */
    0,                         /* tp_iter */
    0,                         /* tp_iternext */
    LispE_methods,             /* tp_methods */
    0,             /* tp_members */
    0,                         /* tp_getset */
    0,                         /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    (initproc)LispE_init,      /* tp_init */
    0,                         /* tp_alloc */
    LispE_new,                 /* tp_new */
};

static struct PyModuleDef pylispemodule = {
    PyModuleDef_HEAD_INIT,
    "pylispe",
    "LispE module.",
    -1,
    NULL, NULL, NULL, NULL, NULL
};

static PyObject* pylispeError;

PyMODINIT_FUNC PyInit_pylispe(void)
{

    PyObject* m;

    LispEType.tp_new = PyType_GenericNew;
    if (PyType_Ready(&LispEType) < 0)
        return NULL;

    m = PyModule_Create(&pylispemodule);
    if (m == NULL)
        return NULL;

    Py_INCREF(&LispEType);
    PyModule_AddObject(m, "lisp", (PyObject *)&LispEType);

    pylispeError = PyErr_NewException("pylispe.error", NULL, NULL);
    Py_INCREF(pylispeError);
    PyModule_AddObject(m, "error", pylispeError);

    return m;
}
}

//https://docs.python.org/3.5/extending/newtypes.html for information

/*
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
Calling Python from within LispE
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
*/

//To avoid issue with some specific libs
static int LispEHook(void) {
    return 0;
}

class Pythoninterpreter : public Element {
public:

    string pythonfilename;
    PyObject* pModule;
    PyObject* pDict;
    PyObject* pLocalDict;
    bool init_python;
    std::unordered_map<string, PyObject*> dictionaries;

    Pythoninterpreter(short ty) : Element(ty) {
        pModule = NULL;
        pDict = NULL;
        pLocalDict = NULL;
        init_python = false;
    }

    ~Pythoninterpreter() {
        if (init_python)
            Py_Finalize();
    }

    void initialize() {
        if (!init_python) {
            Py_Initialize();
            init_python = true;
        }
    }
    
    Element* methodSetpath(LispE* lisp, string& path) {
        lisp->lock();
        initialize();

        stringstream code;
        code << "import sys\n";
#ifdef WIN32
        string local("\\");
        string with("\\\\");
        path = s_replacingstring(path, local, with);
        code << "sys.path.append('" << path << "')\n";
#else
        code << "sys.path.append('" << path << "')\n";
#endif

        PyRun_SimpleString(code.str().c_str());

        if (PyErr_Occurred()) {
            string err = "Error: PYT(996):";
            err += python_error_string();
            lisp->unlock();
            throw new Error(err);
        }
        //you may return any value of course...
        lisp->unlock();
        return true_;
    }

    Element* methodSimpleString(LispE* lisp, string& code) {
        lisp->lock();
        initialize();

        PyRun_SimpleString(STR(code));

        if (PyErr_Occurred()) {
            string err = "Error: PYT(996):";
            err += python_error_string();
            lisp->unlock();
            throw new Error(err);
        }
        
        //you may return any value of course...
        lisp->unlock();
        return true_;
    }

    Element* methodClose(LispE* lisp) {
        if (init_python == false)
            return null_;
        lisp->lock();
        Py_Finalize();
        lisp->unlock();
        init_python = false;
        pModule = NULL;
        return true_;
    }

    Element* Run_simple(LispE* lisp, string& code, PyObject* py_dict)
    {

        PyObject *py_result = PyRun_StringFlags(code.c_str(), Py_file_input, py_dict, py_dict, nullptr);

        if (!py_result)
        {
            if (PyErr_Occurred())
            {
                string return_variable = "PYT(997):";
                return_variable += python_error_string();
                // Imprime l'erreur ou la traite selon vos besoins.
                throw new Error(return_variable);
            }
        }
        else
        {
            Py_DECREF(py_result); // Nettoie le résultat si rien ne s'est mal passé
        }
        return true_;
    }

    // Function to run the Python script in a separate thread
    Element* Run_elapse(LispE* lisp, string& code, int elapse_time, PyObject* py_dict) {
        static char* import_code = "import signal\nimport time\n";
        static char* func_code = "def timeout_handler(signum, frame):\n  signal.signal(signal.SIGALRM, signal.SIG_DFL)\n  signal.alarm(0)\n  raise TimeoutError(\"Time out\")\n";
        static char* init_code = "signal.signal(signal.SIGALRM, timeout_handler)\nsignal.alarm(";
        const char* clean_code = "signal.signal(signal.SIGALRM, signal.SIG_DFL)\nsignal.alarm(0)\n";
        
        stringstream c;
        c << import_code << func_code << init_code << elapse_time << ")\n";
        string cde = c.str();
        PyRun_StringFlags(cde.c_str(), Py_file_input, py_dict, py_dict, nullptr);

        PyObject* py_result = PyRun_StringFlags(code.c_str(), Py_file_input, py_dict, py_dict, nullptr);

        if (!py_result) {
            if (PyErr_Occurred()) {
                cde = "PYT(997):";
                cde += python_error_string();
                cerr << cde << endl;
                if (cde.find("Time out") == -1)
                    PyRun_StringFlags(clean_code, Py_file_input, py_dict, py_dict, nullptr);
                // Handle the error as needed
                throw new Error(cde);
            }
        } else {
            Py_DECREF(py_result); // Clean up the result if nothing went wrong
        }

        //we clean our signal before leaving
        PyRun_StringFlags(clean_code, Py_file_input, py_dict, py_dict, nullptr);
        return true_;
    }

    Element* methodRun(LispE* lisp, string& code, string& return_variable, double elapse_time) {

        lisp->lock();
        if (!init_python) {
            initialize();
            pModule = PyImport_AddModule("__main__");
            pDict = PyModule_GetDict(pModule);
            pLocalDict = PyDict_New();
        }

        //0 is the first parameter and so on...
        if (code != "") {
            Element* kcmd;
            try {
                //First, we test if the syntax is correct:
                PyObject *compiled_code = Py_CompileString(code.c_str(), "<string>", Py_file_input);
                if (compiled_code == NULL) {
                    return_variable = "PYT(996):";
                    return_variable += python_error_string();
                    // Imprime l'erreur ou la traite selon vos besoins.
                    throw new Error(return_variable);
                } else {
                    Py_DECREF(compiled_code);
                    if (elapse_time == -1)
                        kcmd = Run_simple(lisp, code, pDict);
                    else {
        #ifndef WIN32
                        struct sigaction old_action;
                        sigaction(SIGALRM, nullptr, &old_action);
                        kcmd = Run_elapse(lisp, code, elapse_time, pDict);
                        sigaction(SIGALRM, &old_action, nullptr);
        #else
                        kcmd = Run_elapse(lisp, code, elapse_time, pDict);
        #endif
                    }
                }
            }
            catch(Error* e) {
                clean_signal();
                //methodClose(lisp);
                lisp->unlock();
                throw e;
            }
        }
        Element* returning_value = true_;
        if (return_variable != "") {
            PyObject* py_result_val = PyDict_GetItemString(pDict, return_variable.c_str());
            if (py_result_val != NULL) {
                returning_value = toLispE(lisp, py_result_val);
            } else {
                return_variable += ": This Python variable is unknown";
                throw new Error(return_variable);
            }
        }

        //methodClose(lisp);
        clean_signal();
        lisp->unlock();

        // You may return any value of course...
        return returning_value;
    }

    Element* methodRunFile(LispE* lisp, string& pathname) {

        lisp->lock();
        initialize();
        
        //0 is the first parameter and so on...
        if (pathname != "") {
            FILE * fp;
            fp = fopen(STR(pathname), "rb");
            if (fp == NULL) {
                string msg="Error: '";
                msg += pathname;
                msg += "' is unknown";
                throw new Error(msg);
            }

            PyRun_SimpleFile(fp, STR(pathname));

            if (PyErr_Occurred()) {
                string err = "Error: PYT(997):";
                err += python_error_string();
                lisp->unlock();
                throw new Error(err);
            }

            lisp->unlock();
            return true_;
        }

        lisp->unlock();
        //you may return any value of course...
        return true_;
    }

    Element* methodRunModule(LispE* lisp, string& name, string& code, string& return_variable)
    {

        lisp->lock();
        if (!init_python)
        {
            initialize();
        }

        // 0 is the first parameter and so on...
        PyObject *py_main = PyImport_AddModule("__main__");
        PyObject *py_dict = PyModule_GetDict(py_main);

        PyObject *local_dict = NULL;

        if (code != "")
        {
            try {
                PyObject *compiled_code = Py_CompileString(code.c_str(), "<string>", Py_file_input);            
                if (compiled_code == NULL) {
                    return_variable = "PYT(996):";
                    return_variable += python_error_string();
                    // Imprime l'erreur ou la traite selon vos besoins.
                    throw new Error(return_variable);
                }
                Py_DECREF(compiled_code);

                local_dict = PyDict_New();
                PyObject *py_result = PyRun_StringFlags(code.c_str(), Py_file_input, py_dict, local_dict, nullptr);

                if (!py_result)
                {
                    if (PyErr_Occurred())
                    {
                        Py_DECREF(local_dict);
                        return_variable = "PYT(997):";
                        return_variable += python_error_string();
                        // Imprime l'erreur ou la traite selon vos besoins.
                        throw new Error(return_variable);
                    }
                }
                else
                {
                    Py_DECREF(py_result); // Nettoie le résultat si rien ne s'est mal passé
                }
                dictionaries[name] = local_dict;
            }
            catch (Error* err) {
                lisp->unlock();
                clean_signal();                
                throw err;
            }
        }

        Element* return_value = true_;
        if (return_variable != "") {
            
            PyObject* py_result_val = PyDict_GetItemString(local_dict, return_variable.c_str());
            if (py_result_val != NULL) {
                return_value = toLispE(lisp, py_result_val);
            }
            else {
                return_variable += ": This Python variable is unknown";
                lisp->unlock();
                clean_signal();
                throw new Error(return_variable);
            }
        }

        lisp->unlock();
        clean_signal();
        // you may return any value of course...
        return return_value;
    }

    Element* methodGetModule(LispE* lisp, string& module_name, string& return_variable)
    {        
        if (!init_python)    
        {
            throw new Error("No Python interpreter has been initialized");
        }

        lisp->lock();
        // 0 is the first parameter and so on...
        if (dictionaries.find(module_name) == dictionaries.end()) {
            module_name += "is unknown";
            lisp->unlock();
            throw new Error(module_name);
        }

        PyObject *local_dict = dictionaries[module_name];
        Element *tmg;

        if (return_variable == "")
        {
            Dictionary* variables = new Dictionary();
            // Access local variables in local_dict
            PyObject *local_keys = PyDict_Keys(local_dict);
            Py_ssize_t num_locals = PyList_Size(local_keys);
            for (Py_ssize_t i = 0; i < num_locals; i++)
            {
                PyObject *key = PyList_GetItem(local_keys, i);     // Get each variable name (key)
                PyObject *value = PyDict_GetItem(local_dict, key); // Get the corresponding value

                // Convert the key (variable name) to a string and print the variable name and value
                const char *variable_name = PyUnicode_AsUTF8(key);

                if (variable_name != NULL && value != NULL)
                {
                    module_name = variable_name;
                    tmg = toLispE(lisp, value);
                    variables->recording(module_name, tmg);
                }
            }

            Py_DECREF(local_keys); // Clean up the keys list

            clean_signal();
            lisp->unlock();
            return variables;
        }
        else
        {
            PyObject *py_result_val = PyDict_GetItemString(local_dict, return_variable.c_str());
            if (py_result_val != NULL)
            {
                lisp->unlock();
                return toLispE(lisp, py_result_val);
            }
        }

        lisp->unlock();
        return_variable += ": This Python variable is unknown";
        throw new Error(return_variable);
    }

    Element* methodImport(LispE* lisp, string& pythonfilename) {
        lisp->lock();

        initialize();
        
        PyObject* module = NULL;

        //0 is the first parameter and so on...
        if (pythonfilename != "") {
            PyObject* pName = ConvertToPythonString(pythonfilename.c_str());
            module = PyImport_Import(pName);
            if (module != NULL && pModule == NULL) {
                pModule = module;
                pDict = PyModule_GetDict(pModule);
            }
            Py_DECREF(pName);
        }

        if (PyErr_Occurred() || module == NULL) {
            string err = "Error Python:  in '";
            err += pythonfilename;
            err += "': ";
            err += python_error_string();
            lisp->unlock();
            throw new Error(err);
        }

        lisp->unlock();
        //you may return any value of course...
        return true_;
    }

    Element* methodExecute(LispE* lisp, string& funcname, Element* arguments) {
        if (pModule == NULL)
            throw new Error("Error: PYT(002): No Python file in memory");

        lisp->lock();

        PyObject* pFunc = PyObject_GetAttrString(pModule, STR(funcname));
        if (pFunc == NULL || !PyCallable_Check(pFunc)) {
            lisp->unlock();
            throw new Error("Error: PYT(004): Unknown Python function");
        }

        long nbelements = arguments->size();
        PyObject* pArgs = PyTuple_New(nbelements);
        PyObject* pe;
        for (long i = 0; i < nbelements; i++) {
            pe = toPython(lisp, arguments->index(i));
            PyTuple_SetItem(pArgs, i, pe);
        }

        pe = PyObject_CallObject(pFunc, pArgs);
        Py_DECREF(pArgs);

        if (PyErr_Occurred()) {
            if (pe != NULL)
                Py_DECREF(pe);
            string err = "Error: PYT(999):";
            err += python_error_string();
            lisp->unlock();
            throw new Error(err);
        }

        if (pe != NULL) {
            Element* o = toLispE(lisp, pe);
            Py_DECREF(pe);
            lisp->unlock();
            return o;
        }

        lisp->unlock();
        return true_;
    }

};

typedef enum {python_new, python_run, python_runfile, python_setpath, python_import, python_execute, python_simplestring, python_close, python_runmodule, python_getmodule} pythonery;

class Pythonmethod : public Element {
public:
    pythonery action;
    short py_var;
    short python_type;

    Pythonmethod(LispE* lisp, pythonery p, short ty) : Element(ty) {
        action = p;
        wstring w = L"py";
        py_var = lisp->encode(w);
        w = L"python";
        python_type = lisp->encode(w);
    }

    Element* eval(LispE* lisp) {
        Pythoninterpreter* py = NULL;
        if (action != python_new) {
            Element* e = lisp->get_variable(py_var);
            if (e->type != python_type)
                throw new Error("Error: first argument should be a 'python' object");
            py = (Pythoninterpreter*)e;
        }
        switch (action) {
            case python_new:
                return new Pythoninterpreter(python_type);
            case python_run: {
                string code = lisp->get_variable(U"code")->toString(lisp);
                double timeout = lisp->get_variable(U"timeout")->asNumber();
                Element* ret = lisp->get_variable(U"variable");
                string returnvariable;
                if (ret != null_)
                    returnvariable = ret->toString(lisp);
                return py->methodRun(lisp, code, returnvariable, timeout);
            }
            case python_runfile: {
                string path = lisp->get_variable(U"path")->toString(lisp);
                return py->methodRunFile(lisp, path);
            }
            case python_runmodule: {
                string name = lisp->get_variable(U"name")->toString(lisp);
                string code = lisp->get_variable(U"code")->toString(lisp);
                Element* ret = lisp->get_variable(U"variable");
                string returnvariable;
                if (ret != null_)
                    returnvariable = ret->toString(lisp);
                return py->methodRunModule(lisp, name, code, returnvariable);
            }
            case python_getmodule: {
                string name = lisp->get_variable(U"name")->toString(lisp);
                Element* ret = lisp->get_variable(U"variable");
                string returnvariable;
                if (ret != null_)
                    returnvariable = ret->toString(lisp);
                return py->methodGetModule(lisp, name, returnvariable);
            }
            case python_setpath:{
                string path = lisp->get_variable(U"path")->toString(lisp);
                return py->methodSetpath(lisp, path);
            }
            case python_import:{
                string path = lisp->get_variable(U"path")->toString(lisp);
                return py->methodImport(lisp, path);
            }
            case python_execute:{
                Element* args = lisp->get_variable(U"arguments");
                if (!args->isList())
                    throw new Error("Error: arguments should be a list");
                string funcname = lisp->get_variable(U"name")->toString(lisp);
                return py->methodExecute(lisp, funcname, args);
            }
            case python_simplestring: {
                string code = lisp->get_variable(U"code")->toString(lisp);
                return py->methodSimpleString(lisp, code);
            }
            case python_close:{
                return py->methodClose(lisp);
            }
        }
        return null_;
    }


    wstring asString(LispE* lisp) {
        switch (action) {
            case python_new:
                return L"Create a new Python interpreter";
            case python_run:
                return L"Execute some code with the Python interpreter";
            case python_runfile:
                return L"Execute a Python script";
            case python_setpath:
                return L"Call Python 'setpath' commands with the given path";
            case python_import:
                return L"Import some Python library or some python code";
            case python_execute:
                return L"Execute a function that was imported with the arguments as a list";
            case python_simplestring:
                return L"Execute a piece of code";
            case python_close:
                return L"Close the Python interpreter";
        }
        return L"";
    }


};


extern "C" {
    Exporting bool InitialisationModule(LispE* lisp) {
        wstring w = L"python_";
        short type_python = lisp->encode(w);
        lisp->extension("deflib python()", new Pythonmethod(lisp, python_new, type_python));
        lisp->extension("deflib python_run(py code (variable "") (timeout -1))", new Pythonmethod(lisp, python_run, type_python));
        lisp->extension("deflib python_runmodule(py name code (variable ""))", new Pythonmethod(lisp, python_runmodule, type_python));
        lisp->extension("deflib python_getmodule(py name (variable ""))", new Pythonmethod(lisp, python_getmodule, type_python));
        lisp->extension("deflib python_runfile(py path)", new Pythonmethod(lisp, python_runfile, type_python));
        lisp->extension("deflib python_setpath(py path)", new Pythonmethod(lisp, python_setpath, type_python));
        lisp->extension("deflib python_import(py path)", new Pythonmethod(lisp, python_import, type_python));
        lisp->extension("deflib python_execute(py name arguments)", new Pythonmethod(lisp, python_execute, type_python));
        lisp->extension("deflib python_simple(py code)", new Pythonmethod(lisp, python_simplestring, type_python));
        lisp->extension("deflib python_close(py)", new Pythonmethod(lisp, python_close, type_python));
        return true;
    }
}

