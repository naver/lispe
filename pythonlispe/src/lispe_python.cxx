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

string PyAsString(PyObject* po) {
    string s;
    Py_ssize_t sz = PyUnicode_GetSize(po);
    Py_UNICODE* ibuff = PyUnicode_AsUnicode(po);
    for (int i = 0; i < sz; i++)
        s += cs_unicode_to_utf8(ibuff[i]);
    return s;
}

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
            if (!lisp->handlingutf8->est_une_lettre(USTR(s), i))
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
        wstring kval;
        try {
            while (PyDict_Next(po, &pos, &key, &pelement)) {
                if (key != NULL && pelement != NULL) {
                    k = toLispE(lisp, key);
                    e = toLispE(lisp, pelement);
                    kval = k->asString(lisp);
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
    #else
    if (PyUnicode_Check(po)) {
        Py_ssize_t sz = PyUnicode_GetSize(po);
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
        wstring wkey;
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
    if (!nbelements) {
        PyErr_SetString(PyExc_AttributeError, "Expecting some parameters");
        return ConvertToPythonLong(-1);
    }

    PyObject* pelement = PyTuple_GetItem(args, 0);
    if (!PyCheck_String(pelement)) {
        PyErr_SetString(PyExc_AttributeError, "Expecting a string");
        return ConvertToPythonLong(-2);
    }

    //The first element is an atom...
    List code;
    code.append(toLispE(self->lisp, pelement));

    Element* e;
    for (int i = 1; i < nbelements; i++) {
        pelement = PyTuple_GetItem(args, i);
        e = toLispE(self->lisp, pelement);
        code.append(e);
    }

    e = code.eval(self->lisp);
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

class Pythoninterpreter : public Element {
public:

    string pythonfilename;
    PyObject* pModule;
    PyObject* pDict;
    bool init_python;
    
    Pythoninterpreter(short ty) : Element(ty) {
        pModule = NULL;
        pDict = NULL;
        init_python = false;
    }
    
    ~Pythoninterpreter() {
        if (init_python)
            Py_Finalize();
    }

    Element* methodSetpath(LispE* lisp, string& path) {
        lisp->lock();
        if (!init_python) {
            Py_Initialize();
            init_python = true;
        }
        
        stringstream code;
        code << "import sys\n";
#ifdef WIN32
        string local("\\");
        string with("\\\\");
        path = s_replacestring(path, local, with);
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
    
    Element* methodRun(LispE* lisp, string& code) {
        
        lisp->lock();
        if (!init_python) {
            Py_Initialize();
            init_python = true;
            pModule = PyImport_AddModule("__main__");
            pDict = PyModule_GetDict(pModule);
        }

        //0 is the first parameter and so on...
        if (code != "") {
            if (pDict == NULL)
                pDict = PyModule_GetDict(pModule);
            
            PyObject* pstr  = PyRun_String(STR(code), Py_single_input, pDict, pDict);
            if (PyErr_Occurred()) {
                string err = "Error: PYT(997):";
                err += python_error_string();
                lisp->unlock();
                throw new Error(err);
            }
            
            lisp->unlock();
            Element* e = toLispE(lisp, pstr);
            Py_DECREF(pstr);
            return e;
        }
        
        lisp->unlock();
        //you may return any value of course...
        return true_;
    }

    Element* methodImport(LispE* lisp, string& pythonfilename) {
        if (pModule != NULL)
            throw new Error("Error: PYT(020): Module already imported");

        lisp->lock();

        if (!init_python) {
            Py_Initialize();
            init_python = true;
        }

        //0 is the first parameter and so on...
        if (pythonfilename != "") {
            PyObject* pName = ConvertToPythonString(pythonfilename.c_str());
            pModule = PyImport_Import(pName);
            Py_DECREF(pName);
        }

        if (PyErr_Occurred()) {
            string err = "Error: PYT(998):";
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

typedef enum {python_new, python_run, python_setpath, python_import, python_execute, python_close} pythonery;

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
        switch (action) {
            case python_new:
                return new Pythoninterpreter(python_type);
            case python_run: {
                Element* py = lisp->get(py_var);
                if (py->type != python_type)
                    throw new Error("Error: first argument should be a 'python' object");
                string code = lisp->get(L"code")->toString(lisp);
                return ((Pythoninterpreter*)py)->methodRun(lisp, code);
            }
            case python_setpath:{
                Element* py = lisp->get(py_var);
                if (py->type != python_type)
                    throw new Error("Error: first argument should be a 'python' object");
                string path = lisp->get(L"path")->toString(lisp);
                return ((Pythoninterpreter*)py)->methodSetpath(lisp, path);
            }
            case python_import:{
                Element* py = lisp->get(py_var);
                if (py->type != python_type)
                    throw new Error("Error: first argument should be a 'python' object");
                string path = lisp->get(L"path")->toString(lisp);
                return ((Pythoninterpreter*)py)->methodImport(lisp, path);
            }
            case python_execute:{
                Element* py = lisp->get(py_var);
                if (py->type != python_type)
                    throw new Error("Error: first argument should be a 'python' object");
                Element* args = lisp->get(L"arguments");
                if (!args->isList())
                    throw new Error("Error: arguments should be a list");
                
                string funcname = lisp->get(L"name")->toString(lisp);
                return ((Pythoninterpreter*)py)->methodExecute(lisp, funcname, args);
            }
                
            case python_close:{
                Element* py = lisp->get(py_var);
                if (py->type != python_type)
                    throw new Error("Error: first argument should be a 'python' object");
                return ((Pythoninterpreter*)py)->methodClose(lisp);
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
            case python_setpath:
                return L"Call Python 'setpath' commands with the given path";
            case python_import:
                return L"Import some Python library or some python code";
            case python_execute:
                return L"Execute a function that was imported with the arguments as a list";
            case python_close:
                return L"Close the Python interpreter";
        }
        return L"";
    }

};


extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {
    wstring w = L"_python";
    short type_python = lisp->encode(w);
    lisp->extension("deflib python()", new Pythonmethod(lisp, python_new, type_python));
    lisp->extension("deflib python_run(py code)", new Pythonmethod(lisp, python_run, type_python));
    lisp->extension("deflib python_setpath(py path)", new Pythonmethod(lisp, python_setpath, type_python));
    lisp->extension("deflib python_import(py path)", new Pythonmethod(lisp, python_import, type_python));
    lisp->extension("deflib python_execute(py name arguments)", new Pythonmethod(lisp, python_execute, type_python));
    lisp->extension("deflib python_close(py)", new Pythonmethod(lisp, python_close, type_python));
    return true;
}

}
