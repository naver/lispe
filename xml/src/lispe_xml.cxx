/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  lispe_xml.cxx
//


/*
 This the template for new 'extensions' in lispe
 */

#include "lispe.h"

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <list>

#ifdef WIN32
#if (_MSC_VER >= 1900)
#pragma comment(lib, "legacy_stdio_definitions.lib")
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif
#endif


typedef enum {xml_load, xml_parse, xml_next, xml_previous, xml_parent, xml_child, xml_root,
    xml_name, xml_properties, xml_line, xml_type, xml_content, xml_namespace, xml_nodeid, xml_setnodeid} xml_action;

void TraverseXML(xmlNodePtr n, string& contenu, bool first) {
    if (n == NULL)
        return;
    if (n->content != NULL)
        contenu += (char*)n->content;
    TraverseXML(n->children, contenu, false);
    if (!first)
        TraverseXML(n->next, contenu, false);
}

class Lispe_xmlnode;
class Lispe_xml;

class Lispe_xmldoc : public Element {
public:
    string name;
    xmlDoc* doc;
    LispE* lisp;
    short iddoc;

    Lispe_xmldoc(LispE* l, short i, short ty) : Element(ty) {
        iddoc = i;
        lisp = l;
    }

    Element* load(string& nm) {
        name = nm;
        doc = xmlParseFile(STR(name));
        if (doc == NULL)
            throw new Error("Error: Unknown file");
        return this;
    }

    Element* parse(string& buff) {
        doc = xmlSAXParseMemory(NULL, STR(buff), (int)buff.size(), 1);
        if (doc == NULL)
            throw new Error("Error: could not parse this string");
        return this;
    }
    wstring asString(LispE* lisp) {
        wstring nm;
        s_utf8_to_unicode(nm, USTR(name), name.size());
        return nm;
    }

    Element* root(short type_xmlnode);
    void clean();

    ~Lispe_xmldoc() {
        clean();
        if (doc != NULL)
            xmlFreeDoc(doc);
    }
};


class Lispe_xmlnode : public Element {
public:
    xmlNodePtr node;
    short iddoc;

    Lispe_xmlnode(short ty, short idd, xmlNodePtr n) : Element(ty) {
        node = n;
        iddoc = idd;
    }

    bool getDoc();

    Element* eval(LispE* lisp) {
        if (!getDoc() || node == NULL)
            return null_;
        return this;
    }

    bool Boolean() {
        return (getDoc());
    }

    double asNumber() {
        return (getDoc());
    }

    long asInteger() {
        return (getDoc());
    }

    Element* next(LispE* lisp) {
        if (!getDoc() || node == NULL || node->next == NULL)
            return null_;
        return new Lispe_xmlnode(type, iddoc,  node->next);
    }

    Element* previous(LispE* lisp) {
        if (!getDoc() || node == NULL || node->prev == NULL)
            return null_;
        return new Lispe_xmlnode(type, iddoc,  node->prev);
    }

    Element* child(LispE* lisp) {
        if (!getDoc() || node == NULL || node->children == NULL)
            return null_;
        return new Lispe_xmlnode(type, iddoc,  node->children);
    }

    Element* parent(LispE* lisp) {
        if (!getDoc() || node == NULL || node->parent == NULL || node->parent->type == XML_DOCUMENT_NODE || node->parent->name == NULL)
            return null_;
        return new Lispe_xmlnode(type, iddoc,  node->parent);
    }

    Element* name(LispE* lisp) {
        if (!getDoc() || node == NULL || node->name == NULL)
            return null_;
        string s = (char*)node->name;
        return lisp->provideString(s);
    }

    wstring asString(LispE* lisp) {
        if (!getDoc() || node == NULL || node->name == NULL)
            return L"";
        wstring n;
        string s = (char*)node->name;
        s_utf8_to_unicode(n, USTR(s), s.size());
        return n;
    }

    Element* line(LispE* lisp) {
        if (!getDoc() || node == NULL)
            return null_;

        return lisp->provideInteger(node->line);
    }

    Element* node_type(LispE* lisp) {
        if (!getDoc() || node == NULL)
            return null_;

        return lisp->provideInteger(node->type);
    }

    Element* node_id(LispE* lisp) {
        if (!getDoc() || node == NULL)
            return null_;

        return lisp->provideInteger((long)node->_private);
    }

    Element* set_node_id(LispE* lisp, long idx) {
        if (!getDoc() || node == NULL)
            return null_;
        node->_private = (void*)idx;
        return this;
    }

    Element* content(LispE* lisp) {
        if (!getDoc() || node == NULL)
            return null_;

        string content;
        TraverseXML(node, content, true);
        return lisp->provideString(content);
    }

    Element* name_space(LispE* lisp) {
        if (!getDoc() || node == NULL)
            return emptylist_;

        List* kvect = emptylist_;
        if (node->nsDef != NULL) {
            kvect = new List;
            xmlNsPtr cur = node->nsDef;
            string val;
            while (cur != NULL) {
                if (cur->href != NULL) {
                    val = (char*)cur->href;
                    kvect->append(lisp->provideString(val));
                }
                else
                    kvect->append(emptystring_);

                val = (char*)cur->prefix;
                kvect->append(lisp->provideString(val));
                cur = cur->next;
            }
        }
        return kvect;
    }

    Element* properties(LispE* lisp) {
        if (!getDoc() || node == NULL)
            return emptydictionary_;

        Dictionary* kmap = emptydictionary_;
        xmlAttrPtr propriete;
        if (node->properties != NULL) {
            kmap = new Dictionary;
            propriete = node->properties;
            string key;
            wstring wkey;
            string value;
            while (propriete != NULL) {
                key = (char*)propriete->name;
                value = (char*)propriete->children->content;
                wkey = L"";
                s_utf8_to_unicode(wkey, USTR(key), key.size());
                kmap->recording(key, lisp->provideString(value));
                propriete = propriete->next;
            }
        }
        return kmap;
    }
};


class Lispe_xml : public Element {
public:

    static Lispe_xmldoc* docs[1024];
    static short firstposition;
    static ThreadLock _lock;
    
    xml_action action;
    short type_xmlnode;
    short type_xmldoc;

    Lispe_xml(LispE* lisp, xml_action a, short identifier) : action(a), Element(identifier) {
        wstring w = L"xmlnode";
        type_xmlnode = lisp->encode(w);
        w = L"xmldoc";
        type_xmldoc = lisp->encode(w);
    }

    short first_empty_slot(bool tobelocked) {
        _lock.locking(tobelocked);
        long current = firstposition;
        while (firstposition < 1024 && docs[firstposition] != NULL)
            firstposition++;
        
        if (firstposition < 1024) {
            _lock.unlocking(tobelocked);
            return firstposition;
        }
        
        //We restart from 0
        if (current) {
            firstposition = 0;
            while (firstposition < 1024 && docs[firstposition] != NULL)
                firstposition++;
            
            if (firstposition < 1024) {
                _lock.unlocking(tobelocked);
                return firstposition;
            }
        }
        _lock.unlocking(tobelocked);
        return -1;
    }
    
    Element* eval(LispE* lisp) {
        //The name defined in the extension is not insignificant, it is used to retrieve our arguments.
        switch (action) {
            case xml_load: {
                short iddoc = first_empty_slot(lisp->checkforLock());
                if (iddoc == -1)
                    throw new Error("Error: XML table full");
                
                docs[iddoc] = new Lispe_xmldoc(lisp, iddoc, type_xmldoc);
                string pathname = lisp->get(L"pathname")->toString(lisp);
                return docs[iddoc]->load(pathname);
            }
            case xml_parse: {
                short iddoc = first_empty_slot(lisp->checkforLock());
                if (iddoc == -1)
                    throw new Error("Error: XML table full");
                
                docs[iddoc] = new Lispe_xmldoc(lisp, iddoc, type_xmldoc);
                string buffer = lisp->get(L"buffer")->toString(lisp);
                return docs[iddoc]->parse(buffer);
            }
            case xml_root: {
                //The first parameter should be a doc
                Element* doc = lisp->get(L"doc");
                if (doc->type != type_xmldoc)
                    throw new Error("Error: the first parameter should be an 'xmldoc'");
                return ((Lispe_xmldoc*)doc)->root(type_xmlnode);
            }
            case xml_next: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->next(lisp);
            }
            case xml_previous: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->previous(lisp);
            }
            case xml_child: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->child(lisp);
            }
            case xml_parent: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->parent(lisp);
            }
            case xml_name: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->name(lisp);
            }
            case xml_properties: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->properties(lisp);
            }
            case xml_line: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->line(lisp);
            }

            case xml_namespace: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->name_space(lisp);
            }
            case xml_type: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->node_type(lisp);
            }
            case xml_content: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->content(lisp);
            }
            case xml_nodeid: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->node_id(lisp);
            }
            case xml_setnodeid: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                long idx = lisp->get("id")->asInteger();
                return ((Lispe_xmlnode*)node)->set_node_id(lisp, idx);
            }
        }
    }

    //We use this instruction to return a description of the instruction
    //Indeed, just do: (print xml_example) to get this information
    wstring asString(LispE* lisp) {
        switch (action) {
            case xml_load:
                return L"Load an XML document";
            case xml_root:
                return L"Return the root node of the document";
            case xml_next:
                return L"Return the next node";
            case xml_previous:
                return L"Return the previous node";
            case xml_parent:
                return L"Return the parent node";
            case xml_child:
                return L"Return the first child node";
            case xml_name:
                return L"Return the node name";
            case xml_properties:
                return L"Return the node properties";
            case xml_line:
                return L"Return the node line number";
            case xml_type:
                return L"Return the node type";
            case xml_content:
                return L"Return the node content";
            case xml_namespace:
                return L"Return the node name space";
            case xml_nodeid:
                return L"Return the node internal id";
            case xml_setnodeid:
                return L"Set an internal numerical value to the current xml node";
        }
    }

};

Lispe_xmldoc* Lispe_xml::docs[1024];
short Lispe_xml::firstposition = 0;
ThreadLock Lispe_xml::_lock;

Element* Lispe_xmldoc::root(short type_xmlnode) {
    if (doc->children == NULL)
        return null_;
    return new Lispe_xmlnode(type_xmlnode, iddoc, doc->children);
}

void Lispe_xmldoc::clean() {
    if (lisp->checkforLock())
        Lispe_xml::_lock.locking();
    
    Lispe_xml::docs[iddoc] = NULL;
    if (iddoc < Lispe_xml::firstposition)
        Lispe_xml::firstposition = iddoc;
    
    if (lisp->checkforLock())
        Lispe_xml::_lock.unlocking();
}

bool Lispe_xmlnode::getDoc() {
    return (Lispe_xml::docs[iddoc] != NULL);
}

extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {
    //We first create the body of the function

    wstring w = L"xml";
    short identifier = lisp->encode(w);
    
    Lispe_xml::firstposition = 0;
    for (short i = 0; i < 1024; i++) {
        Lispe_xml::docs[i] = NULL;
    }
    
    lisp->extension("deflib xml_load (pathname)", new Lispe_xml(lisp, xml_load, identifier));
    lisp->extension("deflib xml_parse (buffer)", new Lispe_xml(lisp, xml_parse, identifier));
    lisp->extension("deflib xml_root (doc)", new Lispe_xml(lisp, xml_root, identifier));
    lisp->extension("deflib xml_next (node)", new Lispe_xml(lisp, xml_next, identifier));
    lisp->extension("deflib xml_previous (node)", new Lispe_xml(lisp, xml_previous, identifier));
    lisp->extension("deflib xml_child (node)", new Lispe_xml(lisp, xml_child, identifier));
    lisp->extension("deflib xml_parent (node)", new Lispe_xml(lisp, xml_parent, identifier));
    lisp->extension("deflib xml_name (node)", new Lispe_xml(lisp, xml_name, identifier));
    lisp->extension("deflib xml_properties (node)", new Lispe_xml(lisp, xml_properties, identifier));
    lisp->extension("deflib xml_line (node)", new Lispe_xml(lisp, xml_line, identifier));
    lisp->extension("deflib xml_type (node)", new Lispe_xml(lisp, xml_type, identifier));
    lisp->extension("deflib xml_content (node)", new Lispe_xml(lisp, xml_content, identifier));
    lisp->extension("deflib xml_namespace (node)", new Lispe_xml(lisp, xml_namespace, identifier));
    lisp->extension("deflib xml_nodeid (node)", new Lispe_xml(lisp, xml_nodeid, identifier));
    lisp->extension("deflib xml_setnodeid (node id)", new Lispe_xml(lisp, xml_setnodeid, identifier));
    return true;
}
}

