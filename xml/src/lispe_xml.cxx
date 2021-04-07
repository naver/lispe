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
    ~Lispe_xmldoc() {
        lisp->vpool_release(iddoc);
        if (doc != NULL)
            xmlFreeDoc(doc);
    }
};


class Lispe_xmlnode : public Element {
public:
    xmlNodePtr node;
    LispE* lisp;
    short iddoc;

    Lispe_xmlnode(LispE* l, short ty, short idd, xmlNodePtr n) : Element(ty) {
        node = n;
        iddoc = idd;
        lisp = l;
    }

    bool getDoc() {
        return lisp->vpool_check(iddoc);
    }

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

    Element* next() {
        if (!getDoc() || node == NULL || node->next == NULL)
            return null_;
        return new Lispe_xmlnode(lisp, type, iddoc,  node->next);
    }

    Element* previous() {
        if (!getDoc() || node == NULL || node->prev == NULL)
            return null_;
        return new Lispe_xmlnode(lisp, type, iddoc,  node->prev);
    }

    Element* child() {
        if (!getDoc() || node == NULL || node->children == NULL)
            return null_;
        return new Lispe_xmlnode(lisp, type, iddoc,  node->children);
    }

    Element* parent() {
        if (!getDoc() || node == NULL || node->parent == NULL || node->parent->type == XML_DOCUMENT_NODE || node->parent->name == NULL)
            return null_;
        return new Lispe_xmlnode(lisp, type, iddoc,  node->parent);
    }

    Element* name() {
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

    Element* line() {
        if (!getDoc() || node == NULL)
            return null_;

        return lisp->provideInteger(node->line);
    }

    Element* node_type() {
        if (!getDoc() || node == NULL)
            return null_;

        return lisp->provideInteger(node->type);
    }

    Element* node_id() {
        if (!getDoc() || node == NULL)
            return null_;

        return lisp->provideInteger((long)node->_private);
    }

    Element* set_node_id(long idx) {
        if (!getDoc() || node == NULL)
            return null_;
        node->_private = (void*)idx;
        return this;
    }

    Element* content() {
        if (!getDoc() || node == NULL)
            return null_;

        string content;
        TraverseXML(node, content, true);
        return lisp->provideString(content);
    }

    Element* name_space() {
        if (!getDoc() || node == NULL)
            return emptylist_;

        Strings* kvect = new Strings;
        if (node->nsDef != NULL) {
            xmlNsPtr cur = node->nsDef;
            string val;
            while (cur != NULL) {
                if (cur->href != NULL) {
                    val = (char*)cur->href;
                    kvect->append(val);
                }
                else {
                    val = "";
                    kvect->append(val);
                }

                val = (char*)cur->prefix;
                kvect->append(val);
                cur = cur->next;
            }
        }
        return kvect;
    }

    Element* properties() {
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

    xml_action action;
    short type_xmlnode;
    short type_xmldoc;

    Lispe_xml(LispE* lisp, xml_action a, short identifier) : action(a), Element(identifier) {
        wstring w = L"xmlnode";
        type_xmlnode = lisp->encode(w);
        w = L"xmldoc";
        type_xmldoc = lisp->encode(w);
    }

    Element* eval(LispE* lisp) {
        //The name defined in the extension is not insignificant, it is used to retrieve our arguments.
        switch (action) {
            case xml_load: {
                long iddoc = lisp->vpool_slot();
                Lispe_xmldoc* doc = new Lispe_xmldoc(lisp, iddoc, type_xmldoc);
                lisp->vpool_in(doc, iddoc);
                string pathname = lisp->get(L"pathname")->toString(lisp);
                return doc->load(pathname);
            }
            case xml_parse: {
                long iddoc = lisp->vpool_slot();
                Lispe_xmldoc* doc = new Lispe_xmldoc(lisp, iddoc, type_xmldoc);
                lisp->vpool_in(doc, iddoc);
                string buffer = lisp->get(L"buffer")->toString(lisp);
                return doc->parse(buffer);
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
                return ((Lispe_xmlnode*)node)->next();
            }
            case xml_previous: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->previous();
            }
            case xml_child: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->child();
            }
            case xml_parent: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->parent();
            }
            case xml_name: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->name();
            }
            case xml_properties: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->properties();
            }
            case xml_line: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->line();
            }

            case xml_namespace: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->name_space();
            }
            case xml_type: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->node_type();
            }
            case xml_content: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->content();
            }
            case xml_nodeid: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                return ((Lispe_xmlnode*)node)->node_id();
            }
            case xml_setnodeid: {
                Element* node = lisp->get("node");
                if (node->type != type_xmlnode)
                    throw new Error("Error: the first parameter should be an 'xmlnode'");
                long idx = lisp->get("id")->asInteger();
                return ((Lispe_xmlnode*)node)->set_node_id(idx);
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

Element* Lispe_xmldoc::root(short type_xmlnode) {
    if (doc->children == NULL)
        return null_;
    return new Lispe_xmlnode(lisp, type_xmlnode, iddoc, doc->children);
}

extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {
    //We first create the body of the function

    wstring w = L"xml";
    short identifier = lisp->encode(w);
        
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

