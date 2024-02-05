/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  tokens.h (see tools.cxx for implementation)
//
//

#ifndef tokens_h
#define tokens_h


#include <string>
#include <vector>
#include <unordered_map>
#include <set>

using std::u16string;
using std::string;
using std::wstring;

#define u_uchar char32_t
#define u_ustring std::u32string
#define uchar unsigned char

#ifdef WIN32
#define Exporting __declspec(dllexport)
#else
#define Exporting
#endif

string c_unicode_to_utf8(u_uchar code);

#include "emojis.h"
#include "tools.h"
#include "vecte.h"


using std::vector;
using std::unordered_map;
typedef enum {flag_none = 0, flag_negation = 1, flag_skip = 2, flag_fail = 4, flag_start_fail = 8, flag_blocking_gate = 16, flag_tail = 32, flag_action = 63, flag_vectorized = 64, flag_postpone = 128, flag_seen = 256, flag_visited = 512, flag_added = 1024} tokenizer_flag;


typedef enum {act_none, act_char , act_uppercase, act_ckj, act_space_and_carriage, act_alphabetical,
    act_lowercase, act_digit, act_greek, act_non_breaking_space, act_operator, act_punctuation, act_emoji, act_emojicomp,
    act_carriage, act_space, act_meta, act_interval, act_any, act_epsilon, act_end} tokenizer_action;

#define check_flag(a,b) ((a&b)==b)
#define remove_flag(a,b) a &= ~b
#define add_flag(a,b) a |= b

bool ckjchar(wchar_t ucs);

const int16_t table_character_size = 1611;

class Token_error {
public:
    u_ustring msg;

    Token_error(u_ustring m) : msg(m) {}
};
//---------------------------------------------------
//---------------------------------------------------
// Automaton based method
//---------------------------------------------------
//---------------------------------------------------

class tokenizer_node;
template <typename STRNG> class tokenizer_result {
public:
    vecte<int16_t> stacktype;
    vector<long> stackln;
    vector<long> positions;
    
    vector<long> p_stack;
    vector<short> i_stack;
    vector<tokenizer_node*> r_stack;

    u_uchar currentchr;
    u_ustring token;
    
    long line;
    long start;
    long position;
    long buffer_size;

    char sz_read;
    bool store_all;
    
    bool end() {
        return (position >= buffer_size && !currentchr);
    }

    vecte_n<STRNG> stack;
#ifdef DEBUGGER    
    vector<STRNG> string_stack;
#endif
    
    STRNG* buffer;
    vecte_n<STRNG>* stack_ptr;
    
    tokenizer_result<STRNG>(vecte_n<STRNG>* s, bool p_all = true) {
        stack_ptr = s;
        store_all = p_all;
    }
    
    tokenizer_result<STRNG>() {
        stack_ptr = &stack;
        store_all = true;
    }
        
    void store(int32_t label);
    void store_currentchr();
    
    inline void clear_stack() {
        r_stack.clear();
        i_stack.clear();
        p_stack.clear();
    }

    //Keeping the stack small
    inline void check_stack(tokenizer_node* rule) {
        if (r_stack.size() > 1000 && rule == r_stack[1]) {
            r_stack.erase(r_stack.begin() + 1, r_stack.end());
            i_stack.erase(i_stack.begin() + 1, i_stack.end());
            p_stack.erase(p_stack.begin() + 3, p_stack.end());
        }
    }
    
    inline long pop() {
        long i = p_stack.back();
        p_stack.pop_back();
        return i;
    }

    inline void push_stack(tokenizer_node* rule, long i) {
        r_stack.push_back(rule);
        i_stack.push_back(i);
        
        p_stack.push_back(line);
        p_stack.push_back(position);
        p_stack.push_back(token.size());
        p_stack.push_back((long)currentchr);
    }

    inline long pop_stack() {
        long  i = i_stack.back();
        i_stack.pop_back();
        r_stack.pop_back();
        
        currentchr = (u_uchar)pop();
        token = token.substr(0, pop());
        position = pop();
        line = pop();
                
        return i;
    }

    
    void clear(STRNG& u) {
        buffer = &u;
        buffer_size = u.size();
        
        currentchr = 0;
        line = 0;
        start = 0;
        position = 0;
        sz_read = 0;
#ifdef DEBUGGER        
        string_stack.clear();
#endif
        stack_ptr->clear();
        stacktype.clear();
        stackln.clear();
        positions.clear();
    }

    void clear(STRNG* u, long sz) {
        buffer = u;
        buffer_size = sz;
        
        currentchr = 0;
        line = 0;
        start = 0;
        position = 0;
        sz_read = 0;
#ifdef DEBUGGER        
        string_stack.clear();
#endif
        stack_ptr->clear();
        stacktype.clear();
        stackln.clear();
        positions.clear();
    }

    long size() {
        return stack.size();
    }
    
    void getnext();
    
};

class tokenizer_automaton;
//A node in the automaton
class tokenizer_node {
public:
    vector<tokenizer_node*> arcs;
    
    int16_t idpos;
    int16_t flags;
    
    u_uchar label;
	u_uchar endlabel;
    
    tokenizer_action action;
    
    
    tokenizer_node(tokenizer_action a, u_uchar c, u_uchar e) {
        idpos = -1;
        flags = flag_none;
        action = a;
        label = c;
        endlabel = e;
    }
    
    inline bool pure_arc() {
        return (action > act_none && action < act_meta && !check_flags());
    }
    
    inline bool is_epsilon() {
        return (action == act_epsilon && !check_flags());
    }
    
    inline long size() {
        return arcs.size();
    }
    
    inline bool check_flags() {
        return ((flags & flag_action) != 0);
    }
    
    inline bool check_negation() {
        return check_flag(flags, flag_negation);
    }
    
    inline bool check_fail() {
        return check_flag(flags, flag_fail);
    }
    
    inline bool check_start_fail() {
        return check_flag(flags, flag_start_fail);
    }
    
    inline bool check_skip() {
        return check_flag(flags, flag_skip);
    }
    
    inline bool check_vectorized() {
        return check_flag(flags, flag_vectorized);
    }
    
    inline bool check_visited() {
        return check_flag(flags, flag_visited);
    }
    
    inline bool check_postpone() {
        return check_flag(flags, flag_postpone);
    }
    
    inline bool check_seen() {
        return check_flag(flags, flag_seen);
    }
    
    inline bool check_added() {
        return check_flag(flags, flag_added);
    }

    inline bool check_blocking_gate() {
        return check_flag(flags, flag_blocking_gate);
    }

    inline bool check_tail() {
        return check_flag(flags, flag_tail);
    }

    void processing_postponed_nodes(std::set<long>& visited, vector<tokenizer_node*>& nodes, long idpos);
    void remove_epsilon_nodes(std::unordered_map<long, std::set<long> >& visited, std::set<long>& up, vector<tokenizer_node*>& nodes, bool epsilon);
    void merge(bool top);

    //We remove the epsilon nodes
    void trim_epsilon(vector<tokenizer_node*>& nodes) {
        std::set<long> up;
        std::unordered_map<long, std::set<long> > visited;
        remove_epsilon_nodes(visited, up, nodes, is_epsilon());
    }

    //We mark all nodes that belong to the same automaton for cleaning purposes
    //The nodes that won't have been marked will be deleted.
    void mark_nodes() {
        if (check_visited())
            return;
        add_flag(flags, flag_visited);
        for (long i = 0; i < arcs.size(); i++) {
            arcs[i]->mark_nodes();
        }
    }

    inline void fail_arcs() {
        tokenizer_node* node;
        //We mark the final arcs, to take into account sequences
        //In a sequence, only the last element is marked with fail.
        for (long i = 0; i < arcs.size(); i++) {
            node = arcs[i];
            //The first node is marked with flag_start_fail
            add_flag(node->flags, flag_start_fail);
            while (node->arcs.size()) node = node->arcs.back();
            //if it a sequence, we have the beginning and the end of the sequence that is marked
            //otherwise the node has both flags on.
                     add_flag(node->flags, flag_fail);
        }
    }
    
    inline void clone(tokenizer_node* a) {
        arcs = a->arcs;
        action = a->action;
        flags = a->flags;
        label = a->label;
        endlabel = a->endlabel;
    }
    
    inline bool empty() {
        return !arcs.size();
    }
    
    inline void append_final(tokenizer_node* a) {
        tokenizer_node* node = this;
        while (node->arcs.size()) node = node->arcs.back();
        if (!node->check_fail())
            node->arcs.push_back(a);
    }
    
    //We addd a and b on the final nodes for each arc
    //This is a way to handle {..}+
    inline void append_final(tokenizer_node* a, tokenizer_node* b) {
        tokenizer_node* node = this;
        while (node->arcs.size()) node = node->arcs.back();
        if (!node->check_fail()) {
            node->arcs.push_back(a);
            node->arcs.push_back(b);
        }
    }
    
    inline void append(tokenizer_node* a) {
        arcs.push_back(a);
    }
    
    bool check(u_uchar chr, UTF8_Handler* access, std::set<u_uchar>& operators, bool not_neg);
    
    inline void getnext(u_ustring& w, u_uchar& res, long& pos, long& l) {
        if (pos>=w.size()) {
            res =  0;
            return;
        }
        
        if (w[pos]==L'\n')
            l++;
        res = w[pos++];
    }
    
};

//---------------------------------------------------------------
//The automaton itself
//---------------------------------------------------------------

class tokenizer_automaton {
public:
    tokenizer_node* vectorization_table[table_character_size];
    hmap<int32_t,vector<tokenizer_node*> > indexed_on_label;

    vector<tokenizer_node*> compiled_rules;
    vector<tokenizer_node*> nodes;
    
    vecte_n<u_ustring> rules;
    
    std::set<u_uchar> operators;
    
    UTF8_Handler* access;
    tokenizer_node* initial;
    
    tokenizer_flag negation;
    bool disjunction;
    bool loaded;
    bool displayautomata;
    
    tokenizer_automaton(UTF8_Handler* a) {
        disjunction =  false;
        displayautomata = false;
        negation = flag_none;
        initial = NULL;
        access = a;
        for (int16_t i = 1; i < table_character_size; i++) {
            vectorization_table[i] = NULL;
        }
        vectorization_table[0] = node(act_epsilon, 0);
        loaded = false;
        setoperators();
    }
    
    ~tokenizer_automaton() {
        for (long i = 0; i < nodes.size(); i++) {
            delete nodes[i];
        }
    }

    void compile();

    void reset() {
        for (long i = 0; i < nodes.size(); i++) {
            delete nodes[i];
        }
        nodes.clear();
        compiled_rules.clear();
        for (int16_t i = 1; i < table_character_size; i++) {
            vectorization_table[i] = NULL;
        }
        vectorization_table[0] = node(act_epsilon, 0);
        indexed_on_label.clear();
        setoperators();
        loaded = false;
    }
    
    
    void setoperators() {
        operators.clear();
        std::set<u_ustring> ops;
        ops.insert(U"¬");
        ops.insert(U"§");
        ops.insert(U"•");
        ops.insert(U"°");
        ops.insert(U"%");
        ops.insert(U"@");
        ops.insert(U"=");
        ops.insert(U"!");
        ops.insert(U"+");
        ops.insert(U"-");
        ops.insert(U"*");
        ops.insert(U"/");
        ops.insert(U"^");
        ops.insert(U"<");
        ops.insert(U">");
        ops.insert(U"~");
        ops.insert(U",");
        ops.insert(U"&");
        ops.insert(U"|");
        ops.insert(U"#");
        ops.insert(U"?");
        ops.insert(U"\\");
        ops.insert(U"$");
        for (auto& u: ops)
            operators.insert(u[0]);
    }
    
    void replacemetas(map<u_ustring, u_ustring>& metalines, u_ustring& line) {
        if (line.find(U"%") == -1 || line.size()==1)
            return;
        
        u_ustring rep;
        u_ustring fd;
        for (const auto& k : metalines) {
            if (line.find(k.first) != -1) {
                fd = k.first;
                rep = k.second;
                line = s_ureplacestring(line, fd, rep);
            }
        }
    }
    
    void initialize() {
        setrules();
        compile();
    }
    
    void set_rules(vecte_n<u_ustring>& r) {
        reset();
        rules = r;
        compile();
    }
    
    void get_rules(vecte_n<u_ustring>& r) {
        if (!rules.size()) {
            setrules();
            r=rules;
            rules.clear();
            return;
        }
        
        r=rules;
    }
    
    virtual void setrules();
    
    void display(tokenizer_node*, int nbblanks, bool);
    void asString(std::set<int16_t>& shared, std::wstringstream& str, tokenizer_node*, int nbblanks, bool);
    
    tokenizer_node* node(tokenizer_action a, u_uchar c, u_uchar e = 0) {
        if (a == act_meta) {
            switch (c) {
                case 'C':
                    a =  act_uppercase;
                    break;
                case 'H':
                    a =  act_ckj;
                    break;
                case 'S':
                    a =  act_space_and_carriage;
                    break;
                case 'E':
                    a =  act_emojicomp;
                    break;
                case 'a':
                    a =  act_alphabetical;
                    break;
                case 'c':
                    a =  act_lowercase;
                    break;
                case 'd':
                    a =  act_digit;
                    break;
                case 'e':
                    a =  act_emoji;
                    break;
                case 'h':
                    a =  act_greek;
                    break;
                case 'n':
                    a =  act_non_breaking_space;
                    break;
                case 'o':
                    a =  act_operator;
                    break;
                case 'p':
                    a =  act_punctuation;
                    break;
                case 'r':
                    a =  act_carriage;
                    break;
                case 's':
                    a =  act_space;
                    break;
            }
        }
        
        tokenizer_node* n = new tokenizer_node(a,c,e);
        add_flag(n->flags, negation);
        negation = flag_none;
        n->idpos = nodes.size();
        nodes.push_back(n);
        return n;
    }
    /*
     There are different ways to access rules when evaluating the first character of a token.
     a) Either it is in the vectorization table
     b) Or it is in the tokenizer_rules vector
     
     The vectorization table makes it possible to index rules on their first character.
     It is composed of 1611 slots (table_character_size), where each slot corresponds to
     the Unicode code of a character. For instance, 'A' has the code 65. At the position 65 we have rules that deals with
     alphabetical characters or with rules that starts with 'A'. A slot can contain more than 1 rule, which means that the order in
     which these rules are stored depends on the order in which they have been declared.
     
     The tokenizer_rules contains the rule, which could not be indexed on their first characters.
     Basically when parsing a code, we first try the rules which are indexed on this character if we can,
     otherwise we roll backs on tokenizer_rules.
     */

    void vectorization(tokenizer_node* a, u_uchar c) {
        if (!c || c >= table_character_size) {
            if (a->check_added())
                return;
            add_flag(a->flags, flag_added);
            c = 0;
        }
        else {
            if (vectorization_table[c] == NULL)
                vectorization_table[c] = node(act_epsilon, 0);
        }
        
        vectorization_table[c]->append(a);
        add_flag(a->flags, flag_vectorized);
        if (initial == NULL)
            initial = a;
    }
    
    tokenizer_node* append(tokenizer_node* root, tokenizer_node* n) {
        if (initial == NULL)
            initial = n;
        
        if (root != NULL) {
            if (disjunction) {
                root->append(n);
                return root;
            }
            //If it is a disjunction, then root action is epsilon
            //the new arc needs to be added to each sub-arc.
            root->append(n);
        }
        else {
            //The position 0 in vectorization_table is used to store no indexable rules
            if (!n->check_vectorized())
                vectorization(n, 0);
        }
        
        return n;
    }
        
    template <typename STRNG> bool iterative_traversal(tokenizer_result<STRNG>& rst, tokenizer_node* rule) {
        //In this case, we apply a iterative analysis...
        //The first element is the one that should be found to stop
        tokenizer_node* r;
        long i = 0;
        
        rst.clear_stack();
        rst.push_stack(rule, 0);
        if (!check_flag(rule->action, act_epsilon)) {
            if (!rule->check_skip())
                rst.token += rst.currentchr;
            rst.getnext();
        }

        long sz = rule->size();
        while (rst.r_stack.size() && !rst.end()) {
            for (; i < sz && !rst.end(); i++) {
                r = rule->arcs[i];
                if (r->action == act_end) {
                    rst.store(r->label);
                    return true;
                }
                
                if (r->check(rst.currentchr, access, operators, !r->check_negation())) {
                    if (r == rule) {
                        i = -1;
                        if (!check_flag(r->action, act_epsilon)) {
                            if (!r->check_skip())
                                rst.token += rst.currentchr;
                            rst.getnext();
                        }
                    }
                    else {
                        rst.check_stack(r);
                        rst.push_stack(r, i + 1);
                        if (!check_flag(r->action, act_epsilon)) {
                            if (!r->check_skip())
                                rst.token += rst.currentchr;
                            rst.getnext();
                        }
                        rule = r;
                        sz = rule->size();
                        i = 0;
                        break;
                    }
                }
            }
            if (i == sz) {
                i = rst.pop_stack();
                if (rst.r_stack.size()) {
                    rule = rst.r_stack.back();
                    sz = rule->size();
                }
            }
        }
        return false;
    }
    
    //Since, the code is slightly different according to the type of string we are handling, we create a template function to handle all these cases in one code.
    template <typename STRNG> bool traverse(tokenizer_result<STRNG>& rst, tokenizer_node* rule, bool alreadychecked, bool top) {
        if (rule->action == act_end) {
            rst.store(rule->label);
            return true;
        }
        if (rst.end())
            return false;
        
        if (top) {
            rst.start = rst.position - rst.sz_read;
            rst.token = U"";
        }
        

        u_uchar chr = rst.currentchr;
        long l = rst.line;
        long p = rst.position;
        long tokenlen = rst.token.size();

        long sz = rule->arcs.size();

        while (sz == 1 && !rst.end()) {
            if (alreadychecked || rule->check(rst.currentchr, access, operators, !rule->check_negation())) {
                //In this case we fail a sequence of nodes that should not go up to here
                // For instance, we want the system to fail if abc is detected with rule: ~{[abc]...}
                if (rule->check_fail())
                    return true;

                if (rule->check_tail())
                    return iterative_traversal<STRNG>(rst, rule);

                if (!check_flag(rule->action, act_epsilon)) {
                    if (!rule->check_skip())
                        rst.token += rst.currentchr;
                    rst.getnext();
                }
                rule = rule->arcs[0];
                if (rule->action == act_end) {
                    rst.store(rule->label);
                    return true;
                }
                alreadychecked = false;
                sz = rule->arcs.size();
            }
            else {
                rst.token = rst.token.substr(0, tokenlen);
                rst.currentchr = chr;
                rst.line = l;
                rst.position = p;
                return false;
            }
        }
        

        if (alreadychecked || rule->check(rst.currentchr, access, operators, !rule->check_negation())) {
            if (rule->check_fail())
                return true;

            if (rule->check_tail())
                return iterative_traversal<STRNG>(rst, rule);

            //If it is not an epsilon, there was an actual character comparison
            alreadychecked = false;
            if (!check_flag(rule->action, act_epsilon)) {
                if (!rule->check_skip())
                    rst.token += rst.currentchr;
                alreadychecked = true;
                rst.getnext();
            }
            
            tokenizer_node* r;
            for (long i = 0; i < sz; i++) {
                r = rule->arcs[i];
                if (r->action == act_end) {
                    rst.store(r->label);
                    return true;
                }
                if (r->check(rst.currentchr, access, operators, !r->check_negation())) {
                    //Tail recursion detected, we simply iterate...
                    //alreadycheck is true is this arc is not an epsilon
                    if (alreadychecked && r == rule) {
                        i = -1;
                        if (!r->check_skip())
                            rst.token += rst.currentchr;
                        rst.getnext();
                    }
                    else {
                        /*
                        else we go into recursion
                        Note the check_fail and check_start_fail to take into account: ~{...}
                        If one element of the disjunction is true, the whole disjunction must fail
                        The only case when fail and start_fail are different is when a sequence is negated
                        in the disjunction. In that case, start_fail occurs on the beginning of the sequence
                        and fail on the last element of the disjunction. Hence, if the sequence succeeds deep in recursion
                        we will fail it by returning !start_fail.
                        */
                        if ((r->check_fail() || traverse<STRNG>(rst, r, true, false)))
                            return (!r->check_start_fail());
                    }
                }
            }
        }
        
        rst.token = rst.token.substr(0, tokenlen);
        rst.currentchr = chr;
        rst.line = l;
        rst.position = p;
        return false;
    }
    
    template <typename STRNG> inline bool execute_rule(u_uchar c, tokenizer_result<STRNG>& rst) {
        return (c < table_character_size) && vectorization_table[c]?traverse<STRNG>(rst, vectorization_table[c], false, true):false;
    }
    
    template <typename STRNG> inline bool execute_rule(tokenizer_result<STRNG>& rst) {
        return traverse<STRNG>(rst, vectorization_table[0], false, true);
    }

    template <typename STRNG> void apply(tokenizer_result<STRNG>& rst) {
        rst.getnext();
        while (!rst.end()) {
            //The first step consists of finding the first rule to apply
            //If the current character is in table, then we start from here
            if (!execute_rule<STRNG>(rst.currentchr, rst) && !execute_rule<STRNG>(rst)) {
                rst.store_currentchr();
                rst.getnext();
            }
        }
    }
  
    template <typename STRNG> long tokenize(STRNG& thestr, tokenizer_result<STRNG>& r) {
        if (!loaded) {
            setrules();
            compile();
        }
        //only stack is necessary
        r.clear(thestr);
        apply<STRNG>(r);
        return r.size();
    }    
};

//---------------------------------------------------------------
//LispE tokenizer
//---------------------------------------------------------------
class lispe_tokenizer : public tokenizer_automaton {
public:
    
    lispe_tokenizer(UTF8_Handler* a) : tokenizer_automaton(a) {}
    
    void setrules() {
        tokenizer_automaton::setrules();
        rules[0] = U"%s+=#";
        long i;
        for (i = 0; i < rules.size(); i++) {
            if (rules[i] == U";;?+;;=:67")
                break;
        }
        //Comments are no longer recorded...
        if (i != rules.size()) {
            rules[i] = U";;?+;;=:#";
            rules[i+1] = U";?+#10=#";
            rules[i+2] = U"%#!?+%r=#";
        }
    }
    
};
//---------------------------------------------------------------
//Segmenter
//---------------------------------------------------------------
class segmenter_automaton : public tokenizer_automaton {
public:
    char decimal_separator;
    bool blanks;
    
    segmenter_automaton(UTF8_Handler* a) : tokenizer_automaton(a) {
        decimal_separator = 0;
        blanks = false;
    }
    
    void setrules();
    
    bool check_rule(int32_t label) {
        return (indexed_on_label.count(label) && indexed_on_label[label].size());
    }
    
    //Rules do not skip blanks anymore
    void keepblanks(bool kp) {
        if (check_rule(99) && blanks != kp) {
            blanks = kp;
            if (kp)
                remove_flag(indexed_on_label[99][0]->flags, flag_blocking_gate);
            else
                add_flag(indexed_on_label[99][0]->flags, flag_blocking_gate);
        }
    }
    
    void setdecimalmode(char sep) {
        if (check_rule(22) && check_rule(66)  && check_rule(88) && decimal_separator != sep) {
            long i;
            decimal_separator = sep;
            switch (decimal_separator) {
                case 0:
                    for (i = 0; i < indexed_on_label[22].size(); i++) {
                        remove_flag(indexed_on_label[22][i]->flags, flag_blocking_gate);
                    }
                    for (i = 0; i < indexed_on_label[66].size(); i++) {
                        add_flag(indexed_on_label[66][i]->flags, flag_blocking_gate);
                    }
                    for (i = 0; i < indexed_on_label[88].size(); i++) {
                        add_flag(indexed_on_label[88][i]->flags, flag_blocking_gate);
                    }
                    break;
                case 1:
                    for (i = 0; i < indexed_on_label[22].size(); i++) {
                        add_flag(indexed_on_label[22][i]->flags, flag_blocking_gate);
                    }
                    for (i = 0; i < indexed_on_label[66].size(); i++) {
                        remove_flag(indexed_on_label[66][i]->flags, flag_blocking_gate);
                    }
                    for (i = 0; i < indexed_on_label[88].size(); i++) {
                        add_flag(indexed_on_label[88][i]->flags, flag_blocking_gate);
                    }
                    break;
                case 2:
                    for (i = 0; i < indexed_on_label[22].size(); i++) {
                        add_flag(indexed_on_label[22][i]->flags, flag_blocking_gate);
                    }
                    for (i = 0; i < indexed_on_label[66].size(); i++) {
                        add_flag(indexed_on_label[66][i]->flags, flag_blocking_gate);
                    }
                    for (i = 0; i < indexed_on_label[88].size(); i++) {
                        remove_flag(indexed_on_label[88][i]->flags, flag_blocking_gate);
                    }
                    break;
            }
        }
    }
};

#endif






