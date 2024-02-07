#include "tokens.h"
//------------------------------------------------------------
//Automaton implementation for tokenization
//------------------------------------------------------------

void tokenizer_automaton::display(tokenizer_node* a, int nbblanks, bool top) {
    static std::set<int16_t> found;
    //We start from initial
    if (top) {
        found.clear();
    }
    //This node was already in our set
    bool ret = false;
    if (found.count(a->idpos))
        ret = true;
    else
        found.insert(a->idpos);
    
    string blanks(nbblanks, ' ');
    cout << blanks;
    
    if (a->check_negation())
        cout << "~";
    if (a->check_skip())
        cout << '-';
    if (a->check_start_fail()) {
        if (a->check_fail())
            cout << "<BE>";
        else
            cout << "<B>";
    }
    else {
        if (a->check_fail())
            cout << "<E>";
    }
    
    if (a->action == act_meta) {
        cout << '%';
    }
    
    if (a->action == act_end) {
        if (a->label < 33)
            cout << '$' << (int)a->label;
        else
            cout << '$' << (uchar)a->label;
    }
    else {
        if (!a->label) {
            if (a->action == act_any)
                cout << "ANY";
            else
                cout << "EPSILON";
        }
        else {
            if (a->label < 33)
                cout << (int)a->label;
            else
                cout << (uchar)a->label;
            if (a->endlabel) {
                if (a->endlabel < 33)
                    cout << "-" << (int)a->endlabel;
                else
                    cout << "-" << (uchar)a->endlabel;
            }
        }
    }
    
    if (ret && a->action != act_end) {
        cout << "_SEE_" << a->idpos << endl;
        return;
    }
    
    cout << '_' << a->idpos << endl;

    //we explore the subarcs to give them a position
    for (long i = 0; i < a->arcs.size(); i++) {
        display(a->arcs[i], nbblanks + 2, false);
    }
    if (top)
        cout << endl;
}

void tokenizer_automaton::asString(std::set<int16_t>& shared, std::wstringstream& str, tokenizer_node* a, int nbblanks, bool top) {
    //This node was already in our set
    bool ret = false;
    if (shared.count(a->idpos))
        ret = true;
    else
        shared.insert(a->idpos);
    
    wstring blanks(nbblanks, ' ');
    str << blanks;
    
    if (a->check_negation())
        str << "~";
    if (a->check_skip())
        str << '-';
    if (a->check_start_fail()) {
        if (a->check_fail())
            str << "<BE>";
        else
            str << "<B>";
    }
    else {
        if (a->check_fail())
            str << "<E>";
    }
    if (a->action == act_meta) {
        str << '%';
    }
    
    if (a->action == act_end) {
        if (a->label < 33)
            str << '$' << (int)a->label;
        else
            str << '$' << (uchar)a->label;
    }
    else {
        if (!a->label) {
            if (a->action == act_any)
                str << "ANY";
            else
                str << "EPSILON";
        }
        else {
            if (a->label < 33)
                str << (int)a->label;
            else
                str << (uchar)a->label;
            if (a->endlabel) {
                if (a->endlabel < 33)
                    str << "-" << (int)a->endlabel;
                else
                    str << "-" << (uchar)a->endlabel;
            }
        }
    }
    if (ret && a->action != act_end) {
        str << "_SEE_" << a->idpos << endl;
        return;
    }
    
    str << '_' << a->idpos << endl;

    //we explore the subarcs to give them a position
    for (long i = 0; i < a->arcs.size(); i++) {
        asString(shared, str, a->arcs[i], nbblanks + 2, false);
    }
    if (top)
        str << endl;
}

//------------------------------------------------------------
//The main function to check a character with a tokenize node
//------------------------------------------------------------

bool tokenizer_node::check(u_uchar chr, UTF8_Handler* access, std::set<u_uchar>& operators, bool not_neg) {
    if (!chr || check_blocking_gate())
        return false;
    
    switch(action) {
        case act_char:
            return (not_neg == (label == chr));
        case act_uppercase:
            return (not_neg == access->c_is_upper(chr));
        case act_ckj://Chinese/Japanese/Korean
            return (not_neg == ckjchar(chr));
        case act_space_and_carriage:
            return (not_neg == c_is_space_or_cr(chr));
        case act_alphabetical:
            return (not_neg == (chr=='_' || access->c_is_alpha(chr)));
        case act_lowercase:
            return (not_neg == access->c_is_lower(chr));
        case act_digit:
            return (not_neg == c_is_digit(chr));
        case act_greek:
            return (not_neg == (chr >= 913 && chr <= 987));
        case act_non_breaking_space: //non-breaking space
            return (not_neg == c_is_nbs_space(chr));
        case act_operator:
            return (not_neg == operators.count(chr));
        case act_punctuation:
            return (not_neg == access->c_is_punctuation(chr));
        case act_carriage:
            return (not_neg == (chr == 10 || chr == 13));
        case act_space:
            return (not_neg == c_is_space(chr));
        case act_interval:
            return (not_neg == (chr >= label && chr <= endlabel));
        case act_any:
        case act_epsilon:
            return true;
        case act_none:
        case act_end:
        case act_meta:
            return false;
        case act_emoji:
            return access->c_is_emoji(chr);
        case act_emojicomp:
            return access->c_is_emojicomp(chr);
    }
    return false;
}

//------------------------------------------------------------
//Cleaning epsilon nodes in tokenize automata
//------------------------------------------------------------

//These nodes have been detected as sub-nodes of themselves. Basically, it is a loop generated out of a Kleene operator (a loop)
void tokenizer_node::processing_postponed_nodes(std::set<long>& visited, vector<tokenizer_node*>& nodes, long idpos) {
    if (check_seen())
        return;
    
    add_flag(flags, flag_seen);
    
    long found = -1;
    for (long i = 0; i < arcs.size(); i++) {
        if (arcs[i]->idpos == idpos) {
            found = i;
        }
        else
            arcs[i]->processing_postponed_nodes(visited, nodes, idpos);
    }

    if (found != -1) {
        vector<tokenizer_node*> local;
        std::set<long> v;
        v.insert(idpos);
        for (long i = 0; i < arcs.size(); i++) {
            if (i == found) {
                for (auto& e : visited) {
                    if (v.count(e))
                        continue;
                    local.push_back(nodes[e]);
                    v.insert(e);
                }
            }
            else {
                if (!v.count(arcs[i]->idpos)) {
                    local.push_back(arcs[i]);
                    v.insert(arcs[i]->idpos);
                }
            }
        }
        arcs = local;
    }
}

//We are looking for epsilon nodes that can be merged...
//When we find an epsilon nodes, we go down in recursion to gather all existing nodes that are not epsilon down the way
//We then replace these epsilon nodes with these nodes.
//When we have an epsilon node that points to a termination node, this epsilon node becomes the new termination node.
void tokenizer_node::remove_epsilon_nodes(std::unordered_map<long,
                                          std::set<long> >& visited, std::set<long>& current,
                                          vector<tokenizer_node*>& nodes, bool epsilon) {
    if (check_visited())
        return;

        add_flag(flags, flag_visited);
    
    std::set<long> locals;
    std::set<long>* vect;
    tokenizer_node* n;
    long i;
    
    long sz = size();
    
    if (sz) {
        //We merge the arcs that share the same definition
        long key;
        std::unordered_map<long, tokenizer_node*> commons;
        vector<tokenizer_node*> nds;
        for (i = 0; i < sz; i++) {
            n = arcs[i];
            if (n->pure_arc()) {
                key = n->action*256 + n->label;
                if (commons.count(key)) {
                    tokenizer_node* nbase = commons[key];
                    for (key = 0; key < n->size(); key++)
                        nbase->arcs.push_back(n->arcs[key]);
                }
                else {
                    commons[key] = n;
                    nds.push_back(n);
                }
            }
            else
                nds.push_back(n);
        }
        //We have found some common arcs
        if (nds.size() != sz) {
            arcs.clear();
            arcs = nds;
            sz = arcs.size();
        }
    }

    if (epsilon)
        vect = &current;
    else
        vect = &locals;
    
    for (i = 0; i < sz; i++) {
        n = arcs[i];
        if (n->is_epsilon()) {
            if (n->size() == 1 && n->arcs[0]->action == act_end) {
                add_flag(n->arcs[0]->flags, flag_visited);
                n->clone(n->arcs[0]);
                n->arcs.clear();
                vect->insert(n->idpos);
            }
            else {
                if (n->check_visited()) {
                    if (visited.count(n->idpos)) {
                        for (auto& e: visited[n->idpos])
                            vect->insert(e);
                    }
                    else {
                        add_flag(n->flags, flag_postpone);
                        vect->insert(n->idpos);
                    }
                }
                else {
                    n->remove_epsilon_nodes(visited, *vect, nodes, true);
                    visited[n->idpos] = *vect;
                    if (n->check_postpone() && visited.count(n->idpos))
                        n->processing_postponed_nodes(visited[n->idpos], nodes, n->idpos);
                }
            }
        }
        else {
            vect->insert(n->idpos);
            n->remove_epsilon_nodes(visited, *vect, nodes, false);
        }
    }
    
    if (locals.size() != 0) {
        arcs.clear();
        for (auto& e : locals) {
            n = nodes[e];
            if (n->pure_arc())
                arcs.push_back(n);
        }
        
        for (auto& e : locals) {
            n = nodes[e];
            if (!n->pure_arc())
                arcs.push_back(n);
        }
        
        for (auto& e : locals) {
            n = nodes[e];
            if (n->check_postpone() && visited.count(n->idpos))
                processing_postponed_nodes(visited[n->idpos], nodes, n->idpos);
        }
    }
}

//----------------------------------------------------------------------------------------
//Specialization of extracting one character from a buffer for tokenizer_result
//tokenizer_result is a template that depends on the type of the string being manupulated.
//However each type has a specific expected behaviour
//----------------------------------------------------------------------------------------

template<> void tokenizer_result<u_ustring>::getnext() {
    if (position >= buffer_size) {
        currentchr =  0;
        return;
    }
    
    sz_read = 1;
    line += (currentchr == '\n');
    currentchr = (*buffer)[position++];
}

template<> void tokenizer_result<wstring>::getnext() {
    if (position >= buffer_size) {
        currentchr =  0;
        return;
    }
    
    sz_read = 1;
    line += (currentchr == '\n');
    if (c_utf16_to_unicode(currentchr, (*buffer)[position++], false)) {
        sz_read = 2;
        c_utf16_to_unicode(currentchr, (*buffer)[position++], true);
    }
}

template<> void tokenizer_result<u16string>::getnext() {
    if (position >= buffer_size) {
        currentchr =  0;
        return;
    }
    
    sz_read = 1;
    line += (currentchr == '\n');
    if (c_utf16_to_unicode(currentchr, (*buffer)[position++], false)) {
        sz_read = 2;
        c_utf16_to_unicode(currentchr, (*buffer)[position++], true);
    }
}

template<> void tokenizer_result<string>::getnext() {
    if (position >= buffer_size) {
        currentchr =  0;
        return;
    }
    
    line += (currentchr == '\n');
    sz_read = 1 + c_utf8_to_unicode(buffer, position, currentchr);
    position += sz_read;
}



template<> void tokenizer_result<u_ustring>::store_currentchr() {
    stack_ptr->push_back(u_ustring(1, currentchr));
    if (store_all) {
        stacktype.push_back(0);
        stackln.push_back(line);
        positions.push_back(start);
        positions.push_back(position - 1);
    }
    start = position;
}

#ifdef WIN32
template<> void tokenizer_result<wstring>::store_currentchr() {
    u_uchar c;
    wstring w;
    if (c_unicode_to_utf16(c, currentchr)) {
        w = (wchar_t)(c >> 16);
        w += (wchar_t)(c & 0xFFFF);
    }
    else
        w = (wchar_t)c;

    
    stack_ptr->push_back(w);
    if (store_all) {
        stacktype.push_back(0);
        stackln.push_back(line);
        positions.push_back(start);
        positions.push_back(position - sz_read);
    }
    start = position;
}
#else
template<> void tokenizer_result<wstring>::store_currentchr() {
    stack_ptr->push_back(wstring(1, currentchr));
    if (store_all) {
        stacktype.push_back(0);
        stackln.push_back(line);
        positions.push_back(start);
        positions.push_back(position - 1);
    }
    start = position;

}
#endif

template<> void tokenizer_result<std::u16string>::store_currentchr() {
    u_uchar c;
    u16string w;
    if (c_unicode_to_utf16(c, currentchr)) {
        w = (char16_t)(c >> 16);
        w += (char16_t)(c & 0xFFFF);
    }
    else
        w = (char16_t)c;
        
    
    stack_ptr->push_back(w);
    if (store_all) {
        stacktype.push_back(0);
        stackln.push_back(line);
        positions.push_back(start);
        positions.push_back(position - sz_read);
    }
    start = position;
}

template<> void tokenizer_result<string>::store_currentchr() {
    string w = c_unicode_to_utf8(currentchr);
    stack_ptr->push_back(w);
#ifdef DEBUGGER    
    string_stack.push_back(w);    
#endif
    if (store_all) {
        stacktype.push_back(0);
        stackln.push_back(line);
        positions.push_back(start);
        positions.push_back(position - sz_read);
    }
    start = position;
}


template<> void tokenizer_result<u_ustring>::store(int32_t label) {
    if (label != -1) {
        stack_ptr->push_back(token);
        if (store_all) {
            stacktype.push_back((unsigned char)label);
            stackln.push_back(line);
            positions.push_back(start);
            positions.push_back(position - 1);
        }
    }
}

#ifdef WIN32
template<> void tokenizer_result<wstring>::store(int32_t label) {
    if (label != -1) {
        wstring w;
        s_unicode_to_utf16(w, token);
        stack_ptr->push_back(w);
        if (store_all) {
            stacktype.push_back((unsigned char)label);
            stackln.push_back(line);
            positions.push_back(start);
            positions.push_back(position - sz_read);
        }
    }
}
#else
template<> void tokenizer_result<wstring>::store(int32_t label) {
    if (label != -1) {
        stack_ptr->push_back((wstring&)token);
        if (store_all) {
            stacktype.push_back((unsigned char)label);
            stackln.push_back(line);
            positions.push_back(start);
            positions.push_back(position - 1);
        }
    }
}
#endif

template<> void tokenizer_result<u16string>::store(int32_t label) {
    if (label != -1) {
        u16string w;
        s_unicode_to_utf16(w, token);
        stack_ptr->push_back(w);
        if (store_all) {
            stacktype.push_back((unsigned char)label);
            stackln.push_back(line);
            positions.push_back(start);
            positions.push_back(position - sz_read);
        }
    }
}

template<> void tokenizer_result<string>::store(int32_t label) {
    if (label != -1) {
        string w;
        s_unicode_to_utf8(w, token);
        stack_ptr->push_back(w);
#ifdef DEBUGGER        
        string_stack.push_back(w);
#endif
        if (store_all) {
            stacktype.push_back((unsigned char)label);
            stackln.push_back(line);
            positions.push_back(start);
            positions.push_back(position - sz_read);
        }
    }
}

//----------------------------------------------------------------------------------------
//Compiler for rules
//----------------------------------------------------------------------------------------

void tokenizer_automaton::compile() {
    /*
     
     The rules are compiled into tokenizer_node arcs and the results are stored in vectorization_table.
     This table contains: 1611 elements (see table_character_size).
     
     When a character code is < 1611, then the associated rules are stored in vectorization_table at character code location.
     For instance, if a rule starts with "A", then the rule will be stored at position 65 (ASCII code for "A")in vectorization_table.
     
     For rules that cannot be indexed, either because they start with an epsilon or because the initial character is > 1611, the
     rule is then stored at position 0.
     
     We have different potential action associated with an arc
     act_char             -> regular character
     act_meta             -> meta-character (%x)
     act_interval         -> interval between two characters: #A-F
     act_any              -> any character
     act_epsilon          -> an epsilon arc, which has created to handle disjunction or optionalaties
     act_end              -> the termination act, associated with a rule identifier, the returned value when the rule has applied
     
     We also have different flags to handle the node status
     flag_negation (1)        -> negation (~)
     flag_skip (2)            -> Character not stored (-)
     flag_fail (4)            -> Set to Element in negated disjunctions. When element is true, rule fails thank to this flag.
     flag_action (7)          -> flag_negation | flag_skip | flag_fail
     
     //The next flags are used to mark node utilisation
     flag_vectorized (8)   //has been added to vectorization_table as an index based on the first value of the rule
     flag_added (16)       //has been added to vectorization_table at position 0
     flag_postpone (32)    //While removing epsilon, a node that has already been seen but not fully evaluated since
     flag_visited (64)     //A node that has already been visited
     flag_seen (128)       //A node that has already been seen when processing postponed nodes
     
     -> IMPORTANT: for meta-characters such as %a or %d, we also use each possible value for these meta-characters as index in vectorization_table.
     */
    
    //The meta-characters for which there is a specific semantics
    char x_actions[]="CEHSacdehnoprs";
    
    map<u_ustring, u_ustring> metalines;
    vector<tokenizer_node*> brackets;
    vector<char> currentbracket;
    
    u_ustring line;
    u_ustring action_id;
    
    tokenizer_node* anode = NULL;
    tokenizer_node* current = NULL;
    tokenizer_node* root = NULL;
    
    u_uchar e;
    u_uchar cc;
    
    int32_t irule, final_action = -1;
    long pos;
    
    bool metarule_available = false;
    char error = 0;
    long begin_nodes;
    bool first;
    bool first_value;
    
    bool tail = false;
    
    for (irule = 0; irule < rules.size() && !error; irule++) {
        tail = false;
        line = rules[irule];
        begin_nodes = nodes.size();
        
        if (line[1]==':') { //we detect a meta-rule...
            cc = line[0];
            line=line.c_str()+2;
            action_id = U"%";
            action_id += cc;
            if (metarule_available)
                replacemetas(metalines,line);
            metalines[action_id] = line;
            metarule_available = true;
            compiled_rules.push_back(NULL);
            continue;
        }
        
        anode = NULL;
        current = NULL;
        disjunction = false;
        initial = NULL;
        root = NULL;
        //We need a dummy value in currentbracket
        currentbracket.clear();
        currentbracket.push_back('*');
        
        //first we look for the = sign at the end of the string...
        
        pos = line.rfind(U"=",line.size()-1);
        if (pos == -1)
            error = 1;
        else {
            // =: forces tail recursion
            if (line[pos + 1] == ':') {
                tail = true;
                pos++;
            }

            action_id = line.c_str()+pos+1;
            
            switch (action_id[0]) {
                case '!':
                    final_action = -2;
                    break;
                case '#':
                    final_action = -1;
                    break;
                default:
                    final_action = (int32_t)convertinginteger(action_id);
            }
            
            if (tail)
                line = line.substr(0,pos - 1);
            else
                line = line.substr(0,pos);

            //We have some meta rules
            if (metarule_available)
                replacemetas(metalines,line);
        }
        
        first = true;
        first_value = false;
        for (pos = 0; pos < line.size() && !error; pos++) {
            cc = line[pos];
            if (!currentbracket.size()) {
                error = 2;
                break;
            }
            
            switch(cc) {
                case '%':
                    //We record the first rule not starting with a character
                    cc = line[pos+1];
                    
                    if (first) {
                        //Note: current = append(current, anode) might seem redundant
                        //but it is important to call it when in a disjunction, as when
                        //the disjunction is the first element, we need to vectorize
                        //for each element that it contains. In that case, append will push
                        //the value in current and returns again current as the main root.
                        switch (cc) {
                            case 'C': //Upper-case characters
                                anode = node(act_meta, cc);
                                for (e = 'A'; e < table_character_size; e++) {
                                    if (access->c_is_upper(e))
                                        vectorization(anode, e);
                                }
                                current = append(current, anode);
                                break;
                            case 'H':
                                anode = node(act_meta, cc);
                                current = append(current, anode);
                                break;
                            case 'E': //An emoji complement cannot start a rule
                                error = 3;
                                break;
                            case 'S': //table space or CR
                                anode = node(act_meta, cc);
                                vectorization(anode, 9);
                                vectorization(anode, 10);
                                vectorization(anode, 13);
                                vectorization(anode, 32);
                                vectorization(anode, 160);
                                vectorization(anode, 0); //for other non-breaking spaces
                                current = append(current, anode);
                                break;
                            case 'a': //Alphabetical characters
                                anode = node(act_meta, cc);
                                vectorization(anode, '_');
                                for (e = 'A'; e < table_character_size; e++) {
                                    if (access->c_is_alpha(e))
                                        vectorization(anode, e);
                                }
                                current = append(current, anode);
                                break;
                            case 'c': //Lower-case characters
                                anode = node(act_meta, cc);
                                for (e = 'a'; e < table_character_size; e++) {
                                    if (access->c_is_lower(e))
                                        vectorization(anode, e);
                                }
                                current = append(current, anode);
                                break;
                            case 'd':
                                anode = node(act_meta, cc);
                                for (e = '0'; e <= '9'; e++)
                                    vectorization(anode, e);
                                current = append(current, anode);
                                break;
                            case 'e':
                                anode = node(act_meta, cc);
                                for (auto& a: access->emojis_characters->utf32_arcs)
                                    vectorization(anode, a.first);
                                current = append(current, anode);
                                break;
                            case 'h': //Greek characters
                                anode = node(act_meta, cc);
                                for (e = 913; e <= 987; e++) {
                                    if (access->c_is_alpha(e))
                                        vectorization(anode, e);
                                }
                                current = append(current, anode);
                                break;
                            case 'n':
                                anode = node(act_meta, cc);
                                vectorization(anode, 160);
                                //there are more than one non breaking space
                                vectorization(anode, 0);
                                current = append(current, anode);
                                break;
                            case 'o': {
                                anode = node(act_meta, cc);
                                for (const auto& a : operators)
                                    vectorization(anode, a);
                                current = append(current, anode);
                                break;
                            }
                            case 'p': {
                                anode = node(act_meta, cc);
                                for (const auto& a: access->vpunctuations)
                                    vectorization(anode, a);
                                current = append(current, anode);
                                break;
                            }
                            case 'r': //CR
                                anode = node(act_meta, cc);
                                vectorization(anode, 10);
                                vectorization(anode, 13);
                                current = append(current, anode);
                                break;
                            case 's': //tab or space or non-breaking spaces
                                anode = node(act_meta, cc);
                                vectorization(anode, 9);
                                vectorization(anode, 32);
                                vectorization(anode, 160);
                                vectorization(anode, 0); //for other non-breaking space characters
                                current = append(current, anode);
                                break;
                            default:
                                anode = node(act_char, cc);
                                vectorization(anode, cc);
                                current = append(current, anode);
                        }
                        pos++;
                        break;
                    }
                    
                    //This is a direct comparison or a meta
                    if (strchr(x_actions,cc) == NULL)
                        anode = node(act_char, cc);
                    else
                        anode = node(act_meta, cc);
                    
                    root = current;
                    current = append(current, anode);
                    pos++;
                    break;
                case '(':
                    //The first element of a rule can never be optional
                    if (first) {
                        currentbracket.push_back('^');
                        first_value = true;
                    }
                    
                    anode = node(act_epsilon, 0);
                    current = append(current, anode);
                    brackets.push_back(current);
                    currentbracket.push_back('(');
                    disjunction = false;
                    break;
                case ')':
                    if (brackets.size()) {
                        root = brackets.back();
                        brackets.pop_back();
                        anode = node(act_epsilon, 0);
                        root->append(anode);
                        current = append(current, anode);
                        currentbracket.pop_back();
                        first_value = false;
                        if (currentbracket.back() == '^') {
                            currentbracket.pop_back();
                            first_value = true;
                        }
                        disjunction = (currentbracket.back() == '{');
                        if (first_value) {
                            first_value = false;
                            first = true;
                            continue;
                        }
                    }
                    else
                        error = 4;
                    break;
                case '[': { //recording a sequence
                    //This is a sequence of elements
                    //If we are in a disjunction, then first_value might be true
                    //if this disjunction starts a rule.
                    //We add a specific marker to return to the right value
                    //once the structure analysis is completed.
                    if (first_value)
                        currentbracket.push_back('^');
                    currentbracket.push_back('[');
                    brackets.push_back(current);
                    current = node(act_epsilon, 0);
                    brackets.push_back(current);
                    disjunction = false;
                    break;
                }
                case ']':
                    if (brackets.size()) {
                        anode = brackets.back();
                        brackets.pop_back();
                        current = brackets.back();
                        brackets.pop_back();
                        if (current->check_negation()) {
                            //We mark the last node as a fail arc
                            current->fail_arcs();
                            anode = node(act_any, 0);
                            current->append(anode);
                        }
                        
                        root = current;
                        current->append(anode->arcs[0]);
                        currentbracket.pop_back();
                        if (currentbracket.back() == '^') {
                            //we were in a disjunction, we need to put first_value back to true
                            currentbracket.pop_back();
                            first_value = true;
                        }
                        disjunction = (currentbracket.back() == '{');
                    }
                    else
                        error = 5;
                    break;
                case '{':
                    anode = node(act_epsilon, 0);
                    disjunction = false;
                    current = append(current, anode);
                    brackets.push_back(current);
                    disjunction = true;
                    currentbracket.push_back('{');
                    //In this case, we need to dispatch elements to vectorize
                    if (first)
                        first_value = true;
                    break;
                case '}':
                    if (brackets.size()) {
                        first_value = false;
                        disjunction = false;
                        current = brackets.back();
                        brackets.pop_back();
                        root = current;
                        if (current->check_negation()) {
                            current->fail_arcs();
                            anode = node(act_any, 0);
                            current->append(anode);
                        }
                        
                        cc = line[pos+1];
                        if (cc != '+' && cc != '*') {
                            anode = node(act_epsilon, 0);
                            for (e = 0; e < current->size(); e++) {
                                current->arcs[e]->append_final(anode);
                            }
                            if (current->check_negation())
                                current->append(anode);
                            current = anode;
                        }
                        
                        currentbracket.pop_back();
                        disjunction = (currentbracket.back() == '{');
                    }
                    else
                        error = 6;
                    break;
                case '+':
                    if (pos) { //only if it is not the first character
                        //append_final appends first current, then anode
                        //on each final nodes in current (to take into account [...])
                        if (check_flag(current->action, act_epsilon)) {
                            anode = node(act_epsilon, 0);
                            for (e = 0; e < current->size(); e++) {
                                current->arcs[e]->append_final(current, anode);
                            }
                            if (current->arcs[0]->check_start_fail())
                                current->append(anode);
                        }
                        else {
                            anode = node(act_epsilon, 0);
                            current->append(current);
                            current = append(current, anode);
                        }
                        current = anode;
                        break;
                    }
                    else
                        error = 7;
                    break;
                case '*':
                    if (pos) { //only if it is not the first character
                        //append_final appends first current, then anode
                        //on each final nodes in current (to take into account [...])
                        //We need to add one more layer of node
                        if (root == NULL) {
                            error = 8;
                            break;
                        }
                        
                        if (check_flag(current->action, act_epsilon)) {
                            anode = node(act_epsilon, 0);
                            for (e = 0; e < current->size(); e++) {
                                current->arcs[e]->append_final(current, anode);
                            }
                            if (current->arcs[0]->check_start_fail())
                                current->append(anode);
                            current = anode;
                        }
                        else {
                            anode = node(act_epsilon, 0);
                            current->append(current);
                            current = append(current, anode);
                            current = anode;
                        }
                        //We add an arc to skip the whole structure since *
                        //also means 0 case...
                        root->append(anode);
                        break;
                    }
                    else
                        error = 9;
                    break;
                case '#': //introduce an interval or a code
                    //interval
                    if (line[pos+2] == '-') {
                        anode = node(act_interval, line[pos+1], line[pos+3]);
                        if (first) {
                            for (u_uchar c = line[pos+1]; c <= line[pos+3]; c++) {
                                vectorization(anode, c);
                            }
                        }
                        root = current;
                        current = append(current, anode);
                        pos += 3;
                    }
                    else {//code
                        e = 0;
                        if (isdigit(line[pos+1])) {
                            pos++;
                            e = line[pos++] - 48;
                            while (pos < line.size() && isdigit(line[pos])) {
                                e *= 10;
                                e += line[pos++] - 48;
                            }
                            pos--;
                            anode = node(act_char, e);
                            if (first)
                                vectorization(anode, e);
                            root = current;
                            current = append(current, anode);
                        }
                        else
                            error = 10;
                    }
                    break;
                case '-':
                    if (pos)
                        add_flag(current->flags, flag_skip);
                    else
                        error = 11;
                    break;
                case '~':
                    negation = flag_negation;
                    break;
                case '?':
                    anode = node(act_any, cc);
                    root = current;
                    current = append(current, anode);
                    break;
                case ' ': //in disjunction we separate each element with a space
                    if (currentbracket.back() == '{')
                        continue;
                default: {
                    //Elements are separated with blanks in disjunction
                    anode = node(act_char, cc);
                    if (first)
                        vectorization(anode , cc);
                    root = current;
                    current = append(current, anode);
                }
            }
            first = first_value;
            //In this case, we need to index on the first element
            //not the following one...
            if (first_value && (currentbracket.back() == '[' || currentbracket.back() == '('))
                first_value = false;
        }
        
        if (initial == NULL || error || brackets.size()) {
            u_ustring error_msg = U"Error in: ";
            error_msg += line;
            error_msg += U". Error is: ";
            switch (error) {
                case 1:
                    error_msg += U"missing '=' sign for label assignment.";
                    break;
                case 2:
                    error_msg += U"too many closing brackets.";
                    break;
                case 3:
                    error_msg += U"a rule cannot start with %E (emoji complement).";
                    break;
                case 4:
                    error_msg += U"Closing parenthesis ')' does not match open parenthesis '('.";
                    break;
                case 5:
                    error_msg += U"Closing bracket ']' does not match open bracket '['.";
                    break;
                case 6:
                    error_msg += U"Closing brace '}' does not match open brace '{'.";
                    break;
                case 7:
                    error_msg += U"'+' operator is not associated with any element.";
                    break;
                case 8:
                case 9:
                    error_msg += U"'*' operator is not associated with any element.";
                    break;
                case 10:
                    error_msg += U"'#' operator is not associated with a number or an interval.";
                    break;
                case 11:
                    error_msg += U"'-' operator is not associated with any element.";
                    break;
            }
            throw new Token_error(error_msg);
        }
        
        if (final_action == -2) {
            final_action = 0;
            if (action_id.size() > 1) {
                action_id = action_id.substr(1, action_id.size());
                final_action = (int32_t)convertinginteger(action_id);
            }
            add_flag(initial->flags, flag_blocking_gate);
        }
        
        anode = node(act_end, final_action);
        append(current, anode);
        
        if (displayautomata) {
            string s;
            s_unicode_to_utf8(s, line);
            cout << s << endl;
            display(initial, 0, true);
        }
        
        initial->trim_epsilon(nodes);
        if (final_action > 0)
            indexed_on_label[final_action].push_back(initial);

        if (tail) {
            anode = initial;
            while (anode->arcs.size() == 1)
                anode = anode->arcs[0];
            add_flag(anode->flags, flag_tail);
        }

        compiled_rules.push_back(initial);
        
        if (displayautomata) {
            display(initial, 0, true);
        }
    }
    
    //Cleaning useless nodes
    //Removing all marks except flag_negation, flag_skip and flag_fail
    for (auto& nd : nodes)
        nd->flags &= flag_action;
    
    //When there is only one rule in vectorization_table, no need to keep the epsilon
    vectorization_table[0]->mark_nodes();
    
    //the position 0 can never be removed, thus we start at 1
    for (e = 1; e < table_character_size; e++) {
        if (vectorization_table[e] != NULL) {
            if (vectorization_table[e]->size() == 1)
                vectorization_table[e] = vectorization_table[e]->arcs[0];
            vectorization_table[e]->mark_nodes();
        }
    }
    
    vector<tokenizer_node*> intermediary;
    for (auto& nd : nodes) {
        if (!nd->check_visited())
            delete nd;
        else {
            nd->idpos = intermediary.size();
            intermediary.push_back(nd);
        }
    }
    
    nodes = intermediary;
    loaded = true;
}

//----------------------------------------------------------------------------------------
//Rules for the LispE tokenizer
//----------------------------------------------------------------------------------------
void tokenizer_automaton::setrules() {
    /*
     a) A metarule is composed of two parts: c:expression, where c is the metacharacter that be accessed through %c and expression is a single body rule.
     
     for instance, we could have encoded %o as:
     rules.push_back(U"o:[≠ ∨ ∧ ÷ × ² ³ ¬]");
     
     IMPORTANT: These rules should be declared with one single operation.
     Their body will replace the call to a %c in other rules  (see the test on metas in the parse section)
     
     If you use a character that is already a meta-character (such as "a" or "d"), then the meta-character will be replaced with
     this new description... However, its content might still use the standard declaration:
     
     rules.push_back(U"a:{%a %d %p}"); "%1 is now a combination of alphabetical characters, digits and punctuations
     
     
     b) A rule is composed of two parts: body = action (action is either an integer or a #.)
     
     body uses the following instructions:
     
     x   is a character that should be recognized
     
     #x     comparison with character of code x...
     #x-y   comparison between x and y. x and y should be ascii characters...
     
     %x  is a meta-character with the following possibilities:
     
     ?  is any character
     %a  is any alphabetical character (including unicode ones such as éè)
     %C  is any uppercase character
     %c  is any lowercase character
     %d  is any digits
     %e  is an emoji
     %E  is an emoji complement (cannot start a rule)
     %h  is a Greek letter
     %H  is any hangul character
     %n  is a non-breaking space
     %o  is any operators
     %p  is any punctuations
     %r  is a carriage return both \n and \r
     %s  is a space (32) or a tab (09)
     %S  is both a carriage return or a space (%s or %r)
     
     %nn  you can create new metarules associated with any OTHER characters...
     
     
     (..) is a sequence of optional instructions
     [..] is a sequence of characters in a disjunction
     {..} is a disjunction of meta-characters
     x*   means that the instruction can be repeated zero or n times
     x+   means that the instruction can be repeated at least once
     x-   means that the character should be recognized but not stored in the parsing string
     ~..  means that all character will be recognized except for those in the list after the tilda.
     
     IMPORTANT: spaces are considered characters except in disjunctions
     
     Three more rules:
     
     ...=:NN -> '=:' in this context forces the evaluation to be handled iteratively, which avoids recursive explosion of the stack for very long strings
     ...=!NN -> '=!' means that the rule has been flagged with flag_blocking_gate. It is basically deactivated.
     ...=# -> the '#' means that the token will be recognized but not stored in the final list of tokens
     
     The flag_blocking_gate can be used to activate or deactivate rules on the fly
     */
    
    //Spaces, skipped in the parsing string
    rules.push_back(U"%S+=#");                    //space (skip)

    //Quoted ;
    rules.push_back(U"';+=59");                       //quote ;
    //Single quote
    rules.push_back(U"'=39");                       //quote
    
    //The opening and closing characters in LispE
    rules.push_back(U"%(=40");
    rules.push_back(U"%)=41");
    
    rules.push_back(U"%{=123");
    rules.push_back(U"%}=125");

    rules.push_back(U":=58");              //Separator in dictionaries

    rules.push_back(U"%[~%r+%]=:91");     //Unix expression
        
    //Comments
    rules.push_back(U";?+%r=:#");                      //comments starting with ; up to the end of the line
    rules.push_back(U"%#!?+%r=#");                      //specific to Linux, calling function integrated in file

    //Strings
    rules.push_back(U"\"\"\"?*\"\"\"=:96");         //long strings Python way """.."""
    rules.push_back(U"\"{[\\-\"] ~%r}*\"=:34");     //string "" does not contain CR and can escape characters
    rules.push_back(U"`?*`=:96");                   //long strings Unix way

    //The definition of a rule is divided into metarules for better clarity
    rules.push_back(U"N:{%- %+}");   //The exponential part of a hexadecimal number
    rules.push_back(U"F:{%d #A-F #a-f}");     //a hexadecimal character is one of these
    rules.push_back(U"P:{pP}(%N)%d+");   //The exponential part of a hexadecimal number
    rules.push_back(U"D:.%F+(%P)");           //The decimal part of a hexadecimal number
    rules.push_back(U"X:{eE}(%N)%d+");   //The exponential part of a decimal number
    rules.push_back(U"V:.%d+(%X)");          //the decimal part of a number
    rules.push_back(U"R:%d+(%V)");          //The real part
    rules.push_back(U"I:,(%N)(%R)i");  //Imaginary part of a number

    //Hexa decimal rules
    rules.push_back(U"%N0x%F+(%D)=57");  //hexadecimal: can handle 0x1.16bca4f9165dep-3
    rules.push_back(U"0x%F+(%D)=57");   //hexadecimal: can handle 0x1.16bca4f9165dep-3

    rules.push_back(U"%N0b{1 0}+=57");  //binaires: -0b11011
    rules.push_back(U"0b{1 0}+=57");  //binaires

    //regular numbers
    rules.push_back(U"%N%R(%I)=57"); //The second part after the "," is the imaginary part: 12.23,2i
    rules.push_back(U"%R(%I)=57");    //exponential digits
    
    rules.push_back(U"%%%d+=65");               //%132 is a token
    rules.push_back(U"%%{%o %p}=65");           //%. is a token
    rules.push_back(U"${%a %d}+=65");           //$abs23 is a token
    rules.push_back(U"%#{%a %d}+=65");          //#abs23 is a token
    
    rules.push_back(U"%o+=63");                 //operator
    rules.push_back(U"%e%E*=65");               //An emoji character
    rules.push_back(U"%H{%H %d %o}*=65");       //Asian characters (Chinese, Korean, Japanese)
    rules.push_back(U"%h{%h %d %o}*=65");       //Greek
    rules.push_back(U"%a{%a %d %o}*=65");       //Regular strings
    rules.push_back(U"%p=32");                  //punctuation
    rules.push_back(U"~{%S %o %p}+=65");           //Any combination of Unicode characters ending at a space or a punctuation
}


