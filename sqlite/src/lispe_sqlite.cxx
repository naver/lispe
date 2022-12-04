/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_sqlite.cxx


/*
 This the template for new 'extensions' in lispe
 */

#include "sqlite3.h"
#include "lispe.h"

#ifdef WIN32
#if (_MSC_VER >= 1900)
#pragma comment(lib, "legacy_stdio_definitions.lib")
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif
#endif

class Lispe_sqlite;
class LispE_sqlite_iteration {
public:
    LispE* lisp;
    Lispe_sqlite* value;
    sqlite3_stmt *stmt;
    int returncode;
    vector<string> columnnames;
    int columnCount;
    Dictionary* currentrow;
    int nb;

    void Storevalue();

    LispE_sqlite_iteration(LispE* lsp, Lispe_sqlite* v);

    Element* Key() {
        return lisp->provideInteger(nb);
    }

    Element* Value();
    void Next();

    bool End();
    Element* Begin();
};

class Lispe_sqlite : public Element {
public:

    string command;
    string dbname;
    sqlite3 *db;
    string sqlcommand;
    string bindcommand;
    sqlite3_stmt* stmt;
    vector<int> insertstructure;


    //---------------------------------------------------------------------------------------------------------------------
    Lispe_sqlite(short ty) : Element(ty) {
        dbname = "";
        db = NULL;
        sqlcommand = "";
        stmt = NULL;
    }

    ~Lispe_sqlite() {
        if (db != NULL)
            sqlite3_close(db);
    }


    //We use this instruction to return a description of the instruction
    //Indeed, just do: (print sqlite_example) to get this information
    wstring asString(LispE* lisp) {
        wstring cmd;
        s_utf8_to_unicode(cmd, dbname, dbname.size());
        return cmd;
    }

    bool Boolean() {
        return (db != NULL);
    }

    Element* sqliteOpen(LispE* lisp);
    Element* sqliteClose(LispE* lisp);
    Element* sqliteCreate(LispE* lisp);
    Element* sqliteInsert(LispE* lisp);
    Element* sqliteProcess(LispE* lisp);
    Element* sqliteRun(LispE* lisp);
    Element* sqliteExecute(LispE* lisp);
    Element* sqliteBegin(LispE* lisp);
    Element* sqliteCommit(LispE* lisp);

    Element* loop(LispE* lisp, short label, List* code);
};


static string Quotting(string& si) {
    string s = "'";
    for (int i = 0; i < si.size(); i++) {
        if (si[i] == '\'')
            s += "'";
        s += si[i];
    }
    s += "'";
    return s;
}

class Couple_LispE {
public:

    LispE* lisp;
    List* results;

    Couple_LispE(LispE* lsp) {
        lisp = lsp;
        results = new List;
    }
};

static int callback(void *asql, int argc, char **argv, char **azColName){
    int i;
    Couple_LispE* cpl = (Couple_LispE*)asql;
    //We use our first parameter as the place where to store our stuff
    List* vresults = cpl->results;
    LispE* lisp = cpl->lisp;

    Dictionary* kmap = new Dictionary;
    u_ustring wkey;
    string key;
    string value;
    for (i = 0; i < argc; i++) {
        key = azColName[i];
        s_utf8_to_unicode(wkey, key, key.size());
        if (argv[i] == NULL)
            kmap->dictionary[wkey] = emptystring_;
        else {
            value = argv[i];
            kmap->dictionary[wkey] = lisp->provideString(value);
        }
        wkey = U"";
    }
    vresults->append(kmap);
    return 0;
}



LispE_sqlite_iteration::LispE_sqlite_iteration(LispE* lsp, Lispe_sqlite* v) {
    lisp = lsp;
    value = v;
    currentrow = NULL;
    columnCount = 0;
    nb = -1;
    stmt = NULL;
}

Element* LispE_sqlite_iteration::Value() {
    if (currentrow == NULL)
        return null_;
    return currentrow;
}

void LispE_sqlite_iteration::Next() {
    Storevalue();
}

bool LispE_sqlite_iteration::End() {
    if (returncode != SQLITE_ROW) {
        sqlite3_finalize(stmt);
        if (currentrow != NULL)
            currentrow->decrement();
        return true;
    }
    return false;
}

Element* LispE_sqlite_iteration::Begin() {
    nb = -1;
    int rc = sqlite3_prepare_v2(value->db, STR(value->sqlcommand), -1, &stmt, 0);
    if (rc != SQLITE_OK) {
        string message = "Wrong statement: ";
        message += value->sqlcommand;
        throw new Error(message);
    }

    //we get the number of columns matching the sql command
    columnCount = sqlite3_column_count(stmt);


    //we then store their names
    char* name;
    char supername[100];
    for (int i = 0; i < columnCount; i++) {
        name = (char*)sqlite3_column_name(stmt, i);
        if (name == NULL) {
            sprintf_s(supername, 100, "column%d", i);
            columnnames.push_back(supername);
        }
        else
            columnnames.push_back(name);
    }

    Storevalue();
    return true_;
}

void LispE_sqlite_iteration::Storevalue() {
    //We can then relieve the previous element
    returncode = sqlite3_step(stmt);
    if (currentrow != NULL) {
        currentrow->decrement();
        currentrow = NULL;
    }

    if (returncode != SQLITE_ROW)
        return;

    currentrow = new Dictionary;
    currentrow->increment();
    Element* k;
    string txt;
    u_ustring wkey;
    string key;

    for (int i = 0; i < columnCount; i++) {
        key = columnnames[i];
        s_utf8_to_unicode(wkey, key, key.size());
        switch (sqlite3_column_type(stmt, i)) {
        case SQLITE_INTEGER:
            k = lisp->provideInteger(sqlite3_column_int(stmt, i));
            currentrow->recording(wkey, k);
            break;
        case SQLITE_BLOB:
        case SQLITE_TEXT:
            txt = (char*)sqlite3_column_text(stmt, i);
            k = lisp->provideString(txt);
            currentrow->recording(wkey, k);
            break;
        case SQLITE_FLOAT:
            k = lisp->provideNumber(sqlite3_column_double(stmt, i));
            currentrow->recording(wkey, k);
            break;
        default:
            currentrow->recording(wkey, null_);
        }
        wkey = U"";
    }
    nb++;
}

Element* Lispe_sqlite::sqliteOpen(LispE* lisp) {
    if (db != NULL)
        throw new Error("SQLite(800): A database has already been opened with this object");
    //the first parameter is the dbname
    Element* kelement = lisp->get_variable(U"dbname");
    dbname = kelement->toString(lisp);
    int rc = sqlite3_open(STR(dbname), &db);
    if (rc) {
        string message = "SQLite(801): Error opening database='";
        message += dbname;
        message += "' --> ";
        message += sqlite3_errmsg(db);
        throw new Error(message);
    }
    return true_;
}

Element* Lispe_sqlite::sqliteClose(LispE* lisp) {
    if (db == NULL)
        throw new Error("SQLite(802): Cannot close this database");
    sqlite3_close(db);
    db = NULL;
    return true_;
}

Element* Lispe_sqlite::sqliteCreate(LispE* lisp) {
    if (db == NULL)
        throw new Error("SQLite(803): Cannot use this database");
    //A typical call would be:
    // mydb.create("table1","name TEXT","age INTEGER",12);
    command = "create table ";
    //The first parameter is the table name
    Element* table = lisp->get_variable(U"table");
    command += table->toString(lisp);
    command += " (";
    Element* callargs = lisp->get_variable(U"args");
    //The next parameters are the rest of the table description
    for (int i = 0; i < callargs->size(); i++) {
        table = callargs->index(i);
        if (i)
            command += ",";
        command += table->toString(lisp);
    }
    command += ");";
    char *zErrMsg = NULL;
    int rc = sqlite3_exec(db, STR(command), NULL, 0, &zErrMsg);
    if (rc != SQLITE_OK) {
        command += "SQLite(805): Create error=";
        command += zErrMsg;
        sqlite3_free(zErrMsg);
        throw new Error(command);
    }
    if (zErrMsg != NULL)
        sqlite3_free(zErrMsg);
    return true_;
}

Element* Lispe_sqlite::sqliteInsert(LispE* lisp) {
    if (db == NULL)
        throw new Error("SQLite(803): Cannot use this database");
    string lacommande = "insert into ";
    //A typical call would be:
    // mydb.insert("table1","name","toto","age",12);
    //The first parameter is the table name
    Element* table = lisp->get_variable(U"table");
    lacommande += table->toString(lisp);
    lacommande += " (";
    Element* callfunc = lisp->get_variable(U"columns");
    //One parameter our of two is column name
    for (int i = 0; i < callfunc->size(); i += 2) {
        table = callfunc->index(i);
        if (i)
            lacommande += ",";
        lacommande += table->toString(lisp);
    }
    lacommande += ") values (";
    string s;
    //One parameter out of two is a value associated to a column name
    for (int i = 1; i < callfunc->size(); i += 2) {
        table = callfunc->index(i);
        if (i != 1)
            lacommande += ",";
        s = table->toString(lisp);
        s = Quotting(s);
        lacommande += s;
    }
    lacommande += ");";
    char *zErrMsg = NULL;
    int rc = sqlite3_exec(db, STR(lacommande), NULL, 0, &zErrMsg);
    if (rc != SQLITE_OK) {
        lacommande += "SQLite(809): Create error=";
        lacommande += zErrMsg;
        sqlite3_free(zErrMsg);
        throw new Error(lacommande);
    }
    if (zErrMsg != NULL)
        sqlite3_free(zErrMsg);
    return true_;
}

Element* Lispe_sqlite::sqliteProcess(LispE* lisp) {
    if (db == NULL)
        throw new Error("SQLite(803): Cannot use this database");
    Element* kcommand = lisp->get_variable(U"command");
    sqlcommand = kcommand->toString(lisp);
    return true_;
}

Element* Lispe_sqlite::sqliteRun(LispE* lisp) {
    if (db == NULL)
        throw new Error("SQLite(803): Cannot use this database");
    Element* kcommand = lisp->get_variable(U"command");
    sqlcommand = kcommand->toString(lisp);

    Couple_LispE cpl(lisp);
    char* errmsg;
    int rc = sqlite3_exec(db, STR(sqlcommand), callback, &cpl, &errmsg);
    if (rc != SQLITE_OK) {
        sqlcommand += "SQLite(811): Execute error=";
        sqlcommand += errmsg;
        throw new Error(sqlcommand);
    }
    return cpl.results;
}


Element* Lispe_sqlite::sqliteExecute(LispE* lisp) {
    if (db == NULL)
        throw new Error("SQLite(803): Cannot use this database");
    Element* kcommand = lisp->get_variable(U"command");
    sqlcommand = kcommand->toString(lisp);
    char* errmsg;
    int rc = sqlite3_exec(db, STR(sqlcommand), NULL, 0, &errmsg);
    if (rc != SQLITE_OK) {
        sqlcommand += "SQLite(811): Execute error=";
        sqlcommand += errmsg;
        throw new Error(sqlcommand);
    }
    return true_;
}

Element* Lispe_sqlite::sqliteBegin(LispE* lisp) {
    if (db == NULL)
        throw new Error("SQLite(803): Cannot use this database");
    string mode = lisp->get_variable(U"mode")->toString(lisp);
    s_trim(mode);
    //A typical call would be:
    // mydb.create("table1","name TEXT","age INTEGER",12);
    command = "BEGIN";
    if (mode != "") {
        command += " ";
        command += mode;
    }
    command += " TRANSACTION;";
    bindcommand = "ok";
    stmt = NULL;
    //command+=" TRANSACTION;";
    char *zErrMsg = NULL;
    int rc = sqlite3_exec(db, STR(command), NULL, 0, &zErrMsg);
    if (rc != SQLITE_OK) {
        command += "SQLite(805): 'BEGIN' error=";
        command += zErrMsg;
        sqlite3_exec(db, "ROLLBACK", 0, 0, 0);
        sqlite3_free(zErrMsg);
        throw new Error(command);
    }
    if (zErrMsg != NULL)
        sqlite3_free(zErrMsg);
    return true_;
}

Element* Lispe_sqlite::sqliteCommit(LispE* lisp) {
    if (db == NULL)
        throw new Error("SQLite(803): Cannot use this database");
    //A typical call would be:
    // mydb.create("table1","name TEXT","age INTEGER",12);
    command = "COMMIT;";
    char *zErrMsg = NULL;
    int rc = sqlite3_exec(db, STR(command), NULL, 0, &zErrMsg);
    if (rc != SQLITE_OK) {
        command += "SQLite(807): 'COMMIT' error=";
        command += zErrMsg;
        sqlite3_free(zErrMsg);
        throw new Error(command);
    }
    bindcommand = "";
    if (stmt != NULL)
        sqlite3_finalize(stmt);
    stmt = NULL;
    if (zErrMsg != NULL)
        sqlite3_free(zErrMsg);
    return true_;
}


Element* Lispe_sqlite::loop(LispE* lisp, short label, List* code) {
    long i_loop;
    Element* e = null_;
    lisp->recording(null_, label);
    long sz = code->liste.size();
    LispE_sqlite_iteration sqliter(lisp, this);
    sqliter.Begin();
    //We then execute our instructions
    while (!sqliter.End()) {
        e = sqliter.Value();
        lisp->recording(e, label);
        e = null_;
        for (i_loop = 3; i_loop < sz && e->type != l_return; i_loop++) {
            e->release();
            e = code->liste[i_loop]->eval(lisp);
        }
        if (e->type == l_return) {
            if (e->isBreak())
                return null_;
            return e;
        }
        sqliter.Next();
    }
    return e;
}


typedef enum {lsql_sql, lsql_open, lsql_close, lsql_create, lsql_insert, lsql_run, lsql_execute, lsql_process, lsql_begin, lsql_commit} lsql_type;

class Lispe_sqlite_method : public Element {
public:
    lsql_type action;
    short sqlite_var;

    Lispe_sqlite_method(short ty, short var, lsql_type a) : action(a), sqlite_var(var), Element(ty) {}

    Element* eval(LispE* lisp) {
        switch (action) {
            case lsql_sql:
                return new Lispe_sqlite(type);
            case lsql_open: {
                Element* sql = lisp->get_variable(sqlite_var);
                if (sql->type != type)
                    throw new Error("Error: wrong type for the first argument. Expect a 'sqlite' object");
                return ((Lispe_sqlite*)sql)->sqliteOpen(lisp);
            }
            case lsql_close: {
                Element* sql = lisp->get_variable(sqlite_var);
                if (sql->type != type)
                    throw new Error("Error: wrong type for the first argument. Expect a 'sqlite' object");
                return ((Lispe_sqlite*)sql)->sqliteClose(lisp);
            }
            case lsql_create: {
                Element* sql = lisp->get_variable(sqlite_var);
                if (sql->type != type)
                    throw new Error("Error: wrong type for the first argument. Expect a 'sqlite' object");
                return ((Lispe_sqlite*)sql)->sqliteCreate(lisp);
            }
            case lsql_insert: {
                Element* sql = lisp->get_variable(sqlite_var);
                if (sql->type != type)
                    throw new Error("Error: wrong type for the first argument. Expect a 'sqlite' object");
                return ((Lispe_sqlite*)sql)->sqliteInsert(lisp);
            }
            case lsql_run: {
                Element* sql = lisp->get_variable(sqlite_var);
                if (sql->type != type)
                    throw new Error("Error: wrong type for the first argument. Expect a 'sqlite' object");
                return ((Lispe_sqlite*)sql)->sqliteRun(lisp);
            }
            case lsql_process: {
                Element* sql = lisp->get_variable(sqlite_var);
                if (sql->type != type)
                    throw new Error("Error: wrong type for the first argument. Expect a 'sqlite' object");
                return ((Lispe_sqlite*)sql)->sqliteProcess(lisp);
            }
            case lsql_execute: {
                Element* sql = lisp->get_variable(sqlite_var);
                if (sql->type != type)
                    throw new Error("Error: wrong type for the first argument. Expect a 'sqlite' object");
                return ((Lispe_sqlite*)sql)->sqliteExecute(lisp);
            }
            case lsql_begin: {
                Element* sql = lisp->get_variable(sqlite_var);
                if (sql->type != type)
                    throw new Error("Error: wrong type for the first argument. Expect a 'sqlite' object");
                return ((Lispe_sqlite*)sql)->sqliteBegin(lisp);
            }
            case lsql_commit: {
                Element* sql = lisp->get_variable(sqlite_var);
                if (sql->type != type)
                    throw new Error("Error: wrong type for the first argument. Expect a 'sqlite' object");
                return ((Lispe_sqlite*)sql)->sqliteCommit(lisp);
            }
        }
        return null_;
    }

    wstring asString(LispE* lisp) {
        switch (action) {
            case lsql_sql:
                return L"Create a SQLite object";
            case lsql_open:
                return L"open a database";
            case lsql_close:
                return L"close a database";
            case lsql_create:
                return L"create a table in a database with the arguments tablename,col1,col2:(create sql \"table1\" \"name TEXT PRIMARY KEY\")";
            case lsql_insert:
                return L"insert a line in a table: (insert sql \"table1\" \"name\" nm \"age\" i)";
            case lsql_process:
                return L"process a sql command with results handled in a 'loop'";
            case lsql_run:
                return L"execute a sql command";
            case lsql_execute:
                return L"execute a raw sql command.";
            case lsql_begin:
                return L"to enter commit mode: default mode is DEFERRED other modes are: IMMEDIATE and EXCLUSIVE";
            case lsql_commit:
                return L"the SQL command are then processed. It should finish a series of commands initiated with a begin";
        }
        return L"";
    }
};

extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {
    //We first create the body of the function
    wstring w = L"sqlite_";
    short sql_type = lisp->encode(w);
    w = L"sqlitev";
    short sql_type_var = lisp->encode(w);

    lisp->extension("deflib sqlite()", new Lispe_sqlite_method(sql_type, sql_type_var, lsql_sql));
    lisp->extension("deflib sqlite_open(sqlitev dbname)", new Lispe_sqlite_method(sql_type, sql_type_var, lsql_open));
    lisp->extension("deflib sqlite_close(sqlitev)", new Lispe_sqlite_method(sql_type, sql_type_var, lsql_close));
    lisp->extension("deflib sqlite_create(sqlitev table (() args))", new Lispe_sqlite_method(sql_type, sql_type_var, lsql_create));
    lisp->extension("deflib sqlite_insert(sqlitev table (() columns))", new Lispe_sqlite_method(sql_type, sql_type_var, lsql_insert));
    lisp->extension("deflib sqlite_process(sqlitev command)", new Lispe_sqlite_method(sql_type, sql_type_var, lsql_process));
    lisp->extension("deflib sqlite_run(sqlitev command)", new Lispe_sqlite_method(sql_type, sql_type_var, lsql_run));
    lisp->extension("deflib sqlite_execute(sqlitev command)", new Lispe_sqlite_method(sql_type, sql_type_var, lsql_execute));
    lisp->extension("deflib sqlite_begin(sqlitev mode)", new Lispe_sqlite_method(sql_type, sql_type_var, lsql_begin));
    lisp->extension("deflib sqlite_commit(sqlitev)", new Lispe_sqlite_method(sql_type, sql_type_var, lsql_commit));
    return true;
}
}

