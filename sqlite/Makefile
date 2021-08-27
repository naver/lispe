include ../Makefile.in
###################################################################
COMPPLUSPLUS = g++
#------------------------------------------------------------
sourcesqlite = lispe_sqlite.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objectsqlite = $(sourcesqlite:%.cxx=$(LOBJPATH)/sqlite/%.o)
#------------------------------------------------------------
C++11Flag = -std=c++11 -fPIC -w -c $(COPTION) -DUNIX -DSQLITE_ENABLE_COLUMN_METADATA -DSQLITE_THREADSAFE
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = -I../include -Iinclude
#------------------------------------------------------------
$(LOBJPATH)/sqlite/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
all: install $(objectsqlite)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/liblispe_sqlite.so $(objectsqlite) -L$(LBINPATH) $(LIBBOOST) $(SQLITELIB) -llispe -ldl -lpthread

install:
	mkdir -p $(LOBJPATH)/sqlite

clean:
	rm -f $(LOBJPATH)/sqlite/*.o

