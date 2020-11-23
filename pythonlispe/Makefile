include ../Makefile.in
###################################################################
COMPPLUSPLUS = g++
#------------------------------------------------------------
sourcepython = lispe_python.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objectpython = $(sourcepython:%.cxx=$(LOBJPATH)/python/%.o)
#------------------------------------------------------------
C++11Flag = -std=c++11 -fPIC -w -c -O3 -DUNIX
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = $(INCLUDEPYTHON) -I../include -Iinclude
#------------------------------------------------------------
$(LOBJPATH)/python/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
all: install $(objectpython)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/pylispe.so $(objectpython) $(PYTHONLIB) -L$(LBINPATH) -llispe -ldl -lpthread $(LIBBOOST)

install:
	mkdir -p $(LOBJPATH)/python

clean:
	rm -f $(LOBJPATH)/python/*.o

