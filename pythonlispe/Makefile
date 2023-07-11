include ../Makefile.in
###################################################################
sourcepython = lispe_python.cxx
testpython = test.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objectpython = $(sourcepython:%.cxx=$(LOBJPATH)/python/%.o)
objecttest = $(testpython:%.cxx=$(LOBJPATH)/python/%.o)
#------------------------------------------------------------
C++11Flag = -std=c++11 -fPIC -w -c $(COPTION) -DUNIX
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = $(INCLUDEPYTHON) -I../include -Iinclude
#------------------------------------------------------------
$(LOBJPATH)/python/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
all: install $(objectpython)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/pylispe.so $(objectpython) $(PYTHONLIB) -L$(LBINPATH) -llispe -ldl -lpthread $(LIBBOOST)

test: install $(objecttest)
	$(COMPPLUSPLUS) -o $(LBINPATH)/testpy $(objecttest) $(PYTHONLIB) -L$(LBINPATH) -ldl -lpthread $(LIBBOOST)

install:
	mkdir -p $(LOBJPATH)/python

clean:
	rm -f $(LOBJPATH)/python/*.o

