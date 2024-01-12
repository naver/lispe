### This version compiles with the C BLAS library (apt-get install libblas-dev)
include ../Makefile.in
include Makefile.loc
#------------------------------------------------------------
sourceblas = $(FILE_BLAS)
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objectblas = $(sourceblas:%.cxx=$(LOBJPATH)/blas/%.o)
#------------------------------------------------------------
C++11Flag = -std=c++11 -fPIC -w -c $(COPTION) -DUNIX $(IBLAS)
LIBPATH = ../bin
#------------------------------------------------------------
INCLUDEPATH = -I../include -Iinclude
#------------------------------------------------------------
$(LOBJPATH)/blas/%.o: src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDEPATH) $< -o $@
#------------------------------------------------------------
blas: $(objectblas)
	$(COMPPLUSPLUS) -shared -o $(LBINPATH)/liblispe_blas.so $(objectblas) -L$(LBINPATH) -llispe -ldl $(LIBBOOST) $(BLAS) -lpthread

install:
	mkdir -p $(LOBJPATH)/blas

all: install blas
	$(blas)

clean:
	rm -f $(LOBJPATH)/blas/*.o

