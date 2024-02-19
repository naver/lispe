include ../Makefile.in
#------------------------------------------------------------
sourceblas = lispe_blas.cxx
#------------------------------------------------------------
LBINPATH = ../bin
LOBJPATH = ../objs
#------------------------------------------------------------
objectblas = $(sourceblas:%.cxx=$(LOBJPATH)/blas/%.o)
#------------------------------------------------------------
C++11Flag = -std=c++11 -fPIC -w -c $(COPTION) -DUNIX -DACCELERATE_NEW_LAPACK -DACCELERATE_LAPACK_ILP64
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
	cp $(BLASLIB) $(LBINPATH)

clean:
	rm -f $(LOBJPATH)/blas/*.o

