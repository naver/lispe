################################################################
LIBBOOST = -L/usr/lib/
################################################################
COMPPLUSPLUS = em++
################ Compiler LispE #################################
SOURCE = lispe.cxx jagget.cxx eval.cxx elements.cxx tools.cxx systems.cxx maths.cxx strings.cxx functions.cxx unify.cxx\
randoms.cxx rgx.cxx composing.cxx ontology.cxx sets.cxx lists.cxx dictionaries.cxx straight_eval.cxx tensors.cxx
SOURCEASM = evaljs.cxx mainwasm.cxx console.cxx
#------------------------------------------------------------
OBJECTCXX = $(SOURCE:%.cxx=objs/%.o)
OBJECTMAIN = $(SOURCEMAIN:%.cxx=objs/%.o)
OBJECTASM = $(SOURCEASM:%.cxx=objs/%.o)
#------------------------------------------------------------
INCLUDES = -I../include
#-sIMPORTED_MEMORY -s SUPPORT_LONGJMP=0 -fexceptions
#WASM = -sALLOW_MEMORY_GROWTH -sINITIAL_MEMORY=47972352 -sSTACK_SIZE=20971520 -fexceptions -s EXPORT_ALL=1 -Oz -DLISPE_WASM -sASYNCIFY=1
WASM = -O3 -sEXPORT_ALL=1 -sWASM=1 -fexceptions -sINITIAL_MEMORY=47972352 -sSTACK_SIZE=20971520 -DNDEBUG -sASSERTIONS=0 -sEXPORTED_FUNCTIONS=[_malloc,_main,_free] -sASYNCIFY=1
LISPWASM = -DLISPE_WASM
C++11Flag = -std=c++11 -c -w -DAPPLE -DPOSIXREGEX -DUNIX $(WASM) $(LISPWASM)
#------------------------------------------------------------
objs/%.o: %.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDES) $< -o $@

objs/%.o: ../src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDES) $< -o $@
#------------------------------------------------------------
all: installasm $(OBJECTCXX) $(OBJECTASM)
		$(COMPPLUSPLUS) $(OBJECTCXX) $(OBJECTASM) -o lispe.html $(WASM) -ldl -lpthread $(LIBBOOST)

installasm:
	mkdir -p objs

clean:
	rm -Rf objs

cleanall:
	rm -Rf objs
	rm lispe.html
	rm lispe.js
	rm lispe.wasm

