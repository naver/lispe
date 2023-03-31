################################################################
LIBBOOST = -L/usr/lib/
################################################################
COMPPLUSPLUS = em++
################ Compiler LispE #################################
SOURCE = lispe.cxx jagget.cxx eval.cxx elements.cxx tools.cxx systems.cxx maths.cxx strings.cxx\
randoms.cxx rgx.cxx composing.cxx ontology.cxx sets.cxx lists.cxx dictionaries.cxx straight_eval.cxx
SOURCEASM = mainwasm.cxx
#------------------------------------------------------------
OBJECTCXX = $(SOURCE:%.cxx=objs/%.o)
OBJECTMAIN = $(SOURCEMAIN:%.cxx=objs/%.o)
OBJECTASM = $(SOURCEASM:%.cxx=objs/%.o)
#------------------------------------------------------------
INCLUDES = -Iinclude
#-sIMPORTED_MEMORY -s SUPPORT_LONGJMP=0 -fexceptions
#WASM = -sALLOW_MEMORY_GROWTH -sINITIAL_MEMORY=47972352 -sSTACK_SIZE=20971520 -fexceptions -s EXPORT_ALL=1 -Oz -DLISPE_WASM
WASM = -O3 -sEXPORT_ALL=1 -sWASM=1 -fexceptions -sINITIAL_MEMORY=47972352 -sSTACK_SIZE=20971520 -DLISPE_WASM
C++11Flag = -std=c++11 -c -w -DAPPLE -DPOSIXREGEX -DUNIX $(WASM)
#------------------------------------------------------------
objs/%.o: %.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDES) $< -o $@

objs/%.o: src/%.cxx
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

