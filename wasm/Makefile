################################################################
LIBBOOST = -L/usr/lib/
################################################################
COMPPLUSPLUS = em++
################ Compiler LispE #################################
SOURCE = lispe.cxx jagget.cxx eval.cxx elements.cxx tools.cxx systems.cxx maths.cxx strings.cxx functions.cxx unify.cxx\
randoms.cxx rgx.cxx composing.cxx ontology.cxx sets.cxx lists.cxx dictionaries.cxx straight_eval.cxx tensors.cxx
SOURCEASM = evaljs.cxx mainwasm.cxx console.cxx
#------------------------------------------------------------
# Two builds of LispE are produced:
#   - lispe.js + lispe.wasm (objs/): the normal version, without threads, used by the pages
#   - lispe_mt.js + lispe_mt.wasm (objs_mt/): the version with threads (-pthread), used by the
#     Web Worker that runs the workers' code. Its objects are compiled apart: -pthread changes the
#     generated code (atomics, shared memory). It defines LISPE_WASM_THREADS.
# make: both versions; make normal: lispe.wasm only; make threads: lispe_mt.wasm only
#------------------------------------------------------------
OBJECTCXX = $(SOURCE:%.cxx=objs/%.o)
OBJECTASM = $(SOURCEASM:%.cxx=objs/%.o)
OBJECTCXX_MT = $(SOURCE:%.cxx=objs_mt/%.o)
OBJECTASM_MT = $(SOURCEASM:%.cxx=objs_mt/%.o)
#------------------------------------------------------------
INCLUDES = -I../include
#-sIMPORTED_MEMORY -s SUPPORT_LONGJMP=0 -fexceptions
#WASM = -sALLOW_MEMORY_GROWTH -sINITIAL_MEMORY=47972352 -sSTACK_SIZE=20971520 -fexceptions -s EXPORT_ALL=1 -Oz -DLISPE_WASM -sASYNCIFY=1
WASM = -O3 -sEXPORT_ALL=1 -sWASM=1 -fwasm-exceptions -sALLOW_MEMORY_GROWTH -sINITIAL_MEMORY=95944704 -sSTACK_SIZE=20971520 -DNDEBUG -sASSERTIONS=0 -sEXPORTED_FUNCTIONS=[_malloc,_main,_free]
LISPWASM = -DLISPE_WASM
C++11Flag = -std=c++11 -c -w -msimd128 -DAPPLE -DPOSIXREGEX -DUNIX $(WASM) $(LISPWASM)
#------------------------------------------------------------
# The version with threads
#   -pthread: at compile time AND at link time
#   PTHREAD_POOL_SIZE: Web Workers created when the module starts, so that a thread starts
#     at once (a thread beyond the pool is created on demand, once the JS gets the hand back)
#   DEFAULT_PTHREAD_STACK_SIZE: the stack of each thread (64 KB by default, far too small for the
#     recursion of LispE): the same as the main stack. It is taken from the shared memory.
#   ENVIRONMENT=web,worker: the module is loaded in a Web Worker (and can still be tried in a page)
#   The page must be cross-origin isolated (Cross-Origin-Opener-Policy: same-origin,
#   Cross-Origin-Embedder-Policy: require-corp), otherwise SharedArrayBuffer does not exist.
#   Emscripten warns that -pthread + ALLOW_MEMORY_GROWTH may slow the JS side (not the wasm).
#   To try another allocator, with a cache per thread: add -sMALLOC=mimalloc to WASM_MT.
#   No -sEXPORT_ALL here: with -pthread and ALLOW_MEMORY_GROWTH, it copies the heap views onto Module
#     when the script loads (Module["HEAP16"]=(growMemViews(),HEAP16)...), and a thread, whose memory
#     only arrives later by a message, crashes ("Cannot read properties of undefined (reading
#     'buffer')"): the workers of the pool die. The exports used by lispe_functions.js and evaljs.cxx
#     are listed instead (EXPORTED_FUNCTIONS_MT, EXPORTED_RUNTIME_MT, LIBRARY_MT).
THREADS = -pthread -DLISPE_WASM_THREADS
EXPORTED_FUNCTIONS_MT = -sEXPORTED_FUNCTIONS=[_malloc,_main,_free,_create_lispe,_clean_lispe,_reset_lispe,_eval_lispe,_eval_indent_lispe,_eval_to_floats_lispe,_eval_to_ints_lispe,_eval_to_strings_lispe,_eval_to_int_lispe,_eval_to_float_lispe,_eval_to_string_lispe,_eval_setq_ints_lispe,_eval_setq_floats_lispe,_eval_setq_int_lispe,_eval_setq_float_lispe,_eval_setq_string_lispe,_eval_setq_strings_lispe]
EXPORTED_RUNTIME_MT = -sEXPORTED_RUNTIME_METHODS=[wasmMemory,UTF8ToString,stringToUTF8,lengthBytesUTF8]
LIBRARY_MT = '-sDEFAULT_LIBRARY_FUNCS_TO_INCLUDE=[$$handleException,$$UTF8ToString,$$stringToUTF8,$$lengthBytesUTF8]'
WASM_MT = -O3 -sWASM=1 -fwasm-exceptions -sALLOW_MEMORY_GROWTH -sINITIAL_MEMORY=95944704 -sSTACK_SIZE=20971520 -DNDEBUG -sASSERTIONS=0 $(EXPORTED_FUNCTIONS_MT) $(EXPORTED_RUNTIME_MT) $(LIBRARY_MT) -pthread -sPTHREAD_POOL_SIZE=4 -sDEFAULT_PTHREAD_STACK_SIZE=20971520 -sENVIRONMENT=web,worker
#------------------------------------------------------------
objs/%.o: %.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDES) $< -o $@

objs/%.o: ../src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(INCLUDES) $< -o $@

objs_mt/%.o: %.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(THREADS) $(INCLUDES) $< -o $@

objs_mt/%.o: ../src/%.cxx
	$(COMPPLUSPLUS) $(C++11Flag) $(THREADS) $(INCLUDES) $< -o $@
#------------------------------------------------------------
all: normal threads

normal: installasm $(OBJECTCXX) $(OBJECTASM)
		$(COMPPLUSPLUS) $(OBJECTCXX) $(OBJECTASM) -o lispe.html $(WASM) -ldl -lpthread $(LIBBOOST)

threads: installmt $(OBJECTCXX_MT) $(OBJECTASM_MT)
		$(COMPPLUSPLUS) $(OBJECTCXX_MT) $(OBJECTASM_MT) -o lispe_mt.js $(WASM_MT) -ldl $(LIBBOOST)

installasm:
	mkdir -p objs

installmt:
	mkdir -p objs_mt

clean:
	rm -Rf objs objs_mt

cleanall:
	rm -Rf objs objs_mt
	rm -f lispe.html
	rm -f lispe.js
	rm -f lispe.wasm
	rm -f lispe_mt.js
	rm -f lispe_mt.wasm

.PHONY: all normal threads installasm installmt clean cleanall
