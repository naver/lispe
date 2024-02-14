################################################################
CPPLUSPLUS = g++
#CPPLUSPLUS = clang++

################ LIB EDITOR #########################################
SOURCEEDITOR = editor.cxx jag.cxx jagget.cxx jagrgx.cxx tokenize.cxx tools.cxx

################ MYEDITOR AND SHELL #################################

SOURCEMYEDITOR = editormain.cxx

SOURCESHELL = shellmain.cxx

################ FILES TO COMPILE LISP MINI LIBRARY #################
SOURCELIB = minilisp.cxx eval.cxx compiling.cxx tokens.cxx tools.cxx

################ WITH A RICH EDITOR #################################
SOURCEMINI = minimain.cxx

################ WITH A VERY POOR ONE ###############################
SOURCELISP = lispminimain.cxx

#------------------------------------------------------------
OBJECTEDITOR = $(SOURCEEDITOR:%.cxx=objs/editor/%.o)
#------------------------------------------------------------
OBJECTMYEDITOR = $(SOURCEMYEDITOR:%.cxx=objs/myeditor/%.o)
OBJECTSHELL = $(SOURCESHELL:%.cxx=objs/shell/%.o)
#------------------------------------------------------------
OBJECTMINI = $(SOURCEMINI:%.cxx=objs/mini/%.o)
OBJECTLISP = $(SOURCELISP:%.cxx=objs/lisp/%.o)
OBJECTLIB = $(SOURCELIB:%.cxx=objs/lisp/%.o)
#------------------------------------------------------------

# Add this option to C++FLAGS below if your terminal is a VT100
#to properly handle mouse control...
# otherwise, you can use -vt100 as an option when launching the editor
# to test if mouse control works better.

# VTERM_MOUSE = -DXTERM_MOUSE_VT100

DEBUGMODE = -g -DDEBUGGER
MODEFAST = -O3
MODECLANG = -Ofast

# Choose DEBUGMODE instead of MODEFAST if you need to check memory leaks
OPTIONS = $(MODEFAST)

#------------------------------------------------------------
## For REGEX, if your compiler is very ancient, it might rely on boost
# Uses the option: BOOSTPOSIXREGEX
# For more modern compilers, the default option is POSIXREGEX (Regex is now available as a STL template)

#REGEX = -DBOOSTPOSIXREGEX
REGEX = -DPOSIXREGEX
#------------------------------------------------------------

# Some compilers need the following options
#C++FLAGS = -std=c++0x -w -c $(OPTIONS) $(VTERM_MOUSE) $(REGEX)
#C++FLAGS = -std=gnu++0x -w -c $(OPTIONS) $(VTERM_MOUSE) $(REGEX)
C++FLAGS =  -fPIC -std=c++11 -w -c $(OPTIONS) $(VTERM_MOUSE) $(REGEX)
#------------------------------------------------------------
objs/editor/%.o: src/%.cxx
	$(CPPLUSPLUS) $(C++FLAGS) -Iinclude $< -o $@

#------------------------------------------------------------
objs/myeditor/%.o: src/%.cxx
	$(CPPLUSPLUS) $(C++FLAGS) -Iinclude $< -o $@

objs/shell/%.o: src/%.cxx
	$(CPPLUSPLUS) $(C++FLAGS) -Iinclude $< -o $@
#------------------------------------------------------------
objs/mini/%.o: src/%.cxx
	$(CPPLUSPLUS) $(C++FLAGS) -Iinclude $< -o $@

objs/lisp/%.o: src/%.cxx
	$(CPPLUSPLUS) $(C++FLAGS) -Iinclude $< -o $@
#------------------------------------------------------------
libeditor: $(OBJECTEDITOR)
	ar rcs bin/libeditor.a $(OBJECTEDITOR)
	ranlib bin/libeditor.a
#------------------------------------------------------------
# For those who prefer a small executable linked with a dynamic library
myeditor: installbase libeditor $(OBJECTMYEDITOR)
	$(CPPLUSPLUS) -o bin/myeditor $(OBJECTMYEDITOR) -ldl -lpthread -Lbin -leditor

shell: installbase libeditor $(OBJECTSHELL)
	$(CPPLUSPLUS) -o bin/shelleditor $(OBJECTSHELL) -ldl -lpthread -Lbin -leditor

#------------------------------------------------------------
# With lib mini
#------------------------------------------------------------
libmini: installlisp $(OBJECTLIB)
	ar rcs bin/libminilisp.a $(OBJECTLIB)
	ranlib bin/libminilisp.a

mini: installlisp libmini libeditor $(OBJECTMINI)
	$(CPPLUSPLUS) -o bin/minilisp $(OBJECTMINI) -ldl -lpthread  -Lbin -lminilisp -leditor

lispmini: installlisp libmini $(OBJECTLISP)
	$(CPPLUSPLUS) -o bin/lisp $(OBJECTLISP) -ldl -lpthread -Lbin -lminilisp
#------------------------------------------------------------
installbase:
	mkdir -p bin
	mkdir -p objs
	mkdir -p objs/editor
	mkdir -p objs/myeditor
	mkdir -p objs/shell

installlisp:
	mkdir -p bin
	mkdir -p objs
	mkdir -p objs/editor
	mkdir -p objs/mini
	mkdir -p objs/lisp

clean:
	rm -Rf objs
	rm -Rf bin
