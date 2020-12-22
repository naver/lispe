# The directories to check are: /usr/lib64, /usr/lib or /usr/lib/x86_64-linux-gnu/

import sys
import os
from os import walk
import subprocess

############################
ostype = subprocess.Popen("uname", stdout=subprocess.PIPE).stdout.read()
ostype=ostype.strip()
############################
# if Mac OS
if ostype == "Darwin":
    print("This is a Mac OS platform\n")
    f=open("Makefile.in", "w")
    f.write("CURLLIB = -lcurl\n")
    f.write("SQLITELIB = -lsqlite3\n")
    f.write("REGEX = -DPOSIXREGEX\n")
    f.write("LIBBOOST = -L/usr/lib/\n")
    f.write("INCLUDEPYTHON = -I/Library/Frameworks/Python.framework/Versions/3.8/Headers\n")
    f.write("PYTHONLIB = /Library/Frameworks/Python.framework/Versions/3.8/Python\n")
    exit(-1)

libpaths = ["/usr/", "/usr/lib/x86_64-linux-gnu/","/usr/lib64/","/usr/lib/"]
libincludes = ["/usr/include/", "/usr/include/x86_64-linux-gnu/", "/usr/include/", "/usr/include/"]

pathboost = ""
nameboost = ""

# On recherche le chemin ou boost regex peut-etre
def cherchelib(lalib):
    biblios = []
    boost = False
    libpath = None
    version = ""
    ilib = 0
    for lb in libpaths:
        fd = []
        for (dirpath, dirnames, filenames) in walk(lb):
            fd.extend(filenames)
            break

        if fd != []:
            if len(fd) > len(biblios):
                biblios = fd
                libpath = lb
                name = ""
                for libname in biblios:
                    if lalib in libname:
                        if libname[-2:] == '.a':
                            continue
                        if name == "":
                           name = libname
                        elif len(name) > len(libname):
                            name = libname
                        # we need to find the python version
                        bversion = libname.find(lalib)
                        eversion = libname.rfind(".so")
                        if bversion != -1 and eversion != -1:
                            bversion += len(lalib)
                            if lalib[-1].isdigit():
                                bversion -= 1
                            pv = libname[bversion:eversion]
                            if len(version) < len(pv):
                                version = pv
                        boost = True
                if boost:
                    if libpath[-1] != '/':
                        libpath += "/"
                    if lalib[:3] == "lib":
                        lalib = lalib[3:]
                    # Si le nom est de la forme so.x.y
                    # et qu'on a pas trouve une forme .so
                    # On cree un lien symbolique
                    if name[-3:] != ".so" and name[-6:] != ".dylib":
                        os.system("cd check; ln -s "+libpath+name+" lib"+lalib+".so")
                        pathlib = "-Lcheck -L../check"
                    else:
                        pathlib = "-L"+libpath
                        lalib = name[3:-3]
                    namelib = "-l"+lalib
                    return [pathlib, libincludes[ilib], namelib, version]
        ilib += 1

def selectionBoost():
    os.system("cd check; rm -f lib*.so")
    f=open("Makefile.in", "w")
    [pathlib, includepath, namelib, vide] = cherchelib("libcurl")
    f.write("REGEX = -DPOSIXREGEX -DBOOSTPOSIXREGEX -I"+includepath+"\n")
    f.write("LIBBOOST = "+pathboost+" "+nameboost+"\n")
    f.write("CURLLIB = "+pathlib+" "+namelib+"\n")
    [pathlib, includepath, namelib, vide] = cherchelib("xml2")
    [pathlib, includepath, namelib, vide] = cherchelib("sqlite3")
    f.write("SQLITELIB = "+pathlib+" "+namelib+"\n")
    [pathlibpython, includepath, namelibpython, pythonversion] = cherchelib("python3")
    f.write("INCLUDEPYTHON = -I/usr/include/python"+pythonversion+"\n")
    f.write("PYTHONLIB = "+pathlibpython+" " + namelibpython+"\n")
    f.close()

def selectionNoRegex():
    os.system("cd check; rm -f lib*.so")
    f=open("Makefile.in", "w")
    [pathlib, includepath, namelib, vide] = cherchelib("libcurl")
    f.write("CURLLIB = "+pathlib+" "+namelib+"\n")
    [pathlib, includepath, namelib, vide] = cherchelib("xml2")
    [pathlib, includepath, namelib, vide] = cherchelib("sqlite3")
    f.write("SQLITELIB = "+pathlib+" "+namelib+"\n")
    [pathlibpython, includepath, namelibpython, pythonversion] = cherchelib("python3")
    f.write("REGEX = -I"+includepath+"\n")
    f.write("LIBBOOST = "+pathlib+"\n")
    f.write("INCLUDEPYTHON = -I/usr/include/python"+pythonversion+"\n")
    f.write("PYTHONLIB = "+pathlibpython+" " + namelibpython+"\n")
    f.close()


# On remet a zero le Makefile, en utilisant la version dans template
def reinitialisation():
    os.system("cd check; rm -f lib*.so")
    f=open("Makefile.in", "w")
    [pathlib, includepath, namelib, vide] = cherchelib("libcurl")
    f.write("CURLLIB = "+pathlib+" "+namelib+"\n")
    [pathlib, includepath, namelib, vide] = cherchelib("xml2")
    [pathlib, includepath, namelib, vide] = cherchelib("sqlite3")
    f.write("SQLITELIB = "+pathlib+" "+namelib+"\n")
    [pathlibpython, includepath, namelibpython, pythonversion] = cherchelib("python3")
    f.write("REGEX = -DPOSIXREGEX -I"+includepath+"\n")
    f.write("LIBBOOST = "+pathlib+"\n")
    f.write("INCLUDEPYTHON = -I/usr/include/python"+pythonversion+"\n")
    f.write("PYTHONLIB = "+pathlibpython+" " + namelibpython+"\n")
    f.close()

regex = 0
reinitialisation()
############################
#we check now how to include regex... either #include "boost/regex.hpp" or <regex>
cmd=["g++", "-o", "testregex","check/checkregex.cxx", "-std=c++11"]
print('')
print("------------------------------------------")
print("Warning, error messages might show as the system is checking, which version of regex is implemented on your machine.")
print("They are normal and will not prevent any actual compiling")
print('')
ostype = subprocess.call(cmd)
if ostype:
    [pathboost, includepath, nameboost, vide] = cherchelib("boost_regex")
    cmd=["g++", "-o", "testregex","check/checkregex.cxx", "-std=c++11", "-DPOSIXREGEX", "-DBOOSTPOSIXREGEX", pathboost, nameboost]
    ostype = subprocess.call(cmd)
    if ostype:
        regex = 0
    else:
        regex = 2
        print('')
        print("Using boost::regex")
        selectionBoost()
else:
    regex = 1
    print('')
    print("Using std::regex")

if regex==0:
    print("Les regex POSIX ne seront pas disponibles..")
    selectionNoRegex()
else:
    os.system("rm testregex")

