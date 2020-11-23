/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  directorylisting.h
//
//

#ifndef directorylisting_h
#define directorylisting_h

#ifdef WIN32
#include <windows.h>
#include <winbase.h>
#else
#include <sys/types.h>
#include <dirent.h>
#endif

class directorylisting	{
public:
    string current;
    
#ifdef WIN32
    HANDLE thedirectory;
    WIN32_FIND_DATAA dir;
#else
    DIR* thedirectory;
    dirent* dir;
#endif
    
    directorylisting(string n) : current(n) {}
    
#ifdef WIN32
    bool initial() {
        thedirectory = INVALID_HANDLE_VALUE;
        DWORD attribs = GetFileAttributesA(STR(current));
        if (attribs == 0xFFFFFFFF || !(attribs && FILE_ATTRIBUTE_DIRECTORY))
            return false;

        string fullname(current);
        
        if (fullname.size() > 0 && fullname.back() != '\\')
            fullname += "\\";
        
        fullname += "*";
        
        thedirectory = FindFirstFileA(STR(fullname), &dir);
        if (thedirectory == INVALID_HANDLE_VALUE)
            return false;

        current = dir.cFileName;
        return true;
    }
    
    ~directorylisting() {
        if (thedirectory != INVALID_HANDLE_VALUE)
            FindClose(thedirectory);
    }
    
    bool getnext(string& name) {
        if (current == "*")
            return false;
        
        name = current;
        if (FindNextFileA(thedirectory, &dir))
            current = dir.cFileName;
        else
            current = "*";
            
        return true;
    }

    bool getnext() {
        if (current == "*")
            return false;
        
        if (FindNextFileA(thedirectory, &dir))
            current = dir.cFileName;
        else
            current = "*";
        
        return true;
    }
    

#else
    bool initial() {
        thedirectory = opendir(STR(current));
        if (!thedirectory)
            return false;
        
        dir = readdir(thedirectory);
        if (dir) {
            current = dir->d_name;
            return true;
        }
        return false;
    }

    ~directorylisting() {
        if (thedirectory)
            closedir(thedirectory);
    }

    bool getnext(string& name) {
        if (current == "*")
            return false;
        
        name = current;
        dir = readdir(thedirectory);
        if (dir)
            current = dir->d_name;
        else
            current="*";
        return true;
    }

    bool getnext() {
        if (current == "*")
            return false;

        dir = readdir(thedirectory);
        if (dir)
            current = dir->d_name;
        else
            current="*";
        return true;
    }

#endif

};


#endif

