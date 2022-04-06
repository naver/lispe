/*
 *  LispE
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : LispE
 Version    : See lispe.cxx for the version number
 filename   : AppDelegate.h
 Date       : 2017/09/01
 Purpose    :  
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import <Cocoa/Cocoa.h>
#import "Console.h"

@interface AppDelegate : Console <NSApplicationDelegate, NSTextViewDelegate, NSWindowDelegate> {
    BOOL initialization;
    CodeViewController* controleur;
@public
    NSString* currentfilename;
    
}

@end

