/*
 *  Tamgu (탐구)
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : Tamgu (탐구)
 Version    : See tamgu.cxx for the version number
 filename   : Lecode.h
 Date       : 2017/09/01
 Purpose    :  
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import <Cocoa/Cocoa.h>
#import "Linenumber.h"

@interface Lecode : NSTextView {
    NSScrollView* lecode;
    NSURL* fileName;
    BOOL tobecreated;
    BOOL dark;
    Linenumber* ruleur;
    NSRange currentrange;
    long currentlength;
    BOOL modified;
    NSColor* localcouleur;
    NSColor* functioncouleur;
    NSColor* couleurcommentaires;
    NSColor* couleurchaine;
    NSColor* couleurchainesingle;
    NSColor* couleurvar;

}

-(BOOL)appearanceIsDark:(NSAppearance*) appearance;
-(BOOL)ismodified;
-(void)setmodified:(BOOL)v;
-(BOOL)isTobecreated;
-(void)Tobecreated:(BOOL)v;
-(NSString*) Filename;
-(NSURL*)FileURL;
-(NSString*) Contenu;
-(NSURL*) Setname;
-(void)colorie;
-(void)indentation;
-(BOOL)localcolor:(char)key;
-(void)selectLineInCode:(long)l;
-(void)clearallbreakpoints;
-(void)insere:(NSString*)car;
-(void)majruleur:(NSString*)fileContent;
-(void)insertBreakpoint;
-(void)selectmatchingbracket;
-(void)coloreview;
@end
