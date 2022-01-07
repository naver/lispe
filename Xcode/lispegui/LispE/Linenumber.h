/*
 *  Tamgu (탐구)
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : Tamgu (탐구)
 Version    : See tamgu.cxx for the version number
 filename   : Linenumber.h
 Date       : 2017/09/01
 Purpose    :  
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import <Cocoa/Cocoa.h>

@interface Linenumber : NSRulerView {
    NSDictionary* attributsNumeros;
    NSDictionary* attributsBreaks;
    NSMutableArray* lignes;
    NSMutableArray* breakpoints;
    NSMutableArray* ypositions;
}

-(bool)findbreakline:(long)ligne;
-(void)ajoutey:(NSInteger)i;
-(void)nettoiey;
-(void)ajoutel:(NSInteger)i;
-(void)nettoiel;
-(void)numerolignes;
-(void)selectionne:(long)ligne;
-(long)getline:(long)pos;
-(long)getpos:(long)line;
-(long)gettopline;
-(long)getlastline;
-(NSWindow*)fenetre;
-(void)Ligne:(NSMutableArray*)l;
-(void)clearbreaks;
-(long)addbreak:(NSInteger)pos;

@end
