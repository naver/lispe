/*
 *  Tamgu (탐구)
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : Tamgu (탐구)
 Version    : See tamgu.cxx for the version number
 filename   : Console.h
 Date       : 2017/09/01
 Purpose    :  
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import <Cocoa/Cocoa.h>
#import "CodeViewController.h"

@interface Console : NSTextView {
@public
    
    NSRecursiveLock* runlock;
    NSString* inputtxt;
}

- (IBAction)opendocument:(id)sender;
- (IBAction)execution:(id)sender;
- (IBAction)savedocument:(id)sender;
- (IBAction)saveas:(id)sender;
- (IBAction)indentation:(id)sender;
- (IBAction)negation:(id)sender;
- (IBAction)different:(id)sender;
- (IBAction)disjunction:(id)sender;
- (IBAction)conjunction:(id)sender;
- (IBAction)pi:(id)sender;
- (IBAction)tau:(id)sender;
- (IBAction)euler:(id)sender;
- (IBAction)golden:(id)sender;
- (IBAction)multiply:(id)sender;
- (IBAction)divide:(id)sender;
- (IBAction)product:(id)sender;
- (IBAction)breakpoint:(id)sender;
- (IBAction)sum:(id)sender;
- (IBAction)square:(id)sender;
- (IBAction)cube:(id)sender;
- (IBAction)squareroot:(id)sender;
- (IBAction)cubicroot:(id)sender;
- (IBAction)stopexecution:(id)sender;
- (IBAction)abouttamgu:(id)sender;
- (IBAction)listing:(id)sender;
- (IBAction)clearallbreakpoints:(id)sender;
- (IBAction)runnotclean:(id)sender;
- (IBAction)endofapp:(id)sender;
- (IBAction)debugmode:(id)sender;
- (IBAction)matchingbracket:(id)sender;
- (IBAction)arrow:(id)sender;
- (IBAction)setpath:(id)sender;
- (IBAction)arrowright:(id)sender;

-(BOOL)checkstatus;
-(void)Runcode:(NSString*)filename;
-(BOOL)Executecode:(BOOL)debugmode cleaning:(BOOL)clean;
-(void)displaychar: (NSString*)car;
-(void)rappel: (const char*)s;
-(void)prepareForSegue:(NSStoryboardSegue *)segue sender:(id)sender;
-(void)displayinconsole: (NSString*)output;
-(void)DisplayMessage: (NSString*) message;
+(void)runinthread:(CodeViewController*)cn;
-(void)displayerror:(CodeViewController*)cn;
-(CodeViewController*)topview;
-(void)Selectline:(NSNumber*)l;
-(BOOL)DisplayAlert:(NSString*)msg;
-(const char*)InputText:(NSString*)nmsg;
-(void)matchingparenthesis;

@end
