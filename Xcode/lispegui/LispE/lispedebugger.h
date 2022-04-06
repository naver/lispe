/*
 *  LispE
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : LispE
 Version    : See lispe.cxx for the version number
 filename   : lispedebugger.h
 Date       : 2017/09/01
 Purpose    :  
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import <Cocoa/Cocoa.h>
#import "CodeViewController.h"
#import "animationSegue.h"

@interface lispedebugger : NSView {
@public
    BOOL delegatenotdone;
}

- (IBAction)nextline:(id)sender;
- (IBAction)gotonextbreak:(id)sender;
- (IBAction)stoprunning:(id)sender;
- (IBAction)runuptotheend:(id)sender;
- (IBAction)intfunction:(id)sender;
- (IBAction)outfunction:(id)sender;
@property (weak) IBOutlet NSTextField *localvariables;
@property (weak) IBOutlet NSTextField *globalvariables;
@property (weak) IBOutlet NSTextField *thestack;

- (IBAction)fulldisplay:(id)sender;

-(void)Setthedelegate:(id)sender;
-(void)Setlocals:(NSString*)l;
-(void)Setglobals:(NSString*)l;
-(void)Setstack:(NSString*)l;
-(void)Selectline:(long)l;
-(void)clearlock;
@end
