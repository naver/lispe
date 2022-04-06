/*
 *  LispE
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : LispE
 Version    : See lispe.cxx for the version number
 filename   : CodeViewController.m
 Date       : 2017/09/01
 Purpose    : 
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import "CodeViewController.h"
#import "Lecode.h"
#import "AppDelegate.h"

extern AppDelegate* currentdelegate;
@interface CodeViewController ()

@end

@implementation CodeViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do view setup here.
    NSString* title= [[_lecode window] title];
    if (title == nil) {
        [self setTitle:[_lecode Filename]];
    }
}

-(void)viewDidAppear {

    NSWindow* wnd =[_lecode window];
    [wnd setDelegate: currentdelegate];
}

-(Lecode*) Code {
    return _lecode;
}

-(void)selectLine:(NSNumber*)ln {
    [_lecode selectLineInCode: [ln integerValue]];
}


@end
