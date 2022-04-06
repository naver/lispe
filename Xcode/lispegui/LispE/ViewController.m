/*
 *  LispE
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : LispE
 Version    : See lispe.cxx for the version number
 filename   : ViewController.m
 Date       : 2017/09/01
 Purpose    : 
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import "ViewController.h"
#import "AppDelegate.h"
ViewController* vue = nil;

extern AppDelegate* currentdelegate;

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    // Do any additional setup after loading the view.
    vue = self;
}


- (void)setRepresentedObject:(id)representedObject {
    [super setRepresentedObject:representedObject];

    // Update the view, if already loaded.
}

-(Console*)Maconsole {
    return _maconsole;
}


@end
