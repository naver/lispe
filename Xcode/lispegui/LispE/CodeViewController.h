/*
 *  Tamgu (탐구)
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : Tamgu (탐구)
 Version    : See tamgu.cxx for the version number
 filename   : CodeViewController.h
 Date       : 2017/09/01
 Purpose    :  
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

#import <Cocoa/Cocoa.h>
#import "Lecode.h"
#import "animationSegue.h"

@interface CodeViewController : NSViewController {
    @public
    animationSegue* a;
}

@property (unsafe_unretained) IBOutlet Lecode *lecode;

-(Lecode*) Code;
-(void)selectLine:(NSNumber*)ln;
@end
