/*
 *  Tamgu (탐구)
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : Tamgu (탐구)
 Version    : See tamgu.cxx for the version number
 filename   : Linenumber.m
 Date       : 2017/09/01
 Purpose    : 
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

void addabreakpoint(const char* filename, long numline, char add);
void clearallbreakpoints(void);


#import "Linenumber.h"

@implementation Linenumber


-(void)ajoutey:(NSInteger)i {
    if (ypositions==NULL)
        ypositions=[[NSMutableArray alloc] init] ;
    
    [ypositions addObject:[NSNumber numberWithInteger:i+3]];
}

-(void)nettoiey {
    if (ypositions==NULL)
        return;
    [ypositions removeAllObjects];
}

-(void)ajoutel:(NSInteger)i {
    if (lignes==NULL)
        lignes=[[NSMutableArray alloc] init];
    
    [lignes addObject:[NSNumber numberWithInteger:i]];
}

-(void)nettoiel {
    if (lignes==NULL)
        return;
    [lignes removeAllObjects];
}


- (id)init
{
    self = [super init];
    if (self) {
        
    }
    return self;
}

-(void)Ligne:(NSMutableArray*)l {
    for (NSNumber* n in lignes)
        [l addObject:n];
}

-(NSWindow*)fenetre {
    return [[self clientView] window];
}

-(long)getline:(long)pos {
    long i = 1;
    for (NSNumber* n in lignes) {
        if ([n intValue] >= pos)
            return i;
        i++;
    }
    return i;
}

-(long)getpos:(long)ligne {
    if (ligne <= 0 || ligne >= [lignes count])
        return 0;
    
    return [[lignes objectAtIndex:ligne-1] intValue];
}

- (void)selectionne:(long)ligne {
    if (ligne <= 0 || ligne >= [lignes count])
        return;
        
    long debut=[[lignes objectAtIndex:ligne-1] intValue];
    long fin;
    NSTextView* textecode= (NSTextView*)[self clientView];
    if (ligne < [lignes count])
        fin=[[lignes objectAtIndex:ligne] intValue];
    else
        fin=[[textecode textStorage] length];
    
    NSRange rg=NSMakeRange(debut, fin-debut);
    [textecode setSelectedRange:rg];
    [textecode scrollRangeToVisible: rg];
}

-(bool)findbreakline:(long)ligne {
    if (breakpoints==NULL)
        return false;
    long idx=[breakpoints indexOfObjectIdenticalTo:[NSNumber numberWithLong:ligne]];
    if (idx==NSNotFound)
        return false;
    return true;
}

-(void)clearbreaks {
    [breakpoints removeAllObjects];
}

-(long)addbreak:(NSInteger)pos {
    if (breakpoints==NULL)
        breakpoints=[[NSMutableArray alloc] init];
    
    const char* nom=[[[[self clientView] window] title] UTF8String];
    
    long ligne=0;
    for (NSNumber* n in lignes)  {
        if (pos<[n intValue])
            break;
        ligne++;
    }
    
    long idx=[breakpoints indexOfObjectIdenticalTo:[NSNumber numberWithLong:ligne]];
    if (idx!=NSNotFound) {
        //We remove it
        [breakpoints removeObjectAtIndex:idx];
        addabreakpoint(nom,ligne,0);
    }
    else {
        [breakpoints addObject:[NSNumber numberWithLong:ligne]];
        addabreakpoint(nom,ligne,1);
    }
    
    return ligne;
}

- (long)gettopline {
    NSScrollView* lecode=[self scrollView];
    NSRect visibleRect;
    
    visibleRect = [[lecode contentView] bounds];
    long base=visibleRect.origin.y;
    long i = 1;
    for (NSNumber* n in ypositions)  {
        if ([n intValue] >= base)
            break;
        i++;
    }
    
    return i;
}

- (long)getlastline {
    NSScrollView* lecode=[self scrollView];
    NSRect visibleRect;
    
    visibleRect = [[lecode contentView] bounds];
    long base=visibleRect.origin.y;
    base += visibleRect.size.height;
    long i = 1;
    for (NSNumber* n in ypositions)  {
        if ([n intValue] >= base)
            break;
        i++;
    }
    
    return i;
}

- (void)numerolignes {
    
    NSTextView* textecode=(NSTextView*)[self clientView];
    NSScrollView* lecode=[self scrollView];
    NSLayoutManager            *layoutManager;
    NSTextContainer            *container;
    NSRect                    visibleRect;
    
    layoutManager = [textecode layoutManager];
    container = [textecode textContainer];
    visibleRect = [[lecode contentView] bounds];
    //NSRange glyphRange = [layoutManager glyphRangeForBoundingRect:visibleRect inTextContainer:container];
    //NSRange range = [layoutManager characterRangeForGlyphRange:glyphRange actualGlyphRange:NULL];
    
    long l = 0;
    NSString* labelText;
    NSSize szn;
    int base=visibleRect.origin.y - 2;
    long nblines = [lignes count];
    for (l = 0; l < nblines; l++)  {
        labelText = [NSString stringWithFormat:@"%ld", l + 1];
        
        if ([self findbreakline:l+1]) {
            szn=[labelText sizeWithAttributes: attributsBreaks];
            
            NSRect rectb=NSMakeRect(3,[[ypositions objectAtIndex:l] intValue]-base,
                                    szn.width+2, szn.height);
            // Draw string flush right, centered vertically within the line
            [labelText drawInRect:rectb withAttributes:attributsBreaks];
            
        }
        else {
            szn=[labelText sizeWithAttributes: attributsNumeros];
            
            NSRect rectb=NSMakeRect(3,[[ypositions objectAtIndex:l] intValue]-base,
                                    szn.width+2, szn.height);
            // Draw string flush right, centered vertically within the line
            [labelText drawInRect:rectb withAttributes:attributsNumeros];
        }
    }
}

- (void)drawHashMarksAndLabelsInRect:(NSRect)rect {
    
    if (attributsNumeros==NULL) {
        attributsNumeros=[NSDictionary dictionaryWithObjectsAndKeys:
                          [NSFont fontWithName:@"Helvetica" size:10.0], NSFontAttributeName,
                          [NSColor lightGrayColor], NSForegroundColorAttributeName,
                          nil];
        NSFontManager *fontManager = [NSFontManager sharedFontManager];
        NSFont *italique = [fontManager fontWithFamily:@"Helvetica"
                                                traits:NSItalicFontMask
                                                weight:0
                                                  size:10];
        attributsBreaks=[NSDictionary dictionaryWithObjectsAndKeys:
                         italique, NSFontAttributeName,
                         [NSColor blueColor], NSForegroundColorAttributeName,
                         nil];
    }
    
    [self numerolignes];
    
}

@end
