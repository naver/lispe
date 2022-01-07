/*
 *  Tamgu (탐구)
 *
 * Copyright 2019-present NAVER Corp.
 * under BSD 3-clause
 */
/* --- CONTENTS ---
 Project    : Tamgu (탐구)
 Version    : See tamgu.cxx for the version number
 filename   : Lecode.m
 Date       : 2017/09/01
 Purpose    : 
 Programmer : Claude ROUX (claude.roux@naverlabs.com)
 Reviewer   :
 */

const char* Readfile(const char* path);

#import "Lecode.h"
#import "CodeViewController.h"
#import "AppDelegate.h"

extern AppDelegate* currentdelegate;

const char* Getkeywords(void);
const char* lindentation(char* basecode, int blancs);
long indentationVirtuel(char* cr, char* acc);
long* colorparser(const char* txt, long, long);
void deletion(long* c);
long computeparenthesis(const char* ln, char checkcar, long limit);

const char* crgx=NULL;

extern BOOL nouveau;
BOOL dark = false;

@implementation Lecode

-(void)awakeFromNib {
    
    modified = NO;
    
    if (dark) {
        localcouleur= [NSColor cyanColor];
        couleurcommentaires=[NSColor greenColor];
        functioncouleur=[NSColor orangeColor];
        couleurchaine=[NSColor redColor];
        couleurchainesingle = [NSColor colorWithSRGBRed:0.7 green:0.5 blue:1 alpha:1.00];
        couleurvar=[NSColor whiteColor];
    } else {
        localcouleur=[NSColor colorWithSRGBRed:0.62 green:0.131 blue:0.137 alpha:1.00];
        functioncouleur=[NSColor colorWithSRGBRed:5.0/255.0 green:5.0/255.0 blue:245.0/255.0 alpha:1.00];
        couleurcommentaires=[NSColor colorWithSRGBRed:45.0/255.0 green:140.0/255.0 blue:45.0/255.0 alpha:1.00];
        couleurchaine = [NSColor redColor];
        couleurchainesingle =[NSColor colorWithSRGBRed:140.0/255.0 green:140.0/255.0 blue:245.0/255.0 alpha:1.00];
        couleurvar=[NSColor colorWithSRGBRed:130.0/255.0 green:130.0/255.0 blue:230.0/255.0 alpha:1.00];
    }
    
    //[self scrollToBeginningOfDocument:self];
    
    lecode = [self enclosingScrollView];
    //This is where we define our line numerotation
    [lecode setRulersVisible:YES];
    [lecode setHasVerticalRuler:YES];
    ruleur=[[Linenumber alloc] init];
    [lecode setVerticalRulerView:ruleur];
    [ruleur setClientView:self];
    
    NSFont* font=[NSFont fontWithName:@"Helvetica" size:16.0];
    [self setFont: font];
    
    tobecreated = YES;
    currentrange=NSMakeRange(0,0);
    
    NSDate *today = [NSDate date];
    NSDateFormatter *dateFormat = [[NSDateFormatter alloc] init];
    [dateFormat setDateFormat:@"dd/MM/yyyy"];
    NSString *dateString = [dateFormat stringFromDate:today];
    
    NSString* fileContent = [NSString stringWithFormat:@"\n;Date: %@\n;Author: \n;Description: \n\n\n",dateString];
    fileName = [NSURL fileURLWithPath:@""];
    
    if (nouveau == NO) {
        NSString* currentfilename = currentdelegate->currentfilename;
        tobecreated = NO;
        fileName = [NSURL fileURLWithPath:currentfilename];
        [[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:fileName];
        const char* contenu = Readfile([currentfilename UTF8String]);
        if (contenu == NULL) {
            NSString* err = @"Cannot open file: ";
            err = [err stringByAppendingString: [fileName path]];
            NSAlert *alert = [[NSAlert alloc] init];
            [alert addButtonWithTitle:@"OK"];
            [alert setMessageText:@"Error!!!"];
            [alert setInformativeText:err];
            [alert setAlertStyle:NSAlertStyleWarning];
            [alert runModal];
            return;
        }
        
        fileContent = [NSString stringWithUTF8String: contenu];
    }

    nouveau = NO;
    [self setDelegate: currentdelegate];

    [self toggleAutomaticQuoteSubstitution: false];
    [self toggleGrammarChecking:false];
    [self toggleAutomaticDashSubstitution:false];

    NSWindow* wnd = [self window];
    if (wnd != nil)
        [wnd setTitle:[fileName path]];

    [self setString: fileContent];
    [self colorie];
    [self majruleur:fileContent];
}

-(void)selectLineInCode:(long)l {
    [self majruleur:[self string]];
    [ruleur selectionne:l];
}

-(BOOL)ismodified {
    return modified;
}

-(void)setmodified:(BOOL)v {
    modified = v;
}

-(void)clearallbreakpoints {
    [ruleur clearbreaks];
    [ruleur setNeedsDisplay:YES];
}

-(void)insertBreakpoint {
    [self majruleur:[self string]];
    NSRange ps=[self selectedRange];
    [ruleur addbreak:ps.location];
    [ruleur setNeedsDisplay:YES];
}

-(void)updateruleur:(NSString*)fileContent {
    NSLayoutManager* layoutManager = [self layoutManager];
    NSTextContainer* container = [self textContainer];
    NSRect ps;
        
    long longueur=[fileContent length];
    //First we renumber all our lines...
    [ruleur nettoiey];
    [ruleur nettoiel];
    NSRange suivant=NSMakeRange(0,0);
    
    long diff=-1;
    long lasty=0;
    while (suivant.location < longueur) {
        suivant = [fileContent paragraphRangeForRange: suivant];
        ps = [layoutManager boundingRectForGlyphRange:suivant inTextContainer:container];
        
        if (diff == -1) {
            lasty=ps.origin.y;
            diff=0;
        }
        else {
            if (ps.origin.y==0)
                lasty+=diff;
            else {
                diff=ps.origin.y-lasty;
                lasty=ps.origin.y;
            }
        }
        
        [ruleur ajoutey:lasty];
        [ruleur ajoutel:suivant.location];
        
        if (suivant.length!=0) {
            suivant.location+=suivant.length;
            suivant.length = 0;
        }
        else
            suivant.location+=1;
    }
}

-(void)majruleur:(NSString*)fileContent {
    NSLayoutManager            *layoutManager;
    NSTextContainer            *container;
    layoutManager = [self layoutManager];
    container = [self textContainer];
    NSRect ps;
    
    NSString* b=[@"" stringByPaddingToLength:1000 withString:@"\n" startingAtIndex:0];
    fileContent=[fileContent stringByAppendingString:b];
    
    long longueur=[fileContent length];
    //First we renumber all our lines...
    [ruleur nettoiel];
    [ruleur nettoiey];
    NSRange suivant=NSMakeRange(0,0);
    
    currentlength=0;
    long diff=-1;
    long lasty=0;
    while (suivant.location < longueur) {
        suivant = [fileContent paragraphRangeForRange: suivant];
        ps = [layoutManager boundingRectForGlyphRange:suivant inTextContainer:container];
        
        if (diff == -1) {
            lasty=ps.origin.y;
            diff=0;
        }
        else {
            if (ps.origin.y==0)
                lasty+=diff;
            else {
                diff=ps.origin.y-lasty;
                lasty=ps.origin.y;
            }
        }
        
        [ruleur ajoutey:lasty];
        [ruleur ajoutel:suivant.location];
        
        if (suivant.length!=0) {
            suivant.location+=suivant.length;
            suivant.length = 0;
        }
        else
            suivant.location+=1;
        currentlength++;
    }
}

-(NSString*)Filename {
    if (fileName == nil)
        return @"Untitled";
    return [fileName path];
}

-(NSURL*)FileURL {
    return fileName;
}

-(NSString*) Contenu {
    return [self string];
}

-(BOOL) isTobecreated {
    return tobecreated;
}

-(void) Tobecreated:(BOOL)v {
    tobecreated = v;
}

-(NSURL *)Setname {
    NSSavePanel* saveDlg = [NSSavePanel savePanel];
    
    // Enable the selection of files in the dialog.
    
    // Display the dialog.  If the OK button was pressed,
    // process the files.
    if ( [saveDlg runModal] == NSModalResponseOK ) {
        tobecreated = NO;
        fileName = [saveDlg URL];
        [[self window] setTitle:[fileName path]];
        return fileName;
    }
    
    tobecreated = YES;
    return [NSURL fileURLWithPath:@""];
}

-(NSRange)viewRange {
    NSLayoutManager            *layoutManager;
    NSTextContainer            *container;
    NSRect                    visibleRect;
    
    layoutManager = [self layoutManager];
    container = [self textContainer];
    visibleRect = [[lecode contentView] bounds];
    NSRange glyphRange = [layoutManager glyphRangeForBoundingRect:visibleRect inTextContainer:container];
    NSRange range = [layoutManager characterRangeForGlyphRange:glyphRange actualGlyphRange:NULL];
    return range;
}

-(void)coloreview {
    currentrange=NSMakeRange(0, 0);
    [self colorie];
}
    
-(void)colorie {
    unsigned long longueur = [[self string] length];
    if (longueur<=3)
        return;
    
    long limite=longueur;
    
    NSRange suivant;
    NSRange trouve;
    NSString* letexte;
    if (currentrange.length != 0) {
        suivant=currentrange;
        limite=currentrange.location+currentrange.length;
    }
    else {
        suivant.location=0;
        suivant.length = longueur;
    }
    
    letexte=[self string];
    
    NSRect visibleRect = [[[ruleur scrollView] contentView] bounds];
    if (dark)
        [self setTextColor: [NSColor whiteColor] range:suivant];
    else
        [self setTextColor: [NSColor blackColor] range:suivant];
    
    limite = [letexte lengthOfBytesUsingEncoding:NSUTF8StringEncoding];
    long* tobecolored=colorparser([letexte UTF8String], suivant.location, limite);
    
    //[ruleur majy: visibleRect.origin.y];

    for (long i=0; tobecolored[i]!=-1;i+=3) {
        trouve.location=tobecolored[i+1];
        trouve.length=tobecolored[i+2];

        switch (tobecolored[i]) {
            case 1: //string ""
                [self setTextColor: couleurchaine range:trouve];
                break;
            case 2://string ''
                [self setTextColor: couleurchainesingle range:trouve];
                break;
            case 3://string @""@;
                [self setTextColor: [NSColor grayColor] range:trouve];
                break;
            case 4: //.xxx(
                [self setTextColor: functioncouleur range:trouve];
                break;
            case 5://keyword
                [self setTextColor: functioncouleur range:trouve];
                break;
            case 6: //xxx(
                [self setTextColor:localcouleur  range:trouve];
                break;
            case 7: //comments
                [self setTextColor: couleurcommentaires range:trouve];
                break;
            default: //special variables ?label #D+ $d+
                [self setTextColor: couleurvar range:trouve];
        }
    }
    deletion(tobecolored);
    NSRect visibleRectFinal = [[[ruleur scrollView] contentView] bounds];
    if (visibleRectFinal.origin.y != visibleRect.origin.y)
        [self majruleur:[self string]];
}

-(BOOL)testpadding:(NSString*)localstring {
    //Else it has some padding
    //Then we extract the padding on the left...
    const char* code=[localstring UTF8String];
    int ln=0;
    while (code[ln]!=0 && code[ln]<=32) ln++;
    if (code[ln] != '}' && code[ln] != ')')
        return NO;
    return YES;
}

-(long)findTopline:(NSRange)rg {
    //We are looking for the first line without any spaces at the beginning
    while (rg.location>0) {
        rg=[[self string] paragraphRangeForRange: rg];
        if ([[self string] characterAtIndex:rg.location] > 32)
            return rg.location;
        rg.location--;
    }
    return -1;
}

-(void)resetCursor:(NSTimer*)nst {
    NSRange r;
    r.location = [[nst userInfo] longValue];
    r.length = 1;
    if (dark)
        [self setTextColor: [NSColor whiteColor] range:r];
    else
        [self setTextColor: [NSColor blackColor] range:r];
}

-(void)blinkMatchingParenthesis {
    NSRange backrange = [self selectedRange];
    if (backrange.location < 2)
        return;
    
    backrange.location--;
    backrange.length = 1;
    const char* code = [[[self string] substringWithRange: backrange] UTF8String];
    const char key = code[0];

    long pos=[self findTopline: backrange];
    if (pos==-1)
        return;
    
    backrange=NSMakeRange(pos,backrange.location-pos);
        
    code = [[[self string] substringWithRange: backrange] UTF8String];
    pos = computeparenthesis(code, key, backrange.length);
    if (pos != -1) {
        backrange.location += pos;
        backrange.length = 1;
        [self setTextColor: [NSColor redColor] range:backrange];
        currentrange = backrange;
        NSNumber* ni = [NSNumber numberWithLong:backrange.location];
        [NSTimer
         scheduledTimerWithTimeInterval:0.35
         target:self
         selector:@selector(resetCursor:)
         userInfo:ni
         repeats:NO];
    }
}

-(BOOL)localcolor:(char)key {
    static const char cc[]={'"','\'',';','=','/', ' ', 0};
    
    modified=YES;
    
    NSRange localrange=[self selectedRange];
    NSInteger locpos=localrange.location;
    
    unsigned long longueur;
    longueur=[[self string] length];
    if (longueur<=3)
        return NO;
    currentrange=NSMakeRange(locpos,0);
    NSString* truc;
    
    if (strchr(cc,key) != NULL) {
        if (localrange.location>1 && key=='/') {
            key=[[self string] characterAtIndex:localrange.location-2];
            if (key=='@')
                currentrange=[self viewRange];
            else
                currentrange=[[self string] paragraphRangeForRange: localrange];
        }
        else
            //We force coloring
            currentrange=[[self string] paragraphRangeForRange: localrange];
        [self colorie];
        return TRUE;
    }

    NSRange backrange = NSMakeRange(localrange.location-1,0);
    long pos=[self findTopline: backrange];
    if (pos==-1)
        return YES;
    
    backrange=NSMakeRange(pos,localrange.location-pos);
    
    const char* code;
    long ln = -1;
    char acc=0;
    currentrange=[[self string] paragraphRangeForRange: currentrange];
    NSString* line = [[self string] substringWithRange: currentrange];
    
    [self clearallbreakpoints];
    
    if (key == '}' || key == ')' || key == ']') {
        //We are looking for the last previous '{' above

        if ([self testpadding:line] ==  YES) {
            code = [[[self string] substringWithRange: backrange] UTF8String];
            ln=indentationVirtuel((char*)code,&acc);
         }
         else {
             //We make the matching parenthesis blink
             backrange.length = locpos-backrange.location-1;
             code = [[[self string] substringWithRange: backrange] UTF8String];
             pos = computeparenthesis(code, key, backrange.length);
             if (pos != -1) {
                 backrange.location += pos;
                 backrange.length = 1;
                 [self setTextColor: [NSColor redColor] range:backrange];
                 NSNumber* ni = [NSNumber numberWithLong:backrange.location];
                 currentrange = backrange;
                 [NSTimer
                  scheduledTimerWithTimeInterval:0.35
                  target:self
                  selector:@selector(resetCursor:)
                  userInfo:ni
                  repeats:NO];
                 
             }
             return YES;
         }
        
        //we found it...
        //we need to count the number of spaces...
        line=[line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
        if (ln) {
            truc=[@"" stringByPaddingToLength:ln withString:@" " startingAtIndex:0];
            truc=[truc stringByAppendingString: line];
        }
        else {
            if ([truc isEqualToString:line])
                return YES;
            truc=line;
        }
        
        if ([self shouldChangeTextInRange:currentrange replacementString:truc]) {
            [self replaceCharactersInRange:currentrange withString:truc];
            long difference = currentrange.length;
            currentrange.length = [truc length];
            difference -= currentrange.length;
            locpos -= difference;
        }
        
        localrange=currentrange;

        //We make the matching parenthesis blink
        backrange.length = locpos-backrange.location-1;
        if (backrange.length == 0) {
            //This is a final ), let's find the top line
            backrange.location--;
            pos=[self findTopline: backrange];
            if (pos == -1) {
                localrange.location+=ln+1;
                localrange.length=0;
                [self setSelectedRange:localrange];
                return YES;
            }
            
            backrange.length = locpos-pos-1;
            backrange.location = pos;
        }
        code = [[[self string] substringWithRange: backrange] UTF8String];
        pos = computeparenthesis(code, key, backrange.length);
        if (pos != -1) {
            backrange.location += pos;
            backrange.length = 1;
            [self setTextColor: [NSColor redColor] range:backrange];
            currentrange = backrange;
            NSNumber* ni = [NSNumber numberWithLong:backrange.location];
            [NSTimer
             scheduledTimerWithTimeInterval:0.35
             target:self
             selector:@selector(resetCursor:)
             userInfo:ni
             repeats:NO];
        }

        localrange.location+=ln+1;
        localrange.length=0;
        [self setSelectedRange:localrange];
        return YES;
    }

    code = [[[self string] substringWithRange: backrange] UTF8String];
    ln=indentationVirtuel((char*)code,&acc);
    
    //we found it...
    //we need to count the number of spaces...
    line=[line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
    if (ln > 0) {
        truc=[@"" stringByPaddingToLength:ln withString:@" " startingAtIndex:0];
        truc=[truc stringByAppendingString: line];
        
        if ([self shouldChangeTextInRange:currentrange replacementString:truc])
            [self replaceCharactersInRange:currentrange withString:truc];
        
        localrange=currentrange;
        localrange.location+=ln;
        localrange.length=0;
        currentrange=[self viewRange];
        [self colorie];
        [self setSelectedRange:localrange];
        return YES;
    }
    currentrange=[self viewRange];
    [self colorie];
    localrange.length=0;
    [self setSelectedRange:localrange];
    return YES;
}

-(void)indentation {
    NSRange localposition=[self selectedRange];
    long diff = localposition.location;
    localposition = [[self string] paragraphRangeForRange: localposition];
    diff -= localposition.location;
    long line = [ruleur getline: localposition.location];

    long top = [ruleur gettopline];
    long bottom = [ruleur getlastline];

    currentrange=NSMakeRange(0,0);
    const char* code= [[self string] UTF8String];
    code=lindentation((char*)code,0);
    NSString* indentedcode = [NSString stringWithUTF8String:code];
    modified=YES;
    NSRange all= NSMakeRange(0, [[self string] length]);
    
    //to speed up the process, we set the colors in the background
    //We hide the window temporaly
    [self setHidden:true];
    if ([self shouldChangeTextInRange:all replacementString:indentedcode]) {
        [self setString:indentedcode];
        [self colorie];
        [self majruleur: indentedcode];
    }
    [self setHidden:false];
    //We need then to put it back as the current input window
    [[self window] selectNextKeyView:nil];

    [self scrollToBeginningOfDocument:nil];
    top = [ruleur getpos:top];
    bottom = [ruleur getpos:bottom];
    localposition.location = top;
    localposition.length = bottom-top;
    [self scrollRangeToVisible: localposition];

    localposition.location = [ruleur getpos:line] + diff;
    localposition.length = 0;
    [self setSelectedRange:localposition];
}

-(void)insere:(NSString*)car {
    NSRange localposition=[self selectedRange];
    if ([self shouldChangeTextInRange:localposition replacementString:car])
        [self replaceCharactersInRange:localposition withString:car];
}

-(void)viewDidEndLiveResize {
    currentrange=NSMakeRange(0,0);
    
    [self majruleur: [self string]];
    [ruleur setNeedsDisplay:YES];
}


-(void)selectmatchingbracket {
    NSRange localposition=[self selectedRange];
    localposition=[[self string] paragraphRangeForRange: localposition];
    
    long pos=localposition.location+localposition.length-2;
    unsigned char c = [[self string] characterAtIndex:pos];
    long count = 1;
    
    if (c =='{') {
        long i=pos+2;
        //We explore the value AFTER...
        long sz=[[self string] length];
        while (count && i<sz) {
            c=[[self string] characterAtIndex:i];
            if (c=='{')
                count++;
            else
                if (c=='}')
                    count--;
            i++;
        }
        if (!count) {
            localposition.location=i;
            localposition.length=0;
            localposition=[[self string] paragraphRangeForRange: localposition];
            [self setSelectedRange:localposition];
            [self  scrollRangeToVisible: localposition];
            return;
        }
    }

    if (c=='}') {
        //backward...
        long i=pos-1;
        while (count && i>=0) {
            c=[[self string] characterAtIndex:i];
            if (c=='{')
                count--;
            else
                if (c=='}')
                    count++;
            i--;
        }
        if (!count) {
            localposition.location=i;
            localposition.length=0;
            localposition=[[self string] paragraphRangeForRange: localposition];
            [self setSelectedRange:localposition];
            [self  scrollRangeToVisible: localposition];
            return;
        }
    }
}
@end
