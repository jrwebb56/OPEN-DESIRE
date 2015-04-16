/* INIT.C      Initialization and Re-initialization of Symbol Table,   */

/*             Data, Stacks, etc.; initial Display for DESIRE          */
/*             Linux/XWindows Version                                  */

/*                                                                     */
/*             Registered Copyright 20010 G.A. Korn                     */

/***>  This source code is copyrighted and distributed under the GNU General
    Public License found in the file GPL.txt in this software package.    <***/

/*    fixed clear screen for NT                          2/18/96       */
/*    added mouse checker ; new version number           2/23/96  ******/
/*    changed initial display text              4/20/96, 5/13/96       */
/*    now use spare2 to mark runf before STOP            5/21/96       */ 
/*    kill spare2 again = not needed!                   10/11/96       */

/**   corrected initial screen message to read F5, not F7 2/10/97      */
/**   killed stashflag = 0                                             */      
/**   version change in screen message                   12/31/97      */
/**   3-D                                                7/5/98        */
/**   initialize new system parameter MM                 9/18/02       */
/**   DESIRE/2000 Message (new vector operations) 9/13/98, 3/27/99,
                                  10/26/99, 8/29/02,9/13/02, 12/15/02  */
/*    corrected resetting of MM                          9/29/02       */
/*    initialize X, Y for display                        1/11/03       */
/*    killed references to editflg, SPARE3, spare4    11/14/04  */  

/*    note: gcc 3.3 stops printf strings on carriage return in source code  */ 
/*	  Version 11.0			7/028/05      */   
/*             changed initial message, Version 13.0      5/12/99, 8/12/07  */

/**  changed to set DTMAX; initial message V15.0  1/9/10 */
							    /**  initial message V 16.0    9/12/10 **/
/* --------------------------------------------------------------------- */

#include <time.h>
#include <string.h>
#include "declare.h"
#include "global.h"
#include <X11/Xlib.h>

extern void free (void *__ptr) __THROW;
extern  void  derror() ;
extern  void  pdtime() ;                 /* displays date and time */
extern  void  initX() ;                             /* starts XWindows */
extern  void  QuitGraph() ;                         /* kill XWindows */
extern  int X,Y ;
extern  unsigned int WIDTH,HEIGHT; 
extern Screen *theScreen ;
extern int xpos(Screen *s, unsigned int width) ;
extern int ypos(Screen *s, unsigned int height) ;
/* ------------------------------------------------------------------- */

void dummy(STRING ss)    /* installs predefined symbols in symbol table */
                                                   /* CALLED BY prepr1 */
{
    register int li=0, lk=0,  hashvvv ;  ptr  p ;
    
    while(ss[li]) {                   /* extract symbol, get hash value */
	hashvvv=0 ;
	for(lk=0;lk<=varsize-1;lk++) {
	    if (ss[li]>='0') {
		symbol[lk]=ss[li++] ;
		hashvvv+=symbol[lk] ;
            }
	    else symbol[lk]=0 ;
	}
	hashvvv%=maxhash ;        /* simple hashing operation */
	/* symbol-table installation */
	p=(struct symnode *) malloc(sizeof(struct symnode)) ;
	p->SWITCH=TRUE ;
	p->link=hashtab[hashvvv] ;
	strcpy(p->name,symbol) ;
	p->symtype=vartyp ;
	p->valptr=gir ;
	p->next=NULL ;
	hashtab[hashvvv]=p ;
	
	if (vartyp!=1) gir-- ; else relval[gir++]=0 ;   /* 0 if variable */
	li++ ;                                            /* skip $ or ) */
    }                                    /* loop to end of string ss */
}                                                          /* dummy */


void prepr1() {     /* clear/restore data values; clear compiler area; */
    /* reset data pointer, initialize flags etc. */
    /* CALLED BY initialize, execute, old00, new00, prfile */
    
    int  li ;  ptr  p ;
    
    for (li=0 ; li<=maxhash ; li++) {     /* clear symbol table, except */
	    while (hashtab[li]!=NULL) {         /* for system parameters and */
	        p=hashtab[li] ;                          /* library functions */
	        if (p->symtype==1) {
		        if (p->valptr>mmm && p->valptr<=mmm+maxsysp) break ;
                }
	        else if (p->valptr<0) break ;
	        hashtab[li]=p->link ;
	        free(p) ;
	        }                                                 /* while */
        }                                                         /* for */
    
    while (stktop!=NULL) {                       /* clean control stack */
	    tptr=stktop ;
	    stktop=stktop->dslink ;
	    free(tptr) ;
        }
    
    flevel=xlength=spare1=flag=0 ; datptr=linsiz+1 ;
    gii=wptr=1 ; gjj=maxrlvar ;
    gir=mmm+maxsysp+1 ;         /* this will preserve system parameters */
    
    flag1=FALSE ;                 /* TRUE prevents definitions in calls */
    dispp=1 ;                      /* turns display 1 ON, display 2 OFF */
    savindx=0 ;                           /* reset flag for "display 2" */
    nn1=0 ;                           /* no state variables defined yet */
    if (spare3) QuitGraph() ;                          /* kill XWindows */
    
    /** spare2=1 ;                 **/    /* normally poll graphs , killed*/
    
    bdummy=0 ;                                  /* thin dots is default */
    stashflag=0 ;                                  /* no SYSPIC.DAT yet */
    fdn=outptr=stdout ; eofflg=FALSE ;
    
    for (li=1 ; li<11 ; li++) relval[mmm+li]=0 ;     /* default values */
    relval[mmm+7]=2.0E25 ;  	 /* default value of DTMAX    <**** 1/9/10 ****/   
    relval[mmm+15]=1 ;                           /* default value of MM */   
    relval[mmm+5]=1.1E-275 ;           /* default, overrides t=0 **! <***/   
}                                                         /* end prepr1 */

void initialize() {        /* setup, initial display, get command line */
    /* USES prepr1, dummy, scroll, pdtime */
    
    inptr=stdin ; outptr=stdout ; fdn=stdout ;          /***> for LINUX */
    
    savindx=1 ;                                 /* normally don't flush */
    SPARE1=SPARE4=0 ; /* SPARE1 causes run in prcomp, SPARE4 is bye flag */ 
    gir=mmm+1 ;                            /* install system parameters */
    vartyp=1 ; dummy(sysp1)      ;
    relval[mmm+14]=3.141592653589793238462643 ;            /* set up PI */
    relval[mmm+13]=1.0 ;                  /* initial display scale is 1 */
   // X=622 ; Y=198 ;                        /* initial display position */
    
    gir= -1 ;                              /* install library functions */
    vartyp=0 ; dummy(func1) ;
    
    prepr1() ;                                 /* reset everything else */
    
    warea[linsiz+1]=lf ;                /* start of precompiled program */
    strcpy(pname,"no_name.src") ;
    
    if ((noteptr=fopen("NOTES.JRN","w"))==NULL) derror(106) ;/* journal */
    
    if (fprintf(noteptr,"------ Journal File: NOTES.JRN ------\n\n")==EOF)
   							 { clearerr(noteptr) ; derror(106) ; }
    
    initX() ;                                         /* start XWindows */

    X=xpos(theScreen, WIDTH);
    Y=ypos(theScreen, HEIGHT);
    
    system("clear") ;                       /* erase display, cursor up */
    
    printf("  D E S I R E   V16.0 (LINUX)\n\n   - Registered Copyright 2012 by G.A. Korn\n   - Distributed under the GNU General Public License listed in the\n     file GPL in this software package. Type  help GPL  to display\n     the complete license and warranty information\n") ;
    tzset() ;                                           /* set timezone */
    WRITELN ; pdtime() ;                       /* display date and time */
    printf("_______________________\
____________________________________________\n\n") ;
    
    }                                                     /* initialize */


