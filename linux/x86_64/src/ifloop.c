/* IFLOOP.C   if, for, while, and repeat; and seekx()          */
/*            Registered Copyright 1991  G.A. Korn    1/31/91  */

/***>  This source code is copyrighted and distributed under the GNU General
    Public License found in the file GPL.txt in this software package.    <***/

/**            fixed error(24)                          6/5/97 */

/* ----------------------------------------------------------- */

#include <stdlib.h>
#include "declare.h"
#include "global.h"

extern derror() ;      extern ptr getvar() ;
extern ptr instal() ;  extern double evalexp() ;

/* ----------------------------------------------------------- */

void if11() {

    double exp1,exp2 ; int li,relop ;

    old=TRUE ;                            /* old is truth value */

    if (warea[gi+1]!=veof) goto L12 ;       /* check for IF EOF */
    gi=gi+2 ;                                       /* skip EOF */
    if (eofflg) return ;          /* there was an EOF, old=TRUE */
    goto L10 ;

/* ------------------------------------------------ regular IF */

    L12 : exp1=evalexp() ; if (errnum) derror(errnum) ;

    if (warea[gi]=='=') relop=1 ;
    else if (warea[gi]=='<') {
	if (warea[gi+1]=='=')  { relop=2 ; gi++ ; }
	else if (warea[gi+1]=='>')  { relop=3 ; gi++ ; }
	else relop=4 ;
    }
    else if (warea[gi]=='>') {
	if (warea[gi+1]=='=')  { relop=5 ; gi++ ; }
	else relop=6 ;
    }
    else derror(10) ;
    gi++ ;                               /* skip relop symbols */
    exp2=evalexp() ; if (errnum) derror(errnum) ;

    switch(relop) {
	/*EQ*/   
	case 1 : if (exp1==exp2) return ; break ;
	    /*LE*/   
	case 2 : if (exp1<=exp2) return ; break ;
	    /*NE*/   
	case 3 : if (exp1!=exp2) return ; break ;
	    /*LT*/   
	case 4 : if (exp1<exp2)  return ; break ;
	    /*GE*/   
	case 5 : if (exp1>=exp2) return ; break ;
	    /*GT*/   
	case 6 : if (exp1>exp2)  return ;
    } ;                                      /*switch*/
    
 L10: old=FALSE ;
}                                                  /*if11*/


void if00() {              /* for IF, WHILE, UNTIL statements */
    if11() ;
    if (warea[gi]!=ESC) return ; gi++ ;               /*skip ESC*/
    if (warea[gi]==vthen) return ;
    if (warea[gi]==vor && old==0 || warea[gi]==vand && old!=0)
    {gi++ ; if11() ; } ;

    do {             /* must get past second condition, if any! */
	if (warea[gi]==lf || warea[gi]=='|') return ; gi++ ;
    } while (warea[gi]!=vthen) ;
}                                                     /*if00*/



void seekx() {        /* search for end-of-block, check pairs */
                      /*   - USED BY forlop, whil0, rept0     */

    int li, lopcnt ;   unsigned char  ch ;
    li=gi ; lopcnt=1 ;
    do {
	ch=warea[li] ;
	if (ch>vopen && ch<vuntil || ch==vproc) ++lopcnt ;
	else if (ch>=vuntil && ch<vstop) --lopcnt ;
	else if (ch==chr11) if (lopcnt==1) goto L10 ;
	if (lopcnt==0) goto L10 ;
	if (ch==lf) li+=4 ; else li++ ;
    } while (li<endtxt) ;

    derror(24);

    L10 : gi=li ;
}                                                  /* seekx */


void forlop() {                                       /* FOR loop */

    double  exp1, exp2, val ;   int  gisav ;
    dsptr r ;  ptr p ;
    if (runf==0) printf("one-line command-mode loop!!\n") ;
    p=getvar() ; if (errnum) derror(errnum) ;
    if (p==NULL) p=instal() ;
    else if (p->symtype!=1) derror(48) ;
    if (warea[gi++]!='=') derror(46) ;
    relval[p->valptr]=evalexp() ; if (errnum) derror(errnum) ;
    if (warea[++gi]!=vto) derror(23) ;                  /* skip ESC */
    gi++ ;
    exp1=evalexp() ; if (errnum) derror(errnum) ;      /* end value */
    if (warea[gi]==ESC) {
	if (warea[++gi]!=vstep) derror(23) ;
	++gi ;
	exp2=evalexp() ; if (errnum) derror(errnum) ; /* step value */
    }
    else exp2=1 ;
    gisav=gi ;
    seekx() ;            /* search for NEXT, check FOR/NEXT matches */
    if (warea[gi]!=vnext) derror(24) ;

    val=exp1-relval[p->valptr] ; if (exp2<0) val= -val ;
    if (val<0) { gi++ ; return  ; }                  /* test failed */

    /* loop, push control-stack item */

    r=(struct dstak*) malloc((unsigned)sizeof(struct dstak)) ;
    r->head='F' ;
    r->tskptr=gi+1 ;               /* to check for duplicate "next" */
    gi=gisav ;                                 /* go on from old gi */
    r->radd=gi  ;                                     /* for return */
    r->fval=exp1 ; r->step=exp2 ;
    r->cvar=p ;
    r->dslink=stktop ;
    stktop=r ;
}                                                       /*forlop*/


void whil0() {                                      /* WHILE loop */

    int  li, gisav ;  dsptr r ;
    if (runf==0) printf("one-line command-mode loop!!\n") ;
    li=gi ; if00() ;    /* search and check WHILE/END WHILE matches */
    gisav=gi ; seekx() ;        /* li before, gisav after condition */
    if (warea[gi]!=vendwh) derror(24) ;
    if (old==0) { gi++ ; return ; }                  /* test failed */
    /* push control-stack item */
    r=(struct dstak*) malloc((unsigned)sizeof(struct dstak)) ;
    r->head='W' ;
    r->tskptr=gi+1 ;          /* to check for duplicate "end while" */
    gi=gisav ;                                 /* go on from old gi */
    r->radd=li  ;                 /* li is gi before test condition */
    r->dslink=stktop ;
    stktop=r ;
}                                                        /*whil0*/



void rept0() {                                /* REPEAT statement */

    int  gisav ; dsptr r ;

    if (runf==0) printf("one-line command-mode loop!!\n") ;

    gisav=gi ;
    seekx() ;                     /* search for UNTIL, check pairs */
    if (warea[gi]!=vuntil) derror(24) ;

    /* push control-stack item */

    r=(struct dstak*)malloc((unsigned)sizeof(struct dstak)) ;
    r->head='R' ;
    r->tskptr=gi+1 ;             /* to check for duplicate "until" */
    gi=gisav ;                                /* go on from old gi */
    r->radd=gi  ;                                    /* for return */
    r->dslink=stktop ;
    stktop=r ;
}                                                     /* rept0 */

