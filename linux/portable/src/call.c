/* ------------------------------------------------------------------- */
/* CALL.C      go to, call, and invoke                                 */
/*             Registered Copyright 1991 G.A. Korn             1/31/91 */

/***  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    ***/
/* ------------------------------------------------------------------- */

#include <string.h>
#include <stdlib.h>
#include "declare.h"
#include "global.h"

extern void derror() ;  extern void cvalexp() ;

extern ptr getvar() ;   extern ptr instal() ;   extern double evalexp() ;
extern int getsub() ;


void goto00(int i)                  /*  GO TO if i=1, else LABEL */
{
    ptr  p ;  symbl  oname ; register int  li ; int  gisav ;
    double  val ;
    
    if (i==0) {                                            /* LABEL */
	if (! runf) derror(22) ;
	if (warea[gi-5]==lf) goto L100 ;
	derror(79) ;                         /* not at start of line */
    }
    if (warea[gi]<'0' && warea[gi]!='(' || warea[gi]>'9') goto L100 ;
    
/* ------------------------------------------------------------------- */
                                            /* branch to line number   */
/* ------------------------------------------------------------------- */

    val=evalexp() ; if (errnum) derror(errnum) ;
    if (val<1 || val>maxlin) derror(13) ;    /* invalid line number */
    if (warea[gi]=='|') {
	if (warea[gi+1]!=vrem && warea[gi+1]!=chr11 &&
	    warea[gi+1]!=vendls) derror(81) ;
    }
    else if (warea[gi]!=lf) derror(33) ;         /* bad termination */
    
    li=linsiz+2 ;                                         /* search */
    do {
	if (BASE*warea[li]+warea[li+1]==(int) val) goto L8 ; /* found*/
	li+=warea[li+2] ;                     /* no, add line length */
    } while (li<endtxt) ;
    derror(68) ;                                       /* not found */
    
    L8 :   if (tracer>0) { if (tracer==2 || tracer==3 || tracer>=6) {
	printf("%c%s",'\33',"[7m") ;           /* ANSI reverse video */
	printf("%s%u%c","G:",lnumber,'\n') ;
	printf("%c%s",'\33',"[0m") ;           /* ANSI restore video */
    }}
    gi=li-1 ; return ;                 /* gi is on lf before target */
    
/* ------------------------------------------------------------------- */
    /* branch to symbolic label    */
/* ------------------------------------------------------------------- */
    
    L100 : p=getvar() ; if (errnum) derror(errnum) ;
    if (vartyp<=0) derror(82) ;                    /* illegal label */
    
    if (warea[gi]=='|') {                       /* faster than AND? */
	if (warea[gi+1]!=vrem) {
	    if (i==0) derror(62) ;                           /* LABEL */
	    if (warea[gi+1]!=chr11 && warea[gi+1]!=vendls) derror(81) ;
	}
    }                                                    /* if | */
    else if (warea[gi]!=lf) derror(33) ;         /* bad termination */
    
    if (p!=NULL) {                                  /* label exists */
	if (p->symtype!=5) derror(82) ;             /* illegal label */
	if (i==0) {                                         /* LABEL */
	    if (intval[p->valptr]!=gi) derror(82) ;      /* duplicate */
	    return ;                                /* skip the label */
	}
	gi=intval[p->valptr] ;                      /* execute GO TO */
	goto L10 ;
    }
    
    if (i==0) return ;                        /* new LABEL, skip it */
    
    gisav=gi ; /* GO TO, search for target label; save gi for error */
    
    strcpy(oname,symbol) ; li=linsiz+6 ; /* start after first token */
    while (li<endtxt) {                                   /* search */
	if (warea[li-1]==vlabel) {
	    gi=li ; p=getvar() ; if (errnum) derror(errnum) ;
	    li=gi ;
	    while (warea[li]!=lf) li++ ;            /* get to next lf */
	    if (strcmp(symbol,oname)==0) { gi=li ; goto L300 ; }
	    /* success */
	    li+=5 ;                          /* get behind next token */
	}
	else li+=warea[li-2] ;                    /* add line length */
    }                                                   /* while */
    gi=gisav ; derror(68) ;                 /* search failure at gi */
    
    L300 : if (gii+1>maxinvar) derror(41) ;
    p=(struct symnode*)malloc(sizeof(struct symnode)) ;
    p->SWITCH=TRUE ;                           /* install new label */
    p->link=hashtab[hashval] ;
    strcpy(p->name,symbol) ;
    p->symtype=5 ;
    hashtab[hashval]=p ;
    p->valptr=gii ;
    intval[gii++]=gi ;
    
    L10 :  
	if (tracer>0) {
	    if (tracer==2 || tracer==3 || tracer>=6) {
		printf("%c%s",'\33',"[7m") ;           /* ANSI reverse video */
		printf("%s%s%c","G: GO TO LABEL ",symbol,'\n') ;
		printf("%c%s",'\33',"[0m") ;           /* ANSI restore video */
	    }
	}
}            /* goto00 */


void call(int i)                              /* CALL, INVOKE for i = 0, 1 */
{
    int  gkk, li, sym ;  double  val ;
    ptr  p, q, s ;   dsptr  r ;

    if (!runf) derror(22) ;
    p=getvar() ; if (errnum) derror(errnum) ;
    if (p==NULL) derror (201) ;                        /* undefined */
    /* defined, get parameters */
    sym=p->symtype ;                                   /* save type */
    if (i==0 && sym<8 || i>0 && sym!= -59) derror(201) ;
    p->SWITCH=FALSE ;                          /* prevent recursion */
    q=p ;          /* save pointer to procedure or macro definition */

    if (warea[++gi]==')') {                         /* done or ...  */
	if (p->next==NULL) goto L10 ; derror(56) ; }    /* ... mismatch */
    if (p->next==NULL) derror(56) ;                     /* mismatch */
    p=p->next ;                      /* else link to first argument */

    if (i>0) goto L85 ;                                   /* invoke */
    if (warea[gi]==';') {
	if (sym==8) goto L80 ; derror(56) ; } /* call */
    if (sym==8) derror(56) ;                            /* mismatch */

    li=0 ;                          /* call, count value parameters */
    L9 :   li++ ;
    p->SWITCH=TRUE ;                      /* turn local variable ON */
    if (p->symtype==1) relval[p->valptr]=evalexp() ;        /* REAL */
    else if (p->symtype==3)                              /* COMPLEX */
	cvalexp(relval[p->valptr],relval[p->valptr+1]) ;
    else {
	val=evalexp() ;if (errnum) derror(errnum) ;       /* INTEGER */
	if (val>maxint || -val>maxint) derror(20) ;
	intval[p->valptr]=(int) val ;
    }
    if (errnum) derror(errnum) ;

    if (warea[gi]==')') {
	if (p->next==NULL) goto L10 ; derror(56) ; }    /* done */
    if (p->next==NULL) derror(56) ;                     /* mismatch */
    p=p->next ;                       /* else link to next argument */

    if (warea[gi]==';') {               /* no more value parameters */
	if (sym==8+li) goto L80 ; derror(56) ; }
    if (warea[gi]==',') { gi++ ; goto L9 ; } derror(56) ;   /* next */

    L80 :  gi++ ;             /* skip ';' or ',', deal with VAR parameters */
    L85 :  p->SWITCH=TRUE;                       /* turn local variable ON */
    gkk=gi ; s=getvar() ;                     /* might be an array! */
    if (errnum) {
	if (errnum==73) {
	    errnum=0 ;
	    gi=gkk ; start=1 ; s=getvar() ; start=0 ;
	    if (errnum) derror(errnum) ;
	    if (s==NULL || s->symtype!= -1) derror(196) ;
	    p->valptr=s->valptr ;               /* pass array address */
	    goto L87 ;
	}
	derror(errnum) ;
    }

    /*$$*/ if (s==NULL) s=instal() ;             /* no array, install REAL */

    if (s->symtype== -1 && p->symtype==1) goto L90 ;
    if (s->symtype!=p->symtype && s->symtype!= -p->symtype) derror(56) ;
    /* no match */
    L90 :  if (s->symtype>0) p->valptr=s->valptr ;    /* pass by reference */
    else {
	if (s->symtype== -1)                         /* relval index */
	    p->valptr=intval[intval[s->valptr]+s->valptr+1]+getsub(s) ;
	else if (s->symtype== -2)                    /* intval index */
	    p->valptr=intval[s->valptr]+s->valptr+1+getsub(s) ;
	else p->valptr=intval[intval[s->valptr]+s->valptr+1]+2*getsub(s) ;
    }

    L87 :  if (warea[gi]==')') {
	if (p->next==NULL) goto L10 ; derror(56) ; }
    if (p->next==NULL) derror(56) ;
    p=p->next ;

    if (warea[gi]==',') goto L80 ;            /* next VAR parameter */
    derror(33) ;                                 /* bad termination */

    L10 :  gi++ ;                                                /* skip ) */
    /* control stack for return */

    r= (struct dstak*) malloc(sizeof(struct dstak)) ;
    r->head='C' ;
    r->cvar=q ;
    r->radd=gi ;                                      /* on lf or | */
    r->dslink=stktop ;
    stktop=r ;
    flag1=TRUE ;                     /* prevents nested definitions */
    gi=q->valptr ;                              /* go to subroutine */

    if (tracer>0) {
	if (tracer==2 || tracer==3 || tracer>=6) {
	    printf("%c%s",'\33',"[7m") ;        /* ANSI reverse video */
	    printf("%s%s%c","G: EXECUTING PROCEDURE ",q->name,'\n') ;
	    printf("%c%s",'\33',"[0m") ;        /* ANSI restore video */
	}
    }
}                                                       /* call */

