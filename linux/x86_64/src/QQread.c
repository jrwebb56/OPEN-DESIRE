/* QQread.c      read and input                                          */
/*     Registered Copyright 2010  G.A. Korn          original file  2/18/92 */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/
/** fixed eof     12/6/06, 12/16/06  **/
/** added error for COMPLEX data and input       12/15/06 **/
/** changed ch for input to integer kk   12/18/06 **/
/** added '/t' to read tab-delimited files     6/22/10 **/
/** added more \t to conform to Windows program   9/12/10 **/

/* ------------------------------------------------------------------------- */
#include <stdlib.h>
#include "declare.h"
#include "global.h"

extern void gitchn() ;   extern void derror() ; extern void cvalexp() ;

extern ptr getvar() ;   extern ptr instal() ;   extern double evalexp() ;
extern int getsub() ;


void getinput(int i)  { 
	
/*  input, read if i=0,1; previously recognized array if flag */

    int  k, li, sub, lj, lk, savgi, num, dim, jj, kk, flag ;
    unsigned char  ch ;  double  val1, val2 ;  ptr  p ;
    FILE  *INPTR ;

    /*** NOTE: eofflg=0 was set in init.c ***/

    if (!runf) derror(22) ;
    flag=FALSE ;                    /* matrix flag */
    lk=1 ;       /* points to bottom of warea for first goaround (not 0) */  
														/*** <*** 12/06  ***/
    if (i>0) goto L10 ;              /* i=1 for "read", i=0 for "input" */

/*                                    note: k<0 indicates file of REAL */
/* ------------------------------------------------------------------------- */
    /*  i=1, "input" */
    if (warea[gi]=='#') {     /* input is from file; get channel number */
	    k=0 ; gi++ ;          /* skip # */
	    gitchn() ;
     	if (warea[gi++]!=',') derror(44) ;        /* need a comma! */  
	    if ((INPTR=fd[chanlno])==NULL) derror(64) ;    /* not open */
        }
    else INPTR=stdin ;                 /* input is from console terminal */

/* -------------------------------------------------- for both read and input */

    L10 :  savgi=li=gi ;  /* save pointer to INPUT variable name for error */

    /*  gi reads user text; li writes prompts; lj writes input to bottom
        	of warea; lk marks items there. 
	           We return here after comma or semicolon.*/
/* ------------------------------------------------------------------------- */

    if (flag) goto L25 ;        /* earlier goaround started an array */
    kk=gi ;                          /* new variable; is it an array? */
    p=getvar() ;
    if (errnum==73) {                                         /* it is an array */
	    errnum=0 ; flag=TRUE ; gi=kk ; goto L25 ; 
        }
    if (errnum) derror(errnum) ;            /* ordinary error in getvar */
    if (vartyp==0) derror(84) ;                 /* no  functions allowed! */
    if (p==NULL) p=instal() ;  
   												 /* new variable, install it ;
       returns Error 19 if  subscripted and array was not defined **/
       
    if (abs(p->symtype)>2) derror(84) ;         /* COMPLEX is illegal ! */ 
    if (p->symtype<0) sub=getsub(p) ;     /* subscripted, needs sub */
    goto L30 ;
    
    L25 :  start=1 ;                     /* array, fix getvar */
    p=getvar() ; start=0 ; if (errnum) derror(errnum) ;
    if (p==NULL) derror(19) ;
    if (abs(p->symtype)>2) derror(84) ; 
    /* COMPLEX is illegal! */ /** redundant?? **/
    jj=p->valptr ; dim=intval[jj] ; num=1 ;
    for (kk=1 ; kk<=dim ; kk++ ) num*=intval[jj+kk] ;        /* get size */
    sub=0 ;   
                             
    L30 :  if (i>0) goto L500 ;               /* i=1, "read" */

/* ------------------------------------------------------------------  i=0, "input" */

    if (warea[lk]==',' || warea[lk]==';') { lk++ ; goto L400 ; }
   													      /* go around again *** ?? ***/
   													      
    L100 : lk=lj=4;           /* reset pointers ahead of stack ptr wptr */
    if (INPTR==stdin) {      /* terminal input */
    	do {                 /*  prompt user with variable name */ 
	        putchar(warea[li]) ;
	        if (warea[li]=='[') { while (warea[li]!=']' && warea[li]!=lf)
	    	putchar(warea[++li]) ; }        /* subscripted */
	        li++ ;
        	} while (warea[li]!=',' && warea[li]!=';' &&
										warea[li]!=lf && warea[li]!='|') ;
	    printf(" ? ") ;
        }                      /* end if INPTR==stdin */
    
   									 /* now copy input string to bottom of warea */ 
                                                               
    L77 :   kk=getc(INPTR) ;
    if (kk==EOF) {
    	printf("END OF FILE encountered on Channel %d\n",chanlno) ;
	    derror(51) ; 
        }           
    if (INPTR==stdin && kk=='!') derror(197) ;               /* escape! */

    if (kk==lf || kk==',' || kk==';'|| kk=='\t' ) goto L400 ;   /*** note tab!! ***/ 
       
    if (kk!=blank) warea[lj++]=kk ;
    goto L77 ;     
    
    L400 :   warea[lj]=lf ;   
    
/**     <******** ?? L400 moved up, 12/8/06  <***/

    lj=gi ; gi=lk ;                /* gi must now work at bottom of warea */
    val1=evalexp() ; if (errnum) goto L405 ;
    if (abs(p->symtype)==2 &&                              /* INTEGER data */
	(val1>maxint || val1< -maxint)) { errnum=20 ; goto L405 ; }
    
    if (warea[gi]!=lf && warea[gi]!=',' && warea[gi]!=';' && warea[gi]!='\t') derror(33) ;
   						 /****> \t added 9/12/10 **/
    
    L405 : lk=gi ; gi=lj ;                                  /* recover pointers */
    if (errnum) {
	    if (INPTR!=stdin || warea[lk]==',' || warea[lk]==';') derror(errnum) ;
	    bye=FALSE ;     /* disables derror return to longjmp to let ... */
	    derror(errnum) ; bye=TRUE ;       /* ... the user correct error */
	    gjj=maxrlvar ; wptr=1 ;                    /* clean evaluation stack */
	    flevel=0 ;
	    li=savgi ;
	    goto L100 ;                       /* and try again */
         }
/* ------------------------- store a data item for both "input" and "read" */
    
    L700 :                                            /* unsubscripted variable? */
	if (p->symtype==1) relval[p->valptr]=val1 ;          /* yes, REAL */
	else if (p->symtype==2) {                                       /*INTEGER */
	    if (val1>maxint || val1< -maxint) derror(20) ;
	    intval[p->valptr]=val1 ;                          
     	}           
   												 	  /* it is a subscripted variable */
   																 
	else if (p->symtype== -1)                        /* REAL */ 
	    relval[intval[intval[p->valptr]+p->valptr+1]+sub]=val1 ;         
    else if (p->symtype== -2) {                      /* INTEGER */
	    if (val1>maxint || val1< -maxint) derror(20) ; 
	    intval[intval[p->valptr]+p->valptr+1+sub]=val1 ;
	    }   
	else derror(84) ;           /* no COMPLEX input allowed */

    if (tracer>3) {
	    printf("%c%s",'\33',"[7m") ;                      /* reverse video */
	    if (abs(p->symtype)==3)
            printf("A: %s%c%g%s%g%s",p->name,'=',val1,"+j*(",val2,")\n") ;
	    else printf("A: %s%c%g\n",p->name,'=',val1) ;
	    printf("%c%s",'\33',"[0m") ;                       /* restore video */
        }
/* ---------------------------------------------------------------- array input */
    if (flag) {
    	li=savgi ;                                                    /* query again */
	    if (++sub<num) goto L30 ;               /* next array element */
        }
/* ------------------------------------------------------------------------ */
    if (warea[gi]==',' || warea[gi]==';') { gi++ ; goto L10 ; }      
/* back for more */
    else return ;    /* or RETURN from getinput(I)      <****************/
   
/* ------------------------------------------  used only for i=1,  "read" */

    L500 : li=datptr ;                /* li looks for "data" statement */
    if (warea[li]!=',' && warea[li]!=';') {
	    
 L550 : if (warea[li]==lf)  {     /* search for "data"statement */
	        li+=4 ;
	        if (li>=endtxt) derror(51) ;
	        }
	    else if (warea[li]!='|') { li++ ; goto L550 ; }
	    else li++ ;
        if (warea[li]!=vdata) goto L550 ;               /* keep looking */
        }       
    li++ ;    /* skip ',',';', or "data" token and evaluate data item */

    savgi=gi ;  gi=li ;                      /* evalexp needs gi */
    if (p->symtype!=3 && p->symtype!= -3) val1=evalexp() ;
    else cvalexp(val1,val2) ;  if (errnum) derror(errnum) ;

    if (warea[gi]!=lf && warea[gi]!='|' && warea[gi]!=','
	                                     && warea[gi]!=';') derror(33) ;
    datptr=gi ; gi=savgi ; goto L700 ;         /* recover pointers */
    
    }                                                                  /* end getinput */


