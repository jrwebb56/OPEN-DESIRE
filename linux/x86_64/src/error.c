/* ERROR.C   DESIRE Error Handler    - UNIX - XWindows                */
/*  Registered Copyright 2007  Granino A. Korn  - old version 11/6/93 */

/***>  This source code is copyrighted and distributed under the GNU General
    Public License found in the file GPL.txt in this software package.    <***/

/**       integrator messages; out of template message       6/26/97  */
/**       add error 232-234; checked agains cnewcom\error.c  5/12/99  */
/*        changed messages for Vectr d/dt, SUBMODEL          9/19/02  */
/*        increased FFT size to 16384                        3/4/08   */
/* ------------------------------------------------------------------ */

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include "declare.h"
#include "global.h"

extern jmp_buf  djump ;

extern void unpack() ; extern void prfile() ;  


void clsall() {         /* close all open files, kill auto-increment */

    int  li ;

    for (li=0 ; li<=maxchan ; li++) {
	if (fd[li]!=NULL) fclose(fd[li]) ;
	fd[li]=NULL ;                                       /* $$$$ */
    }
    inptr=stdin ; append=FALSE ;                         /* for "load" */
    ncol=0 ;                                    /* was in \cnewcom ??? */
    fdn=outptr=stdout ;                           /* for error display */
    eofflg=FALSE ;                /* also in prepr1; but just in case! */
}                                                        /* clsall */


void derror(int nnu) {     /* Error Handler, with longjmp return to prompt */
		/* USES  unpack, clsall */

    int lj, ljj ;

    if (nnu<0) {                          /* error comes from precomp! */
	nn2-=ncol ;                /*  kill auto-increment after derror */
	maxstk=1 ;                        /* this modifies edit command */
	nnu= -nnu ; lineno=0 ;
	if (nnu==13) goto L15 ;             /* line number out of range */
	goto L10 ;
    }

    start=0 ;                          /* in case getvar set start=1 */

    if (nnu==197) { WRITELN ; goto L15 ;}          /* escape "input" */

    for (lj=0 ; lj<= varsize ; lj++)
	if (symbol[lj]==blank) symbol[lj]=EOS ;

    if (runf) {                              /* find the line number */
        lj=linsiz+4 ;
        while (lj<gi) { ljj=lj ; lj+=warea[lj] ; } ;
        lineno=BASE*(warea[ljj-2])+warea[ljj-1] ;
    }
    L10 :
	WRITELN ;
    if (nnu>=301) {
        if (nnu==302) {INdex=gi ; if (tracer%2) goto L5 ; } /*for "go" */
        else INdex=0 ;
        
        fprintf(stderr,"  Stopped at Line %u",lineno) ;
        goto L5 ;
    }

    start=0 ;         /* reset getvar() for non-arrays, just in case */
    clsall() ;                               /* close all open files */

    if (nnu==231) goto L15 ;
    if (spare4) { spare4=0 ; goto L5 ; }
    system("clear") ;
    if (lineno) fprintf(stderr,"Error %d at LINE %u :\n",nnu,lineno) ;
    else fprintf(stderr,"Error %d :\n",nnu) ;

    L15 :

	switch (nnu) {

	    case 1  : 
		fprintf(stderr," undefined statement\n") ; break ;
	    case 2  : 
		fprintf(stderr," array subscript out of bounds\n") ; break ;
	    case 3  : 
		fprintf(stderr," arithmetic overflow\n") ; break ;
	    case 4  : 
		fprintf(stderr," RETURN without CALL\n") ; break ;
	    case 5  : 
		fprintf(stderr," attempt to divide by zero\n") ; break ;
	    case 6  : 
		fprintf(stderr," floating-point format error\n") ; break ;
	    case 7  : 
		fprintf(stderr," missing or misplaced parenthesis\n") ; break ;

	    case 8  :
		fprintf(stderr,
			" more than one display/type list, stash list,\n") ;
		fprintf(stderr,"\trecover list, or SAMPLE statement\n") ; 
		break ;
	    case 9  :
		fprintf(stderr,
			"square-root argument < 0, logarithm argument <=0,\n");
		fprintf(stderr,"\tor power base <=0\n") ; break ;
	    case 10 :
		fprintf(stderr,
			" illegal conditional operator, or missing THEN\n") ;
		break ;
	    case 11 : fprintf(stderr," cannot CONNECT %s\n",fname) ; break ;
	    case 12 : fprintf(stderr," input/output line too long\n") ; break ;
	    case 13 : fprintf(stderr,
			      " bad line number, or out of range\n") ; break ;
	    case 14 : fprintf(stderr," illegal or missing variable, \
or missing parenthesis\n") ; break ;
	    case 15 : fprintf(stderr," identifier too long\n") ; break ;
	    case 16 : fprintf(stderr," only one stash list is allowed\n") ; 
		break ;
	    case 17 : fprintf(stderr,
			      " display/type or stash/recover list too long\n");		break ;
	    case 18 : fprintf(stderr," cannot read\n") ; break ;
	    case 19 : fprintf(stderr," array %s was not declared\n",symbol) ; 
		break ;
	    case 20 : fprintf(stderr," INTEGER overflow\n") ; 
		break ;
	    case 22 :
		fprintf(stderr,
			" this statement is illegal in command mode\n") ; 
		break ;
	    case 23 :
		fprintf(stderr,
			" missing TO, or missing STEP value in FOR statement\n") ;
	    case 24 :
		fprintf(stderr," Missing NEXT, END WHILE,\
or UNTIL; or incorrect nested loops\n") ; 
		break ;
	    case 25 :
		fprintf(stderr," missing bracket, comma, or semicolon \
in array or variable list\n") ; 
		break ;
	    case 26 : fprintf(stderr," unmatched quotes in statement\n") ; 
		break ;
	    case 27 :
		fprintf(stderr,
			" NEXT, END WHILE, or UNTIL without corresponding FOR,\
WHILE,\n") ;
		fprintf(stderr," or REPEAT; or incorrecty nested loops or IF/ELSE clauses\n") ;
		break ;
	    case 28 :
		fprintf(stderr," multiple declaration of variable or array  %s\n",symbol) ; 
		break ;
	    case 29 : fprintf(stderr," missing equal sign, or \
unexpected char. before  =  \n") ; 
		break ;
	    case 30 : fprintf(stderr," channel or file not CONNECTed\n") ; 
		break ;
	    case 31 :
		fprintf(stderr," operation restricted to REAL and INTEGER quantities\n") ;
		break ;
	    case 32 :
		fprintf(stderr," dimension of array %s is <=0 or too large\n",symbol) ;
		break ;
	    case 33 :
		fprintf(stderr," illegal termination of a statement or variable,\n") ;
		fprintf(stderr,"          or illegal variable type\n") ; 
		break ;
	    case 34 :
		fprintf(stderr," supply device and/or file name, in single quotes\n") ;
		break ;
	    case 35 :
		fprintf(stderr," undefined statement - missing blank or tab ?\n"); break ;
	    case 36 :
		fprintf(stderr," illegal argument; check for multiple declaration\n") ;
		break ;
	    case 37 :
		fprintf(stderr," command mode or illegal syntax in FUNCTION or PROCEDURE\n") ;
		break ;
	    case 38 :  fprintf(stderr,"   file %s\n",fname) ;
		fprintf(stderr," not found on the specified device\n") ; 
		break ;
	    case 39 : fprintf(stderr," user-area overflow - program is too large\n") ;
		break ;
	    case 40 : fprintf(stderr," out of storage for REAL or COMPLEX values\n") ;
		break ;
	    case 41 : fprintf(stderr," out of storage for INTEGER values\n") ; 
		break ;
	    case 42 : fprintf(stderr," operator stack overflow\n") ; 
		break ;
	    case 43 :
		fprintf(stderr," GO without STOP, or program change before GO\n") ; 
		break ;
	    case 44 :
		fprintf(stderr," illegal argument delimiter, or missing argument\n") ;
		break ;
	    case 45 :
		fprintf(stderr," illegal file name or file extension; break ; too long?\n");
		break ;
	    case 46 : fprintf(stderr," missing = sign!\n") ; break ;
	    case 47 : fprintf(stderr," argument of SQRT < 0\n") ; break ;
	    case 48 :
		fprintf(stderr," loop control variable must be unsubscripted REAL\n") ;
		break ;
	    case 49 : fprintf(stderr," bad CONNECT statement\n") ; break ;
	    case 50 : fprintf(stderr," illegal channel number\n") ; break ;
	    case 51 : fprintf(stderr," out of data!\n") ; break ;
	    case 52 :
		fprintf(stderr," illegal recursion in a function or procedure\n") ;
		break ;
	    case 53 :
		fprintf(stderr," wrong number of dimensions in array element,\n") ;
		fprintf(stderr,"    or missing bracket\n") ; break ;
		
	    case 54 : fprintf(stderr," + or - expected after E in E-format\n") ;		break ;
	    case 55 : fprintf(stderr," function or procedure is already defined\n") ;
		break ;
	    case 56 :
		fprintf(stderr," arguments in function/procedure call do not match\n") ;
		break ;
	    case 57 : fprintf(stderr," illegal octal or hexadecimal number\n") ;
		break ;
	    case 58 :
		fprintf(stderr," PROCEDURE or SUBMODEL without end ,\n") ;
		fprintf(stderr,"  or incorrectly nested loops or IF/ELSE clauses\n") ;
		break ;
	    case 59 : fprintf(stderr," function %s not defined in DYNAMIC segment\n",symbol); 
		break ;
	    case 60 : fprintf(stderr," missing operator, or illegal variable\n") ;
		break ;
	    case 61 : fprintf(stderr," FFT array is not properly defined \n") ;
		break ;
	    case 62 : fprintf(stderr," only comments may follow LABEL or RETURN \
on the same line\n") ; 
		break ;
	    case 63 : fprintf(stderr,
			      " Channel %d already CONNECTed\n",chanlno) ;
		break ;
	    case 64 : fprintf(stderr," Channel %d not CONNECTed\n",chanlno) ; 
		break ;
	    case 65 : fprintf(stderr," Channel 0 or 1 is DISCONNECTed!\n") ; 
		break ;
	    case 66 : fprintf(stderr," no such device!\n") ; break ;
	    case 67 :
		fprintf(stderr," Channel %d not CONNECTed on DISCONNECT\n"
			,chanlno) ;
		break ;
	    case 68 : fprintf(stderr," non-existent target in GO TO or RESTORE\n") ;
		break ;
	    case 69 : fprintf(stderr," too many dimensions\n") ; break ;
	    case 70 : fprintf(stderr," undefined symbol %s in \
vector/matrix expression\n",symbol) ; 
		break ;
	    case 71 :
		fprintf(stderr," dummy variable %s appears outside\n",symbol) ;
		fprintf(stderr,"  its FUNCTION/PROCEDURE or SUBMODEL definition\n") ;

		break ;
	    case 72 :
		fprintf(stderr," n for FFT must be a power of 2 between 32 and 16384\n") ;
		break ;
	    case 73 :
		fprintf(stderr," same symbol %s used for\n",symbol) ;
		fprintf(stderr,"       variable or procedure and array\n") ; 
		break ;
	    case 74 : fprintf(stderr," attempt to use a non-conformable matrix\n") ;
		break ;
	    case 75 : fprintf(stderr," illegal syntax in vector/matrix or complex \
expression\n") ; 
		break ;
	    case 76 : fprintf(stderr," illegal operator in DYNAMIC segment\n") ;
		break ;
	    case 77 : fprintf(stderr," illegal value of line number increment\n") ;
		break ;
	    case 78 : fprintf(stderr," cannot close channel\n") ; break ;
	    case 79 : fprintf(stderr," LABEL must be at start of line\n") ; 
		break ;
	    case 80 : fprintf(stderr," dimension <1, out of range, or too \
many dimensions\n") ; 
		break ;
	    case 81 :
		fprintf(stderr," only comments, ELSE, or PROCEED may follow GO TO,\n") ;
		fprintf(stderr," EXIT, RESTORE, OLD, CHAIN, drun LABEL \
statements on the same line\n") ; 
		break ;
	    case 82 : fprintf(stderr," illegal or duplicated LABEL\n") ; 
		break ;
	    case 83 :
		fprintf(stderr," we can print arrays, but not matrix expressions!\n") ;
		break ;
	    case 84 :
		fprintf(stderr," attempt to use an illegal type of variable, %s",symbol) ;
		break ;
	    case 85 : fprintf(stderr," illegal recursive matrix operation\n") ;
		break ;
	    case 86 : fprintf(stderr," no such integration rule!\n") ; break ;
	    case 87 : fprintf(stderr," specify TMAX and check DT") ; break ;
	    case 88 : fprintf(stderr,
			      " nested function or procedure definition\n") ;
		break ;
	    case 89 : fprintf(stderr," GO TO was switched OFF!!\n") ; break ;
	    case 90 :
		fprintf(stderr,
			" user-area overflow in DYNAMIC program segment\n") ;
		break ;
	    case 91 : fprintf(stderr," statement illegal in command mode\n") ;
		break ;
	    case 92 : fprintf(stderr,
			      " no stash list, or drun(r)* - rerun?\n") ;
		break ;
	    case 93 : fprintf(stderr,
			      " CHECKN is larger than order of system\n") ;
		break ;
	    case 94 : fprintf(stderr," variable-integration-step deadlock\n") ;
		break ;
	    case 95 : fprintf(stderr," cannot DISCONNECT\n") ; break ;
	    case 96 :
		fprintf(stderr,
			" explicit array elements illegal in stash list\n") ;
		break ;
	    case 97 : fprintf(stderr," maximum PIC version number is 999\n") ;
		break ;
	    case 98 : fprintf(stderr," illegal EXIT operation\n") ; break ;
	    case 99 : fprintf(stderr," no program to save\n") ; break ;
	    case 100 : fprintf(stderr," too many state equations\n") ; break ;
	    case 101 :
		fprintf(stderr,
			" multiple definition of state variable %s",symbol) ;
		break ;
	    case 102 : fprintf(stderr,
			       " derivative code in interpreter segment,\n") ;
		fprintf(stderr," or jump into DYNAMIC program segment\n") ;
		break ;
	    case 103 : fprintf(stderr," can't DISCONNECT channel %d\n",
			       chanlno) ;
		break ;
	    case 104 :
		fprintf(stderr,
			" attempted multiple or illegal use of DYNAMIC\n") ;
		break ;
	    case 105 : fprintf(stderr," delay is negative or too large\n") ;
		break ;
	    case 106 : fprintf(stderr," cannot write\n") ; break ;
	    case 108 :
		fprintf(stderr,
			" no subscripted state variables in this Version\n") ;
		break ;
	    case 109 : fprintf(stderr," DOS hardware error: check device\n") ;
		break ;
	    case 110 : fprintf(stderr," illegal sampling rate\n") ; break ;
	    case 111 : fprintf(stderr,
			       " IF without ELSE, ELSE without PROCEED,\n\
     or incorrect IF/ELSE/PROCEED nesting\n") ; 
		break ;
	    case 112 : fprintf(stderr,
			       " missing or duplicate SHOW without argument") ;
		break ;
	    case 113 : fprintf(stderr,
			       "ABORT: illegal memory reference or C error") ; 
		prfile(-3) ; 
		break ;
	    case 121 : fprintf(stderr," cannot load new shell\n") ; break ;
	    case 122 : fprintf(stderr," DT must be CONSTANT for tdelay\n") ; 
		break ;
	    case 123 : fprintf(stderr,
			       " matrix is singular or ill-conditioned\n",
			       symbol) ;
		break ;
	    case 124 : fprintf(stderr, "iRow must be greater than 0\n") ; 
		break ;
	    case 125 : fprintf(stderr,
			       " illegal erun or load: need a NEW editor window!\n") ;
		break ;
	    case 196 :
		fprintf(stderr," illegal dummy argument in FUNCTION, PROCEDURE, or SUBMODEL\n") ;
		break ;
	    case 197 : fprintf(stderr," Escape from INPUT - variable \
 unchanged, or 0 if new\n") ; 
		break ;
	    case 198 : fprintf(stderr," line number out of range\n") ; break ;
	    case 199 :
		fprintf(stderr,
			" this statement may be used ONLY in command mode\n") ;
		break ;
	    case 200 : fprintf(stderr,
			       " variable or function %s is not defined\n",
			       symbol) ;
		break ;
	    case 201 : fprintf(stderr," procedure or submodel not defined\n",
			       symbol) ;
		break ;
	    case 202 : fprintf(stderr," unimplemented data type!\n") ; break ;
	    case 206 :
		fprintf(stderr,
			" channel number needed in CONNECT statement\n") ; 
		break ;
	    case 207 :
		fprintf(stderr," no compilation in command mode - use run command\n") ;
		break ;
	    case 208 : fprintf(stderr,
			       " display 2 must have a running display\n") ;
		break ;
	    case 209 : fprintf(stderr,
			       " subscripted ARRAY element is illegal here\n");
		break ; 
	    case 210 : fprintf(stderr, "XWindows error\n") ; break ;
		
	    case 215 :
		fprintf(stderr,
			" no DYNAMIC statement or DYNAMIC program segment\n") ;
		break ;
	    case 218 :
		fprintf(stderr,
			" illegal non-REAL or subscripted variable\n") ; 
		break ;
	    case 219 :
		fprintf(stderr,
			" statement illegal in DYNAMIC program segment\n") ; 
		break ;
	    case 224 : fprintf(stderr," integrator is at lower DT limit\n") ; 
		break ;
	    case 225 : fprintf(stderr," requested ERMAX is too small\n") ; 
		break ;
	    case 226 : fprintf(stderr,
			       " integrator corrector did not converge\n") ; 
		break ;
	    case 227 : fprintf(stderr,
			       "implicit integrator can't solve equations\n") ;
		break ;
	    case 220 : case 221 : case 222 : case 223 :  case 228 :
		fprintf(stderr," variable-integration-step deadlock\n") ; 
		break ;
	    case 229 : fprintf(stderr,
			       " irule 10 or 14 needs a JACOBIAN segment\n") ; 
		break ;
	    case 230 : fprintf(stderr," only one JACOBIAN per segment\n") ; 
		break ;
	    case 231 : fprintf(stderr,"click to exit graphics") ; break ;
	    case 232 : fprintf(stderr,"we need more templates!") ; break ;
	    case 233 : fprintf(stderr,"this needs a Vector expression!\n") ; 
		break ;
	    case 234 : fprintf(stderr,"illegal matrix expression\n\
  - try Vector or Vectr d/dt\n") ; 
		break ;
		
/*****/
		
	    case 300 : fprintf(stderr,
			       " statement illegal in interpreter segment\n"); 
		break ;
	    default :  WRITELN ; break ;
	}                                                  /* switch */
    
    if (lineno<=0 || !bye || dflag && dmode>=0
	|| nnu==43 || nnu==197) goto L5 ;   /* do not display error line */
    
    fprintf(stderr,"%c%s",'\33',"[7m") ;    /* reverse video (octal 33) */
    WRITELN ; unpack(ljj-2,lj-2,1) ;      /* display error line via fdn */
    fprintf(stderr,"%c%s",'\33',"[0m\n") ;  /* restore video (octal 33) */
    
 L5: WRITELN ; errnum=0 ; append=0 ;          /* clear load flag, if any */
 runf=FALSE ;
 if (bye) longjmp(djump,1) ;                 /* restart unless input */
}                                                         /* derror */
