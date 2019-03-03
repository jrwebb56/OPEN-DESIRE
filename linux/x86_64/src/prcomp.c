/* PRCOMP.C    Precompiler for DESIRE/X Interpreter - LINUX            */
/*             Registered Copyright 2004 G.A. Korn                     */
/***>  This source code is copyrighted and distributed under the GNU General
    Public License found in the file GPL.txt in this software package.    <***/
/*             uses sh ./ekiller                         7/15/01       */
/*    killed references to SOLARIS editflg, SPARE3       6/14/04       */
/*** changes for Vectr d/dt, Vectr delta, SUBMODEL 9/13/02             */
/* note: to conform to PASCAL version, warea[0] is never used          */
/* ------------------------------------------------------------------- */

#include <string.h>
#include "declare.h"
#include "global.h"

extern void  derror() ;  extern void QuitGraph() ;  extern void prfile() ;


void precomp() {                                        /* precompiler */

    register int  li, wp, lj, lk ;  int  flag3, found, size ;
    float  number ;  unsigned char  token, ch ;

    maxstk=0 ;                               /* no prcomp errors yet */ 
    if (!append) {                          /* if not "load", prompt */
	if (SPARE1>0) {                   /* erun and no new program */
	    SPARE1=0 ;
	    warea[4]=27 ; warea[5]=lf ; gi=5 ;               /* "run" */
	    gjj=maxrlvar ; wptr=1 ;        /* clear evaluation stacks */
	    flevel=0 ; runf=FALSE ;
	    return ;                                    /* go execute */
	}
	if (SPARE4>0) prfile(-3) ;  /* bye, SPARE4 was set in excut2 */
	if (ncol) printf("%u> ",nn2+ncol) ;
	else printf("> ") ;
    }                            

    L30 :  li=4 ;                /* allows spare locations for line number */
    L31 :  if ((lj=getc(inptr))==EOF) {  /* loop reads file or device line */
	if (ferror(inptr)) { clearerr(inptr) ; derror(18) ; }
	eofflg=1 ; goto L5 ;                           /* for "load" */
    }
    if (lj==lf) {                                    /* end of line */
	if (warea[li-1]=='_') {         /* continuation line, skip _ */
	    warea[--li]=lf ; goto L31 ;
	}
	warea[li]=lf ; goto L32 ;              /* end of line, parse */
    }
    warea[li++]=lj ;                         /* read next character */
    if (li==linsiz) { errnum= -12 ; goto L5 ; } /* line is too long */
    goto L31 ;                                     /* loop for more */

    L32 :  wp=4 ;                                        /* parse the line */
    while (warea[wp]==blank || warea[wp]==tab) wp++ ;       /* skip */
    if (warea[wp]==lf) {                /* he typed RETURN key only */
	if (spare3) QuitGraph() ;                        /* quit X11 */
	goto L120 ;
    } 
    li=wp ;                                 /* mark first non-blank */
    flag3=0 ;     /* three-valued flag, 0 for 1st keyword in a line */

    if (warea[wp]=='0') {                 /* line number 0 or error */
	if (warea[wp+1]!=lf) goto L35 ;           /* bad line number */
	lineno=nn2=0 ; goto L120 ;
    }
    number=0 ;                                   /* get line number */
    while (warea[wp]>='0' && warea[wp]<='9')
	number=10*number+warea[wp++]-'0' ;

    if (number==0) {                        /* no line number, or 0 */

	if (warea[wp]=='/') {           /* '/' switches from/to AUTO */
	    warea[wp]=blank ;                    /* blank out the '/' */
	    if (warea[wp+1]==lf) {                     /* toggle AUTO */
                if (ncol==0) {                                   /* ON */
		    ncol=mparea ;
		    if (lineno==0) lineno=nn2 ;
		}                                             /* ON */
                else {                                          /* OFF */
		    mparea=ncol ; ncol=0 ;
		    if (lineno>0) { nn2=lineno ; lineno=0 ; }
		}                                            /* OFF */
                goto L120 ;
	    }                                       /* toggle AUTO */
	}                                               /* if '/' */
	else if (ncol) {                 /* no '/', AUTO line number */
	    if (lineno==0) lineno=nn2 ;              /* saved earlier */
	    lineno=lineno+ncol ;             /* auto-increment lineno */
	    nn2=lineno ;                           /* for next prompt */
	    warea[1]=lineno/BASE ; warea[2]=lineno%BASE ;   /* lineno */
	    goto L40 ;                                /* find keyword */
	}                                           /* IF ncol<>0 */
	if (lineno>0) { nn2=lineno ; lineno=0 ; }/* command, save no */
	goto L40 ;
    }                                             /* IF number=0 */
    if (number<maxlin) goto L36 ;                   /* program mode */

    L35 :  fprintf(stderr,"Error 13 at Line ") ;    /* bad line number */
    for (lj=li ; lj<=wp-1 ; lj++) putchar(warea[lj]) ;
    putchar(':') ;
    errnum= -13 ; goto L5 ;

/* ------------------------------------------------------------------- */
    /* process the line */
    L36 :  lineno=number ;
    for (lj=li ; lj<= wp-1 ; lj++) warea[lj]=blank ;/* blank old no */
    if (warea[wp]!=blank && warea[wp]!=tab && warea[wp]!=lf) {
	errnum= -35 ; goto L5 ;
    }
    /* need space or tab after line number */
    while (warea[wp]==blank || warea[wp]==tab) wp++ ;

    if (warea[wp]==lf) {
	size=0 ; token=EOS ; goto L110 ;   /* delete line if only lf */
    }
    warea[1]=lineno/BASE ; warea[2]=lineno%BASE ;  /* encode lineno */
    goto L41 ;                        /* we already skipped blanks! */

/*---------------------------------------------------------------------*/
/*     search the array warea for a keyword; substitute token          */
/*---------------------------------------------------------------------*/

    L40 :  while (warea[wp]==blank || warea[wp]==tab) wp++ ;
    L41 :  found=FALSE ; lk=0 ;                                /* 0, not 1 */
    if (warea[wp]<'@' && warea[wp]!='-') goto L45 ;
    /* no keywords start like this! */
    li=wp ; token=chr11 ;                     /* offset token by 10 */

    L42 :  while (warea[wp]==w[lk]) {wp++ ; lk++ ; }
    if (w[lk]=='$' && (warea[wp]==blank || warea[wp]==tab
		       || warea[wp]==lf || warea[wp]=='|' || warea[wp]=='-')) {
	found=TRUE ;                          /* found, insert token */
	if (flag3<0) warea[li++]=ESC ;

	if (token==vVecdt) warea[li]=vMAT ; /* FIX FOR NEW NOTATION! <******/
	else if (token==vVecdel) warea[li]=vDelta ;
	else if (token==vSUBMOD) warea[li]=vmacro ;
	else warea[li]=token ;
          
	for (lk=li+1 ; lk<= wp-1 ; lk++) warea[lk]=blank ;
	goto L45 ;
    }

    while (w[lk]!='$') lk++ ;               /* next keyword, if any */
    lk++ ; token++ ;                                    /* skip '$' */
    wp=li ;
    if (w[lk]!=0) goto L42 ;                     /* end of w string */

/* ------------------------------------------------------------------- */

    L45 :  if (flag3<0) {                      /* search was after IF etc. */
	if (token==vthen) goto L72 ;
	goto L70 ;
    }

    if (token==vrem || token==vat) goto L20 ;         /* comment, @ */

    if (!found) {                           /* "let" or bad keyword */
	lj=wp ;
	do if (warea[++lj]=='=') goto L50 ;     /* if "let", lj on = */
	while (warea[lj]!=lf && warea[lj]!='|') ;
	errnum= -1 ; goto L5 ;                          /* undefined */

    L50:      wp=lj+2 ;                             /* it is "let", skip = */
	while (warea[lj]!=lf) lj++ ;             /* and insert yoken */
	for (lj=lj+1 ; lj>li ; lj--) warea[lj]=warea[lj-1] ;
	warea[li]=vlet ; 

	L51 :                                     /* skip "let" expression */
	    if (warea[wp]==lf) goto L20 ;                   /* next line */
	if (warea[wp++]== '|') goto L72 ;          /* next statement */
	goto L51 ;                                      /* loop back */

    }                                            /* if not found */

    if (token>vuntil) goto L65 ;           /* two or more keywords? */
    flag3= -1 ;                    /* yes, may have to search again */

    if (token==chr11) goto L72 ;                      /* for "else" */
    L65 :  if (token!=vdynmc) goto L70 ;                    /* for DYNAMIC */
    if (dynlin>0 && dynlin!=lineno || flag3!=0) goto L66 ;
    while (warea[wp]==blank || warea[wp]==tab) wp++ ;
    if (warea[wp]==lf) goto L20 ;
    L66 :  errnum= -104 ; goto L5 ;

    L70 :  if (warea[wp]==lf) goto L20 ;                      /* next line */
    if (warea[wp++]!='|') goto L73 ;
    L72 :  flag3=1 ; goto L40 ;                          /* next statement */

    L73 :  if (flag3<0) goto L40 ;                         /* search again */
    goto L70 ;

/* ------------------------------------------------------------------- */
/*    kill blanks/tabs except in comments, @ commands, quoted items    */
/* ------------------------------------------------------------------- */

    L20 : lj=3 ; li=4 ;         /* start after line number and length code */
    L80 : lj++ ; while (warea[lj]==blank || warea[lj]==tab) lj++ ; /* skip */

    if (warea[lj]==vrem || warea[lj]==vat) {         /* comment or @ */
	do warea[li++]=warea[lj++] ; while (warea[lj]!=lf) ;
	warea[li]=lf ;
	goto L100 ;              /* comment or @ line has been copied */
    }
    else warea[li++]=warea[lj++] ;                           /* copy */

    L90 : while (warea[lj]==blank || warea[lj]==tab) lj++ ;        /* skip */

    ch=warea[lj] ;              /* check for single or double quotes */
    if (ch=='\'' || ch=='\"') {
	do {
            warea[li++]=warea[lj++] ;                          /* copy */
            if (warea[lj]==lf || warea[lj]=='|') { /* unmatched quotes */
		errnum= -26 ; goto L5 ;
	    }
	} while (warea[lj]!=ch) ;
    }                                      /* quoted item is done */

    if ((warea[li]=warea[lj])==lf) goto L100 ;       /* line is done */
    li++ ;                                         /* next statement */
    if (warea[lj]=='|' || warea[lj]==vthen) goto L80 ;
    lj++ ;
    goto L90 ;                                         /*  loop back */

/* ------------------------------------------------------------------- */
/*      if no line number was given, then get ready to execute         */
/* ------------------------------------------------------------------- */

    L100 : if (lineno==0) {
	gi=5 ;               /* gi past token for subsequent execute */
	gjj=maxrlvar ; wptr=1 ;           /* clear evaluation stacks */
	flevel=0 ;                          /* and parenthesis level */
	runf=FALSE ;                                /* just in case! */
	goto L5 ;                                      /* go execute */
    }                                           /* if ( lineno=0 */

/* -------------------------------------------------------------------- */
/*     else transfer line to correct location in user text area         */
/*         (warea) above li=linsiz; if size=0 then erase line           */
/* -------------------------------------------------------------------- */

    warea[3]=size=li ;                       /* length code for line */
    L110 : li=linsiz+2 ;
    L111 : if (li>=endtxt) goto L112 ;
    no=BASE*warea[li]+warea[li+1] ;
    if (no>lineno) goto L112 ;
    lj=warea[li+2] ;
    if (no==lineno) { flag3=size-lj ; goto L113 ; }
    li+=lj ; goto L111 ;
    /* "flag3" is now size difference */
    L112 : flag3=size ;
    L113 : if (endtxt+flag3>maxtxt) { errnum= -39 ; goto L5 ; }

    if (li<endtxt) {
	if (flag3>0) for (lj=endtxt-1 ; lj>=li ; lj--)
	    warea[lj+flag3]=warea[lj] ;
	else if (flag3<0) for (lj=li+size ; lj<=endtxt-1 ; lj++)
	    warea[lj]=warea[lj-flag3] ;
    }
    endtxt+=flag3 ;
    for (lj=1 ; lj<=size ; lj++) warea[li+lj-1]=warea[lj] ;

/* -------------------------------------------------------------------- */

    if (lineno==dynlin) dynlin=0 ;             /* DYNAMIC was erased */
    if (token==vdynmc) dynlin=lineno   ;        /* mark DYNAMIC line */
    datptr=linsiz+1 ;                          /* reset data pointer */
    xlength=0 ; nrow=0 ;              /* kill compiler code; no "go" */

    L120 : if (!append) {                                  /* if not "load" */
	if (lineno>0) nn2=lineno ;                   /* in auto mode! */
	if (ncol) printf("%u> ",nn2+ncol) ;             /* new prompt */
	else printf("> ") ;
	goto L30 ;
    }
    if (!eofflg) goto L30 ;

    L5 :   if (append && (eofflg || errnum)) {              /* load is done */
	WRITELN ;
	ncol=0 ; append=FALSE ;
	fclose(inptr) ; inptr=stdin ;         /* fclose just in case! */
	strcpy(fname,"con") ;                    /* for error message */
	if (!errnum) derror(301) ;
    }
    if (errnum) derror(errnum) ;
}                                                     /* precomp */
