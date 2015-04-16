/* LIST.C     "list", "delete", and related functions for DESIRE/X     */
/*             Registered Copyright 2001  G.A. Korn - LINUX            */
/***>  This source code is copyrighted and distributed under the GNU General
    Public License found in the file GPL.txt in this software package.    <***/
/*             added provision for keep+ (i=4)                 5/12/99 */
/*             new ctl-c handler                               8/06/01 */
/***       added Vectr d/dt, Vectr delta, SUBMODEL  9/13/02          ***/
/* ------------------------------------------------------------------- */

#include "declare.h"
#include "global.h"

extern void  derror() ;

/* -------------------------------------- */

int  find00() {                 /* get user-text index for line number */
    /* CALLED BY dlist, old00 */
    register int li ;

    li=linsiz+2 ;
    while (endtxt>li) {
	no=BASE*warea[li]+warea[li+1] ;
	if (no>=lnumber) return(li) ;
	li+=warea[li+2] ;
    }
    return(li) ;
}                                                        /* find00 */


void  unpack(int lno1,int lno2,int i)                   /* for "list" */
/* CALLED BY dlist, execute, prfile */
{
    register int lk ; int  indent, no, cc, lj ;  unsigned char  token, ci ;
    
    indent=0 ;                                     /* indentation level */
    lk=lno1 ;                      /* for register optimization, if any */
    while (lk<lno2) {

	if (spare1) {                              /* look at ctl-c flag */
	    spare1=0 ;
	    if (lj=getchar()!=lf) derror(301) ;  /* lf continues, else stop */
	}   

	if (i>0 && i!=4) fprintf(fdn,"%u", BASE*warea[lk]+warea[lk+1]) ;
	/*** changed for keep+ ***/

	putc(tab,fdn) ; cc=10 ;    /* write tab, reset character counter */
	if (ferror(fdn)) { clearerr(fdn) ; derror(106) ; }

	no=indent ;                                       /* indent line */
	while (no>0) {
	    fprintf(fdn,"  ") ;
	    no-- ; cc+=2 ;            /* no end-of-line check needed here */
	}

	lk+=3 ;                                      /* loop for one line */
	L7 : token=warea[lk++] ;                    /* found a token */
	putc(blank,fdn) ;                         /* blank before keyword */
	if (++cc>=78) {                              /* continuation line */
	    cc=6 ; fprintf(fdn,"_\n") ; }
           
	if (token==vmacro) token=vSUBMOD ;  /* change text to new version */
	else if (token==vMAT) token=vVecdt ;
	else if (token==vDelta) token=vVecdel ;  
          
	if (token==vlet) goto L10 ;                         /* assignment */
	if (token==vlabel) {                             /* set off label */
	    if (i==0) putc('\n',fdn) ;
	    fprintf(fdn,"   ") ;
	    cc+=3 ;                                                     /***/
        }
	else if (token<vstop) {                             /* set indent */
	    if (token>=vuntil) indent-- ;
	    else if (token>vopen) indent++ ;
        }
	else if (token==vproc || token==vmacro || token==vSUBMOD) indent++ ;

	ci=chr11 ; no= -1 ;                      /* find keyword in table */
	while (ci<token) {
	    do no++ ; while (w[no]!='$') ;
	    ci++ ;
        }

	do {                                             /* write keyword */
	    if (++cc>=78) {                           /* continuation line */
		cc=6 ; fprintf(fdn,"_\n") ; }
	    putc(w[++no],fdn) ;
        } while (w[no+1]!='$') ;          /* NOTE: no _ after keyword! */

	if (token==vrem) {                                     /* comment */
	    lk++ ;                                     /* kill first blank */
	    L9 :    if (warea[lk]==lf) goto L12 ;
	    if (++cc<78) putc(warea[lk],fdn) ;
	    lk++ ;                          /* NO comment continuation line */
	    goto L9 ;                                          /* loop back */
        }                                                        /* rem */

	if (token==vthen || token==chr11) goto L7 ;

	if (warea[lk]==lf) goto L12 ;                      /* line is done */
	putc(blank,fdn) ;                           /* space after keyword */
	cc++ ;

	L8 : if (warea[lk]==ESC) { lk++ ; goto L7 ; }    /* another token! */
	L10 : if (warea[lk]=='|') {                   /* another statement */
	    fprintf(fdn," | ") ; lk++ ;
	    cc+=4 ; if (cc>=76) {                     /* continuation line */
		cc=6 ; fprintf(fdn,"_\n") ;
            }
	    goto L7 ;       /* we leave space for next token on this line! */
	}
	if (warea[lk]==lf) goto L12 ;                     /* line is done */

	putc(warea[lk++],fdn) ;                                   /* more */
	if (++cc>=78) {                             /* continuation line? */
	    if (warea[lk]==lf) goto L12 ;              /* no, line is done */
	    cc=6 ; fprintf(fdn,"_\n") ;                             /* yes */
	}
        goto L8 ;

	L12 :   lk++ ; putc('\n',fdn) ;
    }                                            /* while (lk<lno2) */
}                                                         /* unpack */



void  dlist(int i)                  /* "list" or "erase" for i=1,-1 */
{
    register int  li, lj ;  int  lno1, lno2 ;
    if (endtxt<=linsiz+2) {                             /* no text! */
	while (warea[gi]!=lf && warea[gi]!='|') gi++ ;
        return ;
    }
    if (i>=0) { putc('\n',fdn) ; putc('\n',fdn) ; }
    do {
        lnumber=0 ;
        while (warea[gi]>='0' && warea[gi]<='9')
	    lnumber=10*lnumber+warea[gi++]-'0' ;

        if (lnumber==0) {
	    lno1=linsiz+2; lno2=endtxt ;
	    li=0 ; lj=maxlin ;
	}
        else {
	    lno1=find00() ; li=lnumber ;
	    lno2=lno1+warea[lno1+2] ; lj=lnumber ;
	}

        if (warea[gi]=='-') {
	    gi++ ;lnumber=0 ;
	    while (warea[gi]>='0' && warea[gi]<='9')
		lnumber=10*lnumber+warea[gi++]-'0' ;

	    lno2=find00() ; lj=lnumber ;
	    if (lno2<endtxt) lno2=lno2+warea[lno2+2] ;
	}                                                      /* if */

        if (warea[gi]!=lf && warea[gi]!='|' &&
	    warea[gi]!=',') derror(33) ;

        if (i>=0) unpack(lno1,lno2,i) ;                         /* list */
        else {                                                 /* erase */
	    if (dynlin>=li && dynlin<=lj) dynlin=0 ;
	    for (li=0 ; li<= endtxt-lno2 ; li++)
		warea[lno1+li]=warea[lno2+li] ;
	    endtxt=endtxt+lno1-lno2 ;
	    xlength=0 ;                           /* clear compiler area */
	    datptr=linsiz+1 ;                      /* reset data pointer */
	}

        if (warea[gi]==',') {
	    gi++ ;                                           /* skip ',' */
	    if (i>=0) putc('\n',fdn) ;
	    if (warea[gi]<'0' || warea[gi]>'9') derror(44) ;
	}                                                      /* if */

    } while (warea[gi]!=lf && warea[gi]!='|') ;

    if (i>=0) putc('\n',fdn) ;
}                                                         /* dlist */

