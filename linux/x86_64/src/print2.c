/* PRINT.C     "write" and "note" functions for DESIRE                 */
/*             Registered Copyright 2005 G.A. Korn             2/11/94 */
/** added spare5=0 for console and notebook file output      7/26/05   */
/***>  This source code is copyrighted and distributed under the GNU General
    Public License found in the file GPL.txt in this software package.    <***/

/* ------------------------------------------------------------------- */
#include <fcntl.h>


#include "declare.h"
#include "global.h"

extern  void derror() ;  extern  void gitchn() ;  extern  void  cvalexp() ;
extern  void  matprt() ; extern  void MATPRT() ;  extern  void  scroll() ;

extern  ptr  getvar() ;  extern  double  evalexp() ;

/* ------------------------------------------------------------------- */

void print(int j)                /* WRITE string or value of an expression */
/* CALLED BY execute; USES list above */
{
    unsigned char  ch ; int  i, gisav, gisav1 ; double  accum1,accum2 ;
  
    spare5=0 ;              /* for console and notebook-file output */
    if (j>0) outptr=noteptr ;              /* write to journal file */
    else if (warea[gi]=='#') {              /* need channel number */
	gi++ ;                                             /* skip # */
	if (warea[gi]=='#')         
	{ gi++ ; spare5=1 ; }     /* computer-readable data file! */
	gitchn() ;
	if ((outptr=fd[chanlno])==NULL) derror(64) ;     /* not open */
	if (warea[gi++]!=',') derror(44) ;               /* no comma */
    }
    else outptr=stdout ;               /* write to console terminal */

    if (warea[gi]==lf || warea[gi]=='|') {   /* no "write" argument */
	fputc('\n',outptr) ;
	return ;
    }
    L10 :  ch=warea[gi] ;
    if (ch=='\'' || ch=='\"') {                             /* text */
	if (spare5) derror(84) ;                  /* data file only! */
	gi++ ;
	while (warea[gi]!=ch) fputc(warea[gi++],outptr) ;
	gi++ ;                                        /* skip ' or " */
    }
    else if (ch=='$') {                  /* special ASCII character */
	gi++ ; ch=evalexp() ; if (errnum) derror(errnum) ;
          
/***          if (ch<0 || ch>255) derror(57) ;   not needed in Linux ********/

	fputc(ch,outptr) ;
    }
    else {                                            /* expression */
	i=1 ; gisav=gi ;
	if (ch=='%') {                           /* decimal-to-octal */
	    i= -1 ;
	    if (warea[++gi]=='%') {i= -2 ; gi++ ; } /* decimal-to-hex */
	}

	accum1=evalexp() ;                         /* get expression */

	if (i<0) {                        /* octal or hex conversion */
	    if (i== -1) fprintf(outptr,"%o",(int)accum1) ;
	    else if (i== -2) fprintf(outptr,"%x",(int)accum1) ;
	    goto L200 ;
	}
	if (errnum==31) goto L300 ;                       /* COMPLEX */
	if (errnum==73) goto L400 ;                        /* matrix */
	if (errnum>0) derror(errnum) ;

	gisav1=gi ; gi=gisav ;           /* go back, try for INTEGER */
	p=getvar() ;
	if (errnum>0) { errnum=0 ; goto L100 ; }          /* literal */
	if (p->symtype==2) goto L50 ;                     /* INTEGER */
	if(p->symtype!= -2) goto L100 ;               /* not INTEGER */
	do gi++ ; while (warea[gi]!=']') ;    /* subscripted INTEGER */
	gi++ ;                                             /* skip ] */

	L50 :     if (warea[gi]!=lf && warea[gi]!='|' &&
                      warea[gi]!=',' && warea[gi]!=';') goto L100 ;

	if (accum1>maxint || accum1< -maxint) derror(20) ;

	L51 :     fprintf(outptr,"%d",(int)accum1) ; goto L200 ;

	L300 :    if (!runf) derror(22) ;   /* can't do it in command mode! */
	errnum=0 ; gi=gisav ;                             /* COMPLEX */
	gjj=maxrlvar ; wptr=1 ; flevel=0 ; /* clear evaluation stack */
	cvalexp(&accum1,&accum2) ;

	fprintf(outptr,"%g%s%g%c",accum1," + j*( ",accum2,')') ;
	goto L200 ;

	L400 :    errnum=0 ; gi=gisav ;                /* MAT PRINT */
	gjj=maxrlvar ; wptr=1 ; flevel=0 ;            /* clear stack */
	start=1 ; p=getvar() ; start=0 ;
	if (errnum>0) derror(errnum) ;
	if (warea[gi]==lf || warea[gi]=='|'|| warea[gi]==','||
	    warea[gi]==';') {
	    if (p==NULL) derror(19) ;
	    if (p->symtype>-4) { if (spare5) MATPRT() ; else matprt() ; }
	    else derror(14) ;
	    goto L200 ;
	}
	derror(83) ;

	L100 :    gi=gisav1 ; 
	fprintf(outptr,"%g",accum1) ;     /* write value */
    }                                     /* else expression */

    L200 : if (ferror(outptr)) {
	WRITELN ; clearerr(outptr) ; derror(106) ;    /* can't write */
    }

    if (warea[gi]==lf || warea[gi]=='|') goto L20 ;         /* done */

    if (spare5 && warea[gi]==';') derror(84) ;
    if (warea[gi]==',') {                                   /* more */
	if (!spare5) fputc(tab,outptr) ; else fputc('\n',outptr) ;
    }
    else if (warea[gi]!=';') { WRITELN ; derror(60) ; }

    gi++ ;                                       /* skip ',' or ';' */
    if (warea[gi]==lf || warea[gi]=='|') return ;           /* done */
    goto L10 ;                                     /* more to write */

    L20 :  fputc('\n',outptr) ;
}                                                      /* print */
