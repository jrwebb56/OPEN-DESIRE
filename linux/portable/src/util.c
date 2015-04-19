/* UTIL.C      Utilities for DESIRE/X  - UNIX                         */
/*             Registered Copyright 1990 G.A. Korn            10/3/91 */

/***>  This source code is copyrighted and distributed under the GNU General
    Public License found in the file GPL.txt in this software package.    <***/

/*             modified trace command                         5/20/99 */

/* ------------------------------------------------------------------ */

#include <time.h>

#include "declare.h"
#include "global.h"

extern void derror() ;  extern void cvalexp() ;  extern void goto00() ;
extern double evalexp() ;

void pdtime() {                               /* writes date and time */

    struct  tm *timeptr ;
    time_t  secsnow ;

    time(&secsnow) ;                 /* timezone was set in initialize */
    timeptr=localtime(&secsnow) ;

    fprintf(outptr,"%d-%d-%02d      %02d:%02d:%2.2d\n",
	    ((timeptr->tm_mon)+1),timeptr->tm_mday,(timeptr->tm_year)-100,
	    timeptr->tm_hour,timeptr->tm_min,timeptr->tm_sec) ;
}                                                         /*pdtime */


void trac00() {                       /* TRACE - set up for debugging */

    if (warea[gi]==lf) {
	flag2=1-flag2 ;
	if (flag2) { printf ("trace ON") ; tracer=7 ; }
	else { printf ("trace OFF") ; tracer=0 ; }
    }
    else {
	no=warea[gi++]-'0' ;
	if (no<0 || no>7) flag2=1-flag2 ;
	else tracer=no ;
    }
}


void auto0() {                            /* automatic line-numbering */

    float   val ;

    if (runf) derror(199) ;
    val=evalexp() ; if (errnum) derror(errnum) ;
    if (val<0 || val>=maxlin) derror(77) ;
    ncol=(int) val ;
}


void restor() {                                  /* RESTORE statement */

    int  savgi ;

    if (warea[gi]==lf || warea[gi]=='|') { datptr=linsiz+1 ; return ; }
    savgi=gi ;
    goto00(1) ;                 /* only comments, else, proceed after! */
    datptr=gi ; gi=savgi ;
    do gi++ ; while (warea[gi]!=lf) ;
}


void irule() {                        /* IRULE n statement; maxrul=10 */

    float  val ;

    if (warea[gi]==lf || warea[gi]=='|')
	printf(" - current integration rule is  No. %d%c",size,'\n') ;
    else {
	val=evalexp() ; if (errnum) derror(errnum) ;
	if (val==6) { printf("*** use irule 2! ***\n") ; derror(86) ; }
	else if (val<0 || val>16) derror(86) ;
	size=val ;
    }
}                                                        /* irule */
