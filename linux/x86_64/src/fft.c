/* FFT.C      FFTs, Convolution       - UNIX                           */
/*        Registered Copyright 2010 G.A. Korn   - old version 7/22/93 */

/***>  This source code is copyrighted and distributed under the GNU General
    Public License found in the file GPL.txt in this software package.    <***/

/*  increased possible FFT size to 8192          10/01/05*/
/*  increased to 16384     1/9/10  */

/* ------------------------------------------------------------------- */

#include "declare.h"
#include "global.h"

extern void derror() ;   extern ptr getvar() ;   extern double evalexp() ;


void fft() {                                      /*  FFT switch,n,x,y */

    int  n,i,j,k,kmax,istep,nv2,startx,starty,startu,startv,
	starta,startb,startc,startd,first ;
    double  IPI,theta,tr,ti,ur,ui,wr,wi ;
    unsigned char  SWITCH ;  ptr  p ;

    SWITCH=warea[gi++] ;                     /* SWITCH = F,I,R, or C */
    if ( warea[gi++]!=',') derror(56) ;                    /* skip , */
    tr=evalexp() ; if (errnum) derror(errnum) ;            /* read n */
    n=tr ;                                              /* truncated */
    if (n!=1024 && n!=512 && n!=256 && n!=2048 && n!=4096 && n!=8192
    && n!=16384		                                                /** changed 1/9/10  <****/
	&& n!=128 && n!=64 && n!=32) derror(72) ;
    nv2=n/2 ;

    first=TRUE ; start=1 ;        /* start=1 fixes getvar for arrays */
    L10 : if (warea[gi++]!=',') derror(56) ;                     /* skip , */
    p=getvar() ; if (errnum) derror(errnum) ;
    if (p==NULL) derror(19) ;
    i=p->valptr ;
    if (p->symtype!= -1 || intval[i]!=1 || intval[i+1]<n) derror(61) ;
    if (first) {
	startx=intval[i+2]-1 ; first=FALSE ; goto L10 ;
    }
    else starty=intval[i+2]-1 ;

    starta=startx+n+2 ; startb=starty+n+2 ;

    if (SWITCH=='R') {                             /* 2 extra arrays */
	first=TRUE ;
	L20 :    if (warea[gi++]!=',') derror(56) ;
	p=getvar() ; if (errnum) derror(errnum) ;
	if (p==NULL) derror(19) ;
	i=p->valptr ;
	if (p->symtype!= -1 || intval[i]!=1 || intval[i+1]<n) derror(61) ;

	if (first) {
            startu=intval[i+2]-1 ; first=FALSE ; goto L20 ;
	}
	else startv=intval[i+2]-1  ;

	startc=startu+n+2 ; startd=startv+n+2 ;
    }                                              /* if SWITCH=R */

    L30 : j=1 ;                                             /* bit shuffle */
    for (i=1 ; i<=n ; i++) {
	if (i<j) {
            tr=relval[startx+j] ; ti=relval[starty+j] ;
            relval[startx+j]=relval[startx+i] ;
            relval[starty+j]=relval[starty+i] ;
            relval[startx+i]=tr ; relval[starty+i]=ti ;
	}
	k=nv2 ;
	while (k<j) { j=j-k ; k=(k+1)/2 ; }
	j+=k ;
    }

    if (SWITCH=='I') { IPI=PI ; } else IPI= -PI ;
    kmax=1 ;
    while (kmax<n) {
	istep=2*kmax ;
	for (k=0 ; k<=kmax-1 ; k++) {
            theta=IPI*k/kmax ;
            wr=cos(theta) ; wi=sin(theta) ;
            i=k+1 ;
            do {
		j=i+kmax ;
		tr=wr*relval[startx+j]-wi*relval[starty+j] ;
		ti=wr*relval[starty+j]+wi*relval[startx+j] ;
		relval[startx+j]=relval[startx+i]-tr ;
		relval[starty+j]=relval[starty+i]-ti ;
		relval[startx+i]=relval[startx+i]+tr ;
		relval[starty+i]=relval[starty+i]+ti ;
		i+=istep ;
	    } while (i<=n) ;
	}                                                   /* FOR */
	kmax=istep ;
    }                                                    /* WHILE */

    if (SWITCH=='F') { start=0 ; return ; }           /* forward FFT */
    if (SWITCH=='I') {                                /* inverse FFT */
	for (i=1 ; i<=n ; i++) {
            relval[startx+i]=relval[startx+i]/n ;
            relval[starty+i]=relval[starty+i]/n ;
	}
    }
    else {
	if (SWITCH=='C') {
            relval[startx+1]=relval[startx+1]*relval[starty+1] ;
            relval[starty+1]=0 ;
	}
	else if (SWITCH=='R') {
            relval[startu+1]=0 ;
            relval[startv+1]=0 ;
	}
	else derror(300) ;                          /* illegal SWITCH */

	for (i=2 ; i<=nv2 ; i++) {      /* convolution or double REAL */

            tr=0.5*(relval[startx+i]+relval[starta-i]) ;  /* Re(G) */
            ur=0.5*(relval[starty+i]+relval[startb-i]) ;  /* Re(H) */
            ti=0.5*(relval[starty+i]-relval[startb-i]) ;  /* Im(G) */
            ui=0.5*(relval[starta-i]-relval[startx+i]) ;  /* Im(H) */
 
            if (SWITCH=='C') {                          /* convolution */
		wr=tr*ur-ti*ui ; wi=tr*ui+ti*ur ;
		relval[startx+i]=wr ; relval[starta-i]=wr ;
		relval[starty+i]=wi  ; relval[startb-i]= -wi ;
	    }
            else if (SWITCH=='R') {                   /* two REAL FFTs */
		relval[startx+i]=tr ; relval[starta-i]=tr ;
		relval[starty+i]=ur ; relval[startb-i]=ur ;
		relval[startu+i]=ti ; relval[startc-i]= -ti ;
		relval[startv+i]=ui ; relval[startd-i]= -ui ;
	    }
	}                                                  /* FOR */

	if (SWITCH=='C') { SWITCH='I' ; goto L30 ; }
    }                                                       /* if */
    start=0 ;
}                                                         /* fft */

