/* RULE1TO8.C     Euler and RK Integration Rules  - UNIX               */
/*   Registered Copyright 2004 G.A. Korn        - old version  8/21/93 */
/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/
/*             corrected Rule 5                         5/12/99 */          
/*         changed relvalmmm+20] to IDERIV, ztnext to ztnext   9/20/01 */
/* ------------------------------------------------------------------- */

#include "declare.h"
#include "global.h"

extern void derror() ;  extern void drun00() ;

#define UROUND 1.0e-12
#define nn1LOOP for (i=1 ; i<=nn1 ; i++)

void ru1() {                             /*  2nd-order RK Default Rule */
    register int  i ;  double reg ;
    goto L20 ;                    /* first derve1 done before entry! */
    L10 : drun00(2) ; if (errnum) return ;
    L20 : IDERIV= -1 ;                                     /* reset IDERIV */

    nn1LOOP {
	reg=sderiv[i]*hdt ;                              /* reg=rk1/2 */
	dpsv[i]+=reg ;                           /* save dpsv=x+rk1/2 */
	relval[i]=dpsv[i]+reg ;               /* x=x+rk1 for drun00(2) */
    }
    relval[mmm+5]+=relval[mmm+3] ;                     /* t = t + DT */

    drun00(2) ; if (errnum) return ;
    nn1LOOP relval[i]=dpsv[i]+=sderiv[i]*hdt ;

    IDERIV=1 ;                                         /* set IDERIV */
    if (relval[mmm+5]-ztnext<0) goto L10 ;
}



void ru2() {                                /*  Euler Integration Rule */

    register int  i ;

    goto L20 ;                    /* first derve1 done before entry! */
    L10 : drun00(2) ; if (errnum) return ;

    L20 : nn1LOOP relval[i]+=sderiv[i]*relval[mmm+3] ;
    relval[mmm+5]+=relval[mmm+3] ;                     /* t = t + DT */
    if (relval[mmm+5]-ztnext<0) goto L10 ;
}                                                         /* ru2 */



void ru3() {                            /*  4th-order Runge-Kutta Rule */

    register int  i ;
    double  hdt, dto6, rk3, rk1[maxnsv+1], rk2[maxnsv+1] ;

    goto L20 ;                    /* first derve1 done before entry! */
    L10 : drun00(2) ; if (errnum) return ;
    L20 : IDERIV= -1 ;                                     /* reset IDERIV */
    hdt=0.5*relval[mmm+3] ; dto6=relval[mmm+3]/6 ;
    /* local here; can change */
    nn1LOOP {
	rk1[i]=sderiv[i] ;
	relval[i]=dpsv[i]+rk1[i]*hdt ;
    }
    relval[mmm+5]+=hdt ;                             /* t = t + DT/2 */

    drun00(2) ; if (errnum) return ;
    nn1LOOP {
	rk2[i]=sderiv[i] ;
	relval[i]=dpsv[i]+rk2[i]*hdt ;
    }

    drun00(2) ; if (errnum) return ;
    nn1LOOP {
	rk3=sderiv[i] ;
	relval[i]=dpsv[i]+rk3*relval[mmm+3] ;
	rk2[i]+=rk3 ;                /* no need to store both rk2,rk3 */
    }
    relval[mmm+5]+=hdt ;                             /* t = t + DT/2 */

    drun00(2) ; if (errnum) return ;
    nn1LOOP relval[i]=dpsv[i]+=(rk1[i]+2*rk2[i]+sderiv[i])*dto6 ;

    IDERIV=1 ;                                         /* set IDERIV */
    if (relval[mmm+5]-ztnext<0) goto L10 ;
}                                                         /* ru3 */



void ru4() {                             /*  RK 4/2 Variable-step Rule */

    register int  i ;  int  FO ;  double  time, dto6,
				      hdt, rk3, dtem, fehler, 
				      fehlx, rk1[maxnsv+1], rk2[maxnsv+1],val ;
    goto L20 ;                     /* first derve1 done before entry!*/
    L10 : drun00(2) ; if (errnum) return ;
    L20 : time=relval[mmm+5] ;                                   /* save t */
    IDERIV= -1 ;                                      /* reset IDERIV*/


    /* reduce DT near communication points: if tnext-t <= DT
       then FO=TRUE, dtem= old DT, DT=tnext-t */

    fehlx=ztnext-relval[mmm+5] ;
    if (fehlx<=relval[mmm+3]) {
	FO=TRUE ;
	dtem=relval[mmm+3] ; relval[mmm+3]=fehlx+UROUND*relval[mmm+3] ;
    }
    else FO=FALSE ;
    /* --------------------------------------------------------------- */
    /* RK calculations */

    hdt=0.5*relval[mmm+3] ; dto6=relval[mmm+3]/6 ;     /* can change */
    nn1LOOP {
	rk1[i]=sderiv[i] ;
	relval[i]=dpsv[i]+rk1[i]*hdt ;
    }
    relval[mmm+5]+=hdt ;                             /* t = t + DT/2 */

    drun00(2) ; if (errnum) return ;
    nn1LOOP {
	rk2[i]=sderiv[i] ;
	relval[i]=dpsv[i]+rk2[i]*hdt ;
    }

    drun00(2) ; if (errnum) return ;
    nn1LOOP {
	rk3=sderiv[i] ;
	relval[i]=dpsv[i]+rk3*relval[mmm+3] ;
	rk1[i]+=2*rk3 ;                       /* saves storing rk3[i] */
    }
    relval[mmm+5]+=hdt ;                             /* t = t + DT/2 */

    drun00(2) ; if (errnum) return ;

/*  ------------------------------------------------------------------ */
    /* error calculations - ru4 */

    if (relval[mmm+2]==0) {              /* CHECKN=0, relative error */
	fehler=0 ;
	nn1LOOP {
            rk1[i]+=sderiv[i] ;                           /* later,too */
            if ((val=relval[i])<0) val= -val ;              /* was abs */
            if ((fehlx=(4*rk2[i]-rk1[i])/(val+1))<0) fehlx= -fehlx ;
            if (fehler<fehlx) fehler=fehlx ;
	}
    }
    else {                              /* CHECKN<>0, absolute error */
	nn1LOOP rk1[i]+=sderiv[i] ;
	i=relval[mmm+2] ; if (i<0) i= -i ;      /* CHECKN - truncated */
	if ((fehler=4*rk2[i]-rk1[i])<0) fehler= -fehler ;
    }

    fehler*=dto6 ;
    if (fehler<=relval[mmm+9]) goto L30 ;       /* <=ERMAX, no halve */

    /*  --------------------------------------------------------------- */
    /* try to halve DT and repeat the step */

    if (hdt>=relval[mmm+8]) {                  /* if DT/2>=DTMIN ... */
	relval[mmm+3]=hdt ;                               /* halve DT */
	relval[mmm+5]=time ;                             /* restore t */
	goto L10 ;                                     /* repeat step */
    }
    errnum=94 ; relval[mmm+6]=fehler ; return ;            /* deadlock */

    /* ---------------------------------------------------------------- */
    /*    in case of communic. point, restore DT; else try to double DT */

    L30 : if (FO) relval[mmm+3]=dtem ;                          /* DT=dtem */
    else if (fehler<relval[mmm+10] &&              /* fehler < ERMIN */
	     2*relval[mmm+3]<=relval[mmm+7])              /* 2*DT<=DTMAX */
	relval[mmm+3]*=2 ;

/* ------------------------------------------------------------------- */
    /* new solution point */
    nn1LOOP relval[i]=dpsv[i]+=(rk1[i]+2*rk2[i])*dto6;

    IDERIV=1 ;                                           /* set IDERIV */
    if (relval[mmm+5]-ztnext<0) goto L10 ;
}                                                           /* ru4 */



void ru5() {                           /*  RK with program-variable DT */
    register int  i ; double  reg ;

    goto L20 ;                    /* first derve1 done before entry! */
    L10 : drun00(2) ; if (errnum) return ;
    L20 : IDERIV= -1 ;                                     /* reset IDERIV */
    hdt=relval[mmm+3]/2 ;  /* local here, i.e. it can be programmed! */

    nn1LOOP {
	reg=sderiv[i]*hdt ;                              /* reg=rk1/3 */
	dpsv[i]+=reg ;                           /* save dpsv=x+rk1/2 */
	relval[i]=dpsv[i]+reg ;               /* x=x+rk1 for drun00(2) */
    }
    relval[mmm+5]+=relval[mmm+3] ;                     /* t = t + DT */

    drun00(2) ; if (errnum) return ;
    nn1LOOP relval[i]=dpsv[i]+=sderiv[i]*hdt ;

    IDERIV=1 ;                                           /* IDERIV>0 */
    if (relval[mmm+5]-ztnext<0) goto L10 ;
}                                                         /* ru5 */



void ru6() {                                  /*  rule not implemented */

    printf("*** use irule 2! ***") ; errnum=86 ; return ;
}                                                         /* ru6 */



void ru7() {                            /*  RKE 2/1 Variable-step Rule */

    register int  i ;  int  FO ;
    double  dtem, fehler, fehlx, hdt, time, rk1[maxnsv+1], val ;

    goto L20 ;                    /* first derve1 done before entry! */
    L10 : drun00(2) ; if (errnum) return ;
    L20 : time=relval[mmm+5] ;                                   /* save t */
    IDERIV= -1 ;                                     /* reset IDERIV */

    /* reduce DT near communication points: if tnext-t <= DT
       then FO=TRUE, dtem= old DT, DT=tnext-t */

    fehlx=ztnext-relval[mmm+5] ;
    if (fehlx<=relval[mmm+3]) {
	FO=TRUE ;
	dtem=relval[mmm+3] ; relval[mmm+3]=fehlx+UROUND*relval[mmm+3] ;
    }
    else FO=FALSE ;
/* ------------------------------------------------------------------- */
    /* RK calculations */

    hdt=0.5*relval[mmm+3] ;                             /* can change! */

    nn1LOOP {
	rk1[i]=sderiv[i] ;                /* save for error calculation */
	relval[i]=dpsv[i]+sderiv[i]*relval[mmm+3] ;
    }
    relval[mmm+5]+=relval[mmm+3] ;                       /* t = t + DT */

    drun00(2) ; if (errnum) return ;

/*  ------------------------------------------------------------------ */
    /* error calculations */

    if (relval[mmm+2]==0) {              /* CHECKN=0, relative error */
	fehler=0 ;
	nn1LOOP {
            if ((val=relval[i])<0) val= -val ;              /* was abs */
            if ((fehlx=(rk1[i]-sderiv[i])/(val+1))<0) fehlx= -fehlx ; 
            if (fehler<fehlx) fehler=fehlx ;
	}
    }
    else {                              /* CHECKN<>0, absolute error */
	nn1LOOP rk1[i]+=sderiv[i] ;
	i=relval[mmm+2] ; if (i<0) i= -i ;           /* trunc(CHECKN) */
	if ((fehler=rk1[i]-sderiv[i])<0) fehler= -fehler ;
    }
    fehler*=hdt ;
    if (fehler<=relval[mmm+9]) goto L30 ;       /* <=ERMAX, no halve */

    /*  --------------------------------------------------------------- */
    /* try to halve DT and repeat the step */

    if (hdt>=relval[mmm+8]) {                  /* if DT/2>=DTMIN ... */
	relval[mmm+3]=hdt ;                               /* halve DT */
	relval[mmm+5]=time ;                             /* restore t */
	goto L10 ;                                     /* repeat step */
    }
    errnum=94 ; relval[mmm+6]=fehler ; return ;            /* deadlock */

    /* ---------------------------------------------------------------- */
    /* in case of communic. point, restore DT; else try to double DT */

    L30 : if (FO) relval[mmm+3]=dtem ;                          /* DT=dtem */
    else if (fehler<relval[mmm+10] &&              /* fehler < ERMIN */
	     2*relval[mmm+3]<=relval[mmm+7])                /* 2*DT<=DTMAX */
	relval[mmm+3]+=2*relval[mmm+3] ;

/* ------------------------------------------------------------------- */
    /* new solution point */
    nn1LOOP relval[i]=dpsv[i]+=(rk1[i]+sderiv[i])*hdt ;

    IDERIV=1 ;                                         /* set IDERIV */
    if (relval[mmm+5]-ztnext<0) goto L10 ;
}                                                         /* ru7 */


void ru8() {                     /*  RKN  3rd-order Variable-step Rule */
    register int  i ;  int  FO ;
    double  hdt, rk3, fehler, fehlx, dtem, rk1[maxnsv+1], rk2[maxnsv+1],
	time, dto6, val ;

    goto L20 ;                    /* first derve1 done before entry! */
    L10 : drun00(2) ; if (errnum) return ;
    L20 : time=relval[mmm+5] ;                                   /* save t */
    IDERIV= -1 ;                                     /* reset IDERIV */

    /* reduce DT near communication points: if tnext-t <= DT
       then FO=TRUE, dtem= old DT, DT=tnext-t */

    fehlx=ztnext-relval[mmm+5] ;
    if (fehlx<=relval[mmm+3]) {
	FO=TRUE ;
	dtem=relval[mmm+3] ; relval[mmm+3]=fehlx+UROUND*relval[mmm+3] ;
    }
    else FO=FALSE ;

/* ------------------------------------------------------------------- */
    /* RK calculations */

    hdt=0.5*relval[mmm+3] ; dto6=relval[mmm+3]/6 ;      /* can change! */
    nn1LOOP {
	rk1[i]=sderiv[i] ;
	relval[i]=dpsv[i]+rk1[i]*hdt ;
    }
    relval[mmm+5]+=hdt ;                               /* t = t + DT/2 */

    drun00(2) ; if (errnum) return ;
    nn1LOOP {
	rk2[i]=2*sderiv[i] ;
	relval[i]=dpsv[i]+(rk2[i]-rk1[i])*relval[mmm+3] ;
    }
    relval[mmm+5]+=hdt ;                               /* t = t + DT/2 */

    drun00(2) ; if (errnum) return ;

/*  ------------------------------------------------------------------ */
    /* error calculations */

    if (relval[mmm+2]==0) {              /* CHECKN=0, relative error */
	fehler=0 ;
	nn1LOOP {
            rk1[i]+=sderiv[i] ;                           /* later,too */
            if ((val=relval[i])<0) val= -val ;               /* was abs */
            if ((fehlx=(4*rk2[i]-rk1[i])/(val+1))<0) fehlx= -fehlx ;
            if (fehler<fehlx) fehler=fehlx ;
	}
    }
    else {                              /* CHECKN<>0, absolute error */
	nn1LOOP rk1[i]+=sderiv[i] ;
	i=relval[mmm+2] ; if (i<0) i= -i ;       /* CHECKN - truncated */
	if ((fehler=rk1[i]-rk2[i])<0) fehler= -fehler ;
    }
    fehler*=2*dto6 ;
    if (fehler<=relval[mmm+9]) goto L30 ;       /* <=ERMAX, no halve */

    /*  --------------------------------------------------------------- */
    /* try to halve DT and repeat the step */

    if (hdt>=relval[mmm+8]) {                  /* if DT/2>=DTMIN ... */
	relval[mmm+3]=hdt ;                               /* halve DT */
	relval[mmm+5]=time ;                             /* restore t */
	goto L10 ;                                     /* repeat step */
    }
    errnum=94 ; relval[mmm+6]=fehler ; return ;            /* deadlock */

    /* ---------------------------------------------------------------- */
    /* in case of communic. point, restore DT; else try to double DT */

    L30 : if (FO) relval[mmm+3]=dtem ;                          /* DT=dtem */
    else if (fehler<relval[mmm+10] &&              /* fehler < ERMIN */
	     2*relval[mmm+3]<=relval[mmm+7])                /* 2*DT<=DTMAX */
	relval[mmm+3]*=2 ;

/* ------------------------------------------------------------------- */
    /* new solution point */
    nn1LOOP relval[i]=dpsv[i]+=(rk1[i]+2*rk2[i])*dto6;

    IDERIV=1 ;                                         /* set IDERIV */
    if (relval[mmm+5]-ztnext<0) goto L10 ;
}                                                         /* ru8 */

