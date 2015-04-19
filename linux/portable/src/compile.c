/* COMPILE.c      DYNAMIC-segment Routines      UNIX                   */
/*                  - emulates stack machine                           */
/*                                                                     */
/*             Registered Copyright 1990 G.A. Korn              8/8/94 */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/**             corrected sinc()                               5/20/97 */
/***           added drand48 for GNU                           6/13/97 */
/**            new CLEARN with L2 metric                       6/17/97 */
/**            add cAIK etc., added 40 to relval for UNIX       2/8/99 */
/**            much faster F(x routine                         4/17/99 */
/**            for new arraydata2() compilation                4/24/99 */
/**            added cfunc2()                                  4/25/99 */
/*             COMINT to relval[mmm+19] to accommodate MM      9/18/02 */
/*             speeded up F(x routine                          9/20/02 */
/*             faster lim(), SAT(), sat(), deadz(), tri()                  1/12/03 */

/**  zCOMINT, IDERIV, ICOM, INITE1, ztnext no longer in relval 9/20/02 */

/* ------------------------------------------------------------------- */

#include <setjmp.h>
#include <errno.h>
#include <stdlib.h>                           /* just in case, for GNU */
#include "declare.h"
#include "global.h"


jmp_buf  bjump, jjump ;                   /* type declared in setjmp.h */

int  cmark,dmark,NROW1,NCOL1,YBASE,COUNT ;  /* global for compiled loops */



void derve1() {     /* derivative routine, USED BY srun, integr. rules */

      di=ci=0 ;  gjj=maxrlvar+40 ;
      if (setjmp(bjump)) return ;
L10 : (*COMPILE[ci++])() ; goto L10 ;
      }

void jacob() {                     /* Jacobian routine, USED BY pederv */

      di=jdstart ; ci=jcstart ; gjj=maxrlvar+40 ;
      if (setjmp(jjump)) return ;
L10 : (*COMPILE[ci++])() ; goto L10 ;
      }

/* ------------------------------------------------------------------- */
/*               NOTE: stack and array overflow is checked by compiler */

void cpush0() { relval[--gjj]=0 ; }                          /* push 0 */
void cpushvar() { relval[--gjj]=relval[dptr[di++]] ; }         /* push */
void cpop() { relval[dptr[di++]]=relval[gjj++] ; }              /* pop */
void cpopd() { sderiv[dptr[di++]]=relval[gjj++] ; }            /* popd */

void cadd() { relval[gjj+1]+=relval[gjj++] ; }              /* add/pop */
void csub() { relval[gjj+1]-=relval[gjj++] ; }         /* subtract/pop */
void cmult() { relval[gjj+1]*=relval[gjj++]  ; }       /* multiply/pop */
void cdiv() { relval[gjj+1]/=relval[gjj++] ; }      /* divide  and pop */

void cpwr() {                                                   /* x^y */
  if (relval[gjj]==2) relval[gjj+1]=relval[gjj+1]*relval[gjj+1] ;
  else {
    relval[gjj+1]=exp(relval[gjj]*log(relval[gjj+1])) ;
    if (errno==EDOM) derror(9) ;
    }
  gjj++ ;
  }

void CADD() { relval[gjj]+=relval[dptr[di++]] ; }               /* add */
void CSUB() { relval[gjj]-=relval[dptr[di++]] ; }          /* subtract */
void CMULT() { relval[gjj]*=relval[dptr[di++]] ; }         /* multiply */
void CDIV() { relval[gjj]/=relval[dptr[di++]] ; }            /* divide */

void CPWR() {                                                   /* x^y */
  if (relval[dptr[di++]]==2) relval[gjj]=relval[gjj]*relval[gjj] ;
  else {
    relval[gjj]=exp(relval[dptr[di-1]]*log(relval[gjj])) ;
    if (errno==EDOM) derror(9) ;
    }
  }

void csin() { relval[gjj]=sin(relval[gjj]) ; }                  /* sin */
void ccos() { relval[gjj]=cos(relval[gjj]) ; }                  /* cos */
void ctan() { relval[gjj]=tan(relval[gjj]) ; }                  /* tan */
void cexp() { relval[gjj]=exp(relval[gjj]) ; }                  /* exp */
void ctanh() { relval[gjj]=tanh(relval[gjj]) ; }               /* tanh */
void csinh() { relval[gjj]=sinh(relval[gjj]) ; }               /* sinh */
void ccosh() { relval[gjj]=cosh(relval[gjj]) ; }               /* cosh */
void casin() { relval[gjj]=asin(relval[gjj]) ; }               /* asin */
void cacos() { relval[gjj]=acos(relval[gjj]) ; }               /* acos */
void catan() { relval[gjj]=atan(relval[gjj]) ; }               /* atan */

void csinc() {                                                 /* sinc */
        if (relval[gjj]==0) relval[gjj]=1 ;
        else relval[gjj]=sin(relval[gjj])/relval[gjj] ;
        }

void crecip() {relval[gjj]=1/relval[gjj] ; }                  /* recip */

/**
void cran1() {relval[gjj]=2*d_lcran_()-1.0 ; }                  ran1 for Sun C
void cran() {relval[--gjj]=2*d_lcran_()-1.0 ; }                  ran 
                                                          note push!  
                                                 
void cran1() { relval[gjj]=ldexp((double)rand(),-14)-1.0 ; }    ran1 for WATCOM 
void cran() {relval[--gjj]=ldexp((double)rand(),-14)-1.0 ; }    ran 
                                                         note push!  ********/

void cran1() { relval[gjj]=2*drand48()-1.0 ; }      /* ran1 for GNU */
void cran() { relval[--gjj]=2*drand48()-1.0 ; }     /* ran  ****/

                                                           

void csqrt() {                                                 /* sqrt */
   relval[gjj]=sqrt(relval[gjj]) ;
   if (errno==EDOM) derror(47) ;
   }
void cln() {                                                     /* ln */
   relval[gjj]=log(relval[gjj]) ;
   if (errno==EDOM) derror(9) ;
   }
void clog() {                                                   /* log */
   relval[gjj]=log10(relval[gjj]) ;
   if (errno==EDOM) derror(9) ;
   }

void cfabs() { relval[gjj]=fabs(relval[gjj]) ; }                /* abs */

void clim() { relval[gjj]=0.5*(relval[gjj]+fabs(relval[gjj])) ; }   /* lim */  /* <****/

void cswtch() {
   if (relval[gjj]>0) relval[gjj]=1 ; else relval[gjj]=0 ; }  /* swtch */

void ctri() { relval[gjj]=1.0-fabs(relval[gjj]) ; }   /* tri */ /** <****/
  
  /**  if (relval[gjj]<0) relval[gjj]+=1 ;                     
   else relval[gjj]=1-relval[gjj] ;    **/
   
void crect() {  							   /* rect */
   if (fabs(relval[gjj])<1) relval[gjj]=1 ;                 
   else relval[gjj]=0 ;
   }

void csgn() {                                                   /* sgn */
   if (relval[gjj]>0) relval[gjj]=1 ;
   else if (relval[gjj]<0) relval[gjj]=-1 ;
   else relval[gjj]=0 ;
   }

void csat() {                                                   /* sat */ /* <****/
   relval[gjj]=0.5*(fabs(relval[gjj]+1.0)-fabs(relval[gjj]-1.0)) ;
   }

  /****  if (relval[gjj]>1) relval[gjj]=1 ;
   else if (relval[gjj]<-1) relval[gjj]=-1 ;  ***/
   
void cSAT() {                                                   /* SAT */ /* <***/
     relval[gjj]=0.5*(1.0+fabs(relval[gjj])-fabs(relval[gjj]-1)) ;
      }

   /***  if (relval[gjj]>1) relval[gjj]=1 ;
   else if (relval[gjj]<0) relval[gjj]=0 ;  ***/
   
void cdeadz() {                                               /* deadz */ /*  <***/
       relval[gjj]=relval[gjj]-0.5*(fabs(relval[gjj]+1.0)-fabs(relval[gjj]-1.0)) ;
       }

   /*** if (relval[gjj]>1) relval[gjj]-=1 ;
   else if (relval[gjj]<-1) relval[gjj]+=1 ;
   else relval[gjj]=0 ;  ***/
 
void cdeadc() {                                               /* deadc */ 

    if (relval[gjj]>1) relval[gjj]=1 ;
    else if (relval[gjj]<-1) relval[gjj]=-1 ;
    else relval[gjj]=0 ;
    }
   
void csigmoid() { relval[gjj]=1/(exp(-relval[gjj])+1) ; }   /* sigmoid */

void csigsig() {                                            /* SIGMOID */
   if (relval[gjj]>0) {
      relval[gjj]=relval[gjj]*relval[gjj] ;
      relval[gjj]=relval[gjj]/(relval[gjj]+1) ;
      }
   else relval[gjj]=0 ;
   }

void catanx() {                                               /* atan2 */
      relval[gjj+1]=atan2(relval[gjj+1],relval[gjj]) ;
      gjj++ ;
   }

void ccomp() {                                   /* comp(ctl,min,plus) */
   if (relval[gjj+2]<=0) relval[gjj+2]=relval[gjj+1] ;
   else relval[gjj+2]=relval[gjj] ;
   gjj+=2 ;
   }

void cterm() { if (relval[gjj++]>0) errnum=303 ; }             /* term */
void cif() { if (relval[gjj++]<=0) longjmp(bjump,1) ; }          /* if */
void cret() { longjmp(bjump,1) ; }                /* derivative return */
void jret() { longjmp(jjump,1) ; }             /* Jacobian-code return */


void csample() {  /* SAMPLE nn ; dptr[di] has nn, dpt[di+1] is counter */

   if (ICOM>0) {                        /* ICOM>0, communication point */
      if (INITE1>0) dptr[di+1]=dptr[di] ;          /* initialize count */
      if (--dptr[di+1]==0) { dptr[di+1]=dptr[di] ; di+=2 ; return ; }
      }
   longjmp(bjump,1) ;
   }

void cstep() { if (IDERIV<=0) longjmp(bjump,1) ; }             /* step */

void cout() { if (ICOM<=0) longjmp(bjump,1) ; }                 /* OUT */
   

void cstore() {                                           /* store X=x */

/* ****>     dptr[di] has X dimension;
             +1 has X starting address (address of X1);
             +2 is counter, +3 is pointer; +4 has x-address      <**** */

   if (ICOM>0) {                        /* ICOM>0, communication point */
      if (INITE1>0) {                                    /* initialize */
         dptr[di+2]=dptr[di] ; dptr[di+3]=dptr[di+1] ;    /* ctr & ptr */
         }
      if (dptr[di+2]-- >0) relval[dptr[di+3]++]=relval[dptr[di+4]] ;
      }
   di+=5 ;
   }

void cget() {                                               /* get x=X */

   if (ICOM>0) {                        /* ICOM>0, communication point */
      if (INITE1>0) {                                    /* initialize */
         dptr[di+3]=dptr[di+1] ; dptr[di+4]=dptr[di+2] ;  /* ctr & ptr */
         }
      if (dptr[di+3]-- >0) relval[dptr[di]]=relval[dptr[di+4]++] ;
      }
   di+=5 ;
   }


void cfunc() {                               /* y = F(x ; changed 4/99 */

/* ****> on entry, dptr[di] has y-address;
                   +1 has F dimension/2 = N ; (NOT needed!)
                   +2 has F starting address (Xstart, address of x1);
                   +3 has Ystart (F start address + dimension/2);
                   +4 is not used                
                   +5 has x-address                             <**** */

   double x,y ;  int  I, K ;          /* compiler precomputes Ystart */
   
   x=relval[dptr[di+5]] ; 
   I=dptr[di+2] ;                                       /* I = Xstart */
    
   if (x>relval[I]) {                                  /* x > first X */       
      do { if (x<=relval[++I]) break ; }
                           while (I<dptr[di+3]) ;  /* I < Xstart + N */
      K=I+dptr[di+1] ;                           
      if (I<dptr[di+3])                               /* interpolate */

         y=relval[K]+(x-relval[I])
                        *(relval[K]-relval[K-1])/(relval[I]-relval[I-1]) ;
         
      else y=relval[K-1] ;
      }
   else y=relval[dptr[di+3]] ;

   relval[dptr[di]]=y ;
   di+=6 ;
   }

/* ------------------------------------------------------------------------ */

void cfuzz() {}                                          /** FUZZ lo,hi,i=x,X 
                                                                               
   on entry, dptr[di] has lo-address                            vardata(1)
                   +1 has hi-address                            vardata(1)
                   +2 has i
                   +3 has x-address                             vardata(0)  
                   +4 has dimension N of X, then Xend address   arraydata2(0)
                   +6 has X starting address (address of X[1])  
                   (di ends at +7)                                     <****

  integer I, Xend ; double x, Del ;
        
  x=relval[dptr[di+3]] ;  Xend=dptr[di+4] ;                                                             

  I=dptr[di+6] ;                                             * I = Xstart *  
  if (x>relval[I]) {                                        * x > first X *              
    do { if (x<=relval[++I]) break ; }
                           while (I<Xend) ;             * i < Xstart + N  *  
    if (I<Xend){
       Del=1/(relval[I]-relval[I-1]) ; 
       relval[dptr[di]]=(x-relval[I-1])*Del ;
       relval[dptr[di+1]]=(relval[I]-x)*Del ;
       intval[dptr[di+2]]=I-dptr[di+6] ;                  **** <***** ????? *****
       }
    else {                                                 * x >= last X *
       relval[dptr[di]]=1 ; relval[dptr[di+1]]=0 ;
       intval[dptr[di+2]]=Xend-dptr[di+6]  ;        **** <***** ????? *****
       }                   
    }      
  else {                                                  * x <= first X *
    relval[dptr[di]]=0 ; relval[dptr[di+1]]=1 ;
    intval[dptr[di+2]]=1 ;
    }                                                                                         
  }                                                      **************/


void cfunc2() {                                    /* y = Y(x1,x2;X1,X2  */
                        
/* ****> on entry, dptr[di] has y-address;                      vardata(1)
                   +1 has dimension N1 of X1                    arraydata2(1)
                   +2 has dimension N2 of X2
                   +3 has Ystart (address of Y[1,1])                                   
                   +4 has x1-address                            vardata(0)
                   +5 has x2-address                            vardata(0)
                   +6 has N1, then X1start+N = X1end            arraydata2(0)
                   +7 has X1start (address of X1[1])
                   +8 has N2, then X2start+N2 = X2end           arraydata2(0)
                   +9 has X2start (address of X2[1])
                   (di ends at +10)                                  <**** */
                                                                    
  int  I1, I2, K1, K2, Ystart, X1end, X2end, N2 ;
  double x1, x2, HiMEMBER1, LoMEMBER1, HiMEMBER2, LoMEMBER2, Del1, Del2 ;

  Ystart=dptr[di+3] ; X1end=dptr[di+6] ; X2end =dptr[di+8] ; N2=dptr[di+2];
  x1=relval[dptr[di+4]] ; x2=relval[dptr[di+5]] ;

  I1=dptr[di+7] ;                                        /* I1 = X1start */  
  if (x1>relval[I1]) {                                  /* x1 > first X1 */              
    do { if (x1<=relval[++I1]) break ; }
                           while (I1<X1end) ;      /* I1 < X1start + N1  */  
    if (I1<X1end){
       Del1=1/(relval[I1]-relval[I1-1]) ; 
       HiMEMBER1=(x1-relval[I1-1])*Del1 ; LoMEMBER1=(relval[I1]-x1)*Del1 ;      
       }
    else {HiMEMBER1=0 ; LoMEMBER1=1 ; }                 /* x1 >= last X1 */
    }      
  else {HiMEMBER1=1 ; LoMEMBER1=0 ; }                  /* x1 <= first X1 */

  I2=dptr[di+9] ;                                        /* I2 = X2start */  
  if (x2>relval[I2]) {                                  /* x2 > first X2 */              
    do { if (x2<=relval[++I2]) break ; } 
                           while (I2<X2end) ;       /* I2 < X2start + N2 */  
    if (I2<X2end){
       Del2=1/(relval[I2]-relval[I2-1]) ;
       HiMEMBER2=(x2-relval[I2-1])*Del2 ; LoMEMBER2=(relval[I2]-x2)*Del2 ;      
       }
    else { HiMEMBER2=0 ; LoMEMBER2=1 ; }                /* x2 >= last X2 */
    } 
  else {HiMEMBER2=1 ; LoMEMBER2=0 ; }                  /* x2 <= first X2 */

  K1=I1-dptr[di+7] ; K2=I2-dptr[di+9] ;
  
 /*** relval[dptr[di]]=  LoMEMBER1*LoMEMBER2*relval[Ystart +(K1-1)*N2 + (K2-1)]
                       + HiMEMBER1*LoMEMBER2*relval[Ystart +K1*N2     + (K2-1)]
                       + LoMEMBER1*HiMEMBER2*relval[Ystart +K1-1)*N2  + K2]
                       + HiMEMBER1*HiMEMBER2*relval[Ystart +K1*N2     + K2] ; ***/
 
 relval[dptr[di]]= LoMEMBER1*(LoMEMBER2*relval[Ystart+(K1-1)*N2 + (K2-1)]
                            + HiMEMBER2*relval[Ystart+(K1-1)*N2 + K2])
 
                 + HiMEMBER1*(LoMEMBER2*relval[Ystart+K1*N2     + (K2-1)]                
                            + HiMEMBER2*relval[Ystart+K1*N2     + K2]) ;                                                     
 di+=10 ;
 }
   

void cdelay() {                                     /* delay y=X,x,tau */

/* ****>     dptr[di] has y-address; +1 has X dimension>=1000;
             +2 has X starting address (address of X1);
             +3 is counter, +4 is pointer; +5 has x-address;
             +6 has tau-address                                  <**** */

   int  itau, I ;

   if (ICOM>0) {                        /* ICOM>0, communication point */
      if (INITE1>0) {                                    /* initialize */
         dptr[di+3]=1000 ; dptr[di+4]=dptr[di+2] ;        /* ctr & ptr */
         }
         if (--dptr[di+3]<0) { dptr[di+3]=999 ; dptr[di+4]=dptr[di+2] ; }

      relval[dptr[di+4]]=relval[dptr[di+5]] ;               /* write x */

      itau=relval[dptr[di+6]]/zCOMINT ;  /* compute delay index <****/
      if (itau>999 || itau<0) derror(105) ;

      if ((I=(dptr[di+4]++)-itau)<dptr[di+2]) I+=1000 ;
      relval[dptr[di]]=relval[I] ;                           /* read y */
      }   di+=7 ;

   }

void  ctdela() {                                   /* tdelay y=X,x,tau */

/* ****>     dptr[di] has y-address; +1 has X dimension>=1000;
             +2 has X starting address (address of X1);
             +3 is counter, +4 is pointer; +5 has x-address;
             +6 has tau-address                                  <**** */

   int  itau, I ;

   if (IDERIV>0) {                  /* IDERIV>0, state variables valid */
      if (INITE1>0) {                                    /* initialize */
         dptr[di+3]=1000 ; dptr[di+4]=dptr[di+2] ;        /* ctr & ptr */
         }
      if (--dptr[di+3]<0) { dptr[di+3]=999 ; dptr[di+4]=dptr[di+2] ; }

      relval[dptr[di+4]]=relval[dptr[di+5]] ;               /* write x */

      if (relval[mmm+3]!=relval[mmm-3]) derror(122) ;   /* DT changed! */
      itau=relval[dptr[di+6]]/relval[mmm+3] ;   /* compute delay index */
      if (itau>999 || itau<0) derror(105) ;

      if ((I=(dptr[di+4]++)-itau)<dptr[di+2]) I+=1000 ;
      relval[dptr[di]]=relval[I] ;                           /* read y */
      }
   di+=7 ;
   }


void ctrkhld() {                                    /* trkhld y,y0=p,x */

/* ****>     dptr[di] has y-address; +1 has y0 address;
             +2 has p address); +3 has x-address;                <**** */

   if (relval[dptr[di+2]]>0) {                                /* track */
      relval[dptr[di]]=relval[dptr[di+1]] ;                    /* y=y0 */
      relval[dptr[di+1]]=relval[dptr[di+3]] ;                  /* y0=x */
      }
   di+=4 ;
   }


void ccount1() {}


void cround() {                                               /* round */

   int  rj ;                                           /****************/

   if (relval[gjj]>=maxint || relval[gjj]<=-maxint) derror(20) ;
   if (relval[gjj]>0) relval[gjj]=rj=relval[gjj]+0.5 ;
   else if (relval[gjj]<0)  relval[gjj]=rj=relval[gjj]-0.5 ;
   else relval[gjj]=0 ;
   }

/* ------------------------------------------------------------------- */

void cyinit() {                          /* start and mark VECTOR loop */

   NROW1=dptr[di++] ;  YBASE=dptr[di++] ; COUNT=NROW1-1 ;
   cmark=ci ; dmark=di ;                       /* keep for loop return */
   }


void cainit() {                          /* start and mark MATRIX loop */

   NROW1=dptr[di++] ;  NCOL1=dptr[di++] ; COUNT=NROW1*NCOL1-1 ;
   YBASE=dptr[di++] ;
   cmark=ci ; dmark=di ;                       /* keep for loop return */
   }


void cpusharr() { relval[--gjj]=relval[dptr[di++]+COUNT] ; }  /* array */


void caik() {                                              /* push A*x */

   register int  count, Aindex, xindex ; 

   count=dptr[di++] ;                                          /* ncol */
   Aindex=dptr[di++]+(COUNT+1)*count ;        /* ABASE+COUNT*ncol+ncol */
   xindex=dptr[di++]+count ;                             /* XBASE+ncol */

   relval[--gjj]=0 ;                                     /*** changed ***/
   while (--count>=0) relval[gjj]+=relval[--Aindex]*relval[--xindex] ;
 
   }


void cAIK() {                                              /* push A*x */

   register int  count, ROWEND, XEND ;

   count=dptr[di++] ;                                        /* ncol-1 */
   ROWEND=dptr[di++] ;                              /* ABASE+ii*ncol-1 */
   XEND=dptr[di++] ;                                   /* XBASE+ncol-1 */

   relval[--gjj]=0 ;
   while (count-- >=0) relval[gjj]+=relval[ROWEND--]*relval[XEND--] ;
   }



void caki() {                                             /* push A%*x */

   register int  count, Aindex, xindex ;

   count=dptr[di++] ;                      /* nrow of transpose = NCOL */
   Aindex=dptr[di++]+NROW1*count+COUNT ;      /* ABASE+NROW*NCOL+COUNT */
   xindex=dptr[di++]+count ;                             /* XBASE+NCOL */

   relval[--gjj]=0 ;
   while (--count>=0) {
       Aindex-=NROW1 ; relval[gjj]+=relval[Aindex]*relval[--xindex] ; 
      }
   }


void cAKI() {                                             /* push A%*x */

   register int  count, VROW, COLEND, XEND ;

   count=dptr[di++] ;                  /* nrow of transpose-1 = XROW-1 */
   VROW=dptr[di++] ;                                   /* VROW = XBASE */
   COLEND=dptr[di++] ;                    /* ABASE+XBASE*(XROW-1)+ii-1 */
   XEND=dptr[di++] ;                                   /* XBASE+ncol-1 */

   relval[--gjj]=0 ;
   while (count-- >=0) { 
       relval[gjj]+=relval[COLEND]*relval[XEND--] ; COLEND-=VROW ;
      }
   }


void cpatrn() {                             /* push pattern-matrix row */

     int  i,irow ;

     if ((irow=relval[mmm+12]-1)<0) derror(124) ;  /* index 0 is row 1 */
     i= (int)irow%dptr[di++] ;               /* irow%(no. of patterns) */

     relval[--gjj]=relval[dptr[di++]+i*NROW1+COUNT] ;
     }



void cPATRN() {                            /* push pattern-matrix row */

     int VROW, irow, i ;

     if ((irow=relval[mmm+12]-1)<0) derror(124) ;  /* index 0 is row 1 */    

     i= (int)irow%dptr[di++] ;               /* irow%(no. of patterns) */
     VROW=dptr[di++] ;                             /* vector dimension */    
     relval[--gjj]=relval[dptr[di++]+i*VROW] ;  /* dptr has ABASE+ii-1 */ 
     }


void cmaskq() {                                       /* mask an array */

   if (relval[dptr[di++]+COUNT]==0) {
      COUNT-- ; ci=cmark ; di=dmark ;       /* conditional loop return */
      }
   }


void cpushv2() {                      /* push and multiply vector pair */

   int  xbase ;

   xbase=dptr[di++] ;
   relval[--gjj]=relval[xbase+COUNT/NCOL1]*relval[dptr[di++]+COUNT%NCOL1] ;
   }

void cmin() {                                              /* min(x,y) */

   int  xbase ;
  
   xbase=dptr[di++] ;
   if (relval[xbase+COUNT/NCOL1]<relval[dptr[di]+COUNT%NCOL1])
                  { di++ ; relval[--gjj]=relval[xbase+COUNT/NCOL1] ; }
   else relval[--gjj]=relval[dptr[di++]+COUNT%NCOL1] ;
   }


void cyend() {                                /* finish sum, test loop */

   relval[YBASE+COUNT]=relval[gjj++] ;          /* a y-element is done */
   if (--COUNT<0) return ;                                     /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
   }


void cyendli() {                  /* finish sum, set limits, test loop */

    if (relval[gjj]<relval[gjj+2]) relval[YBASE+COUNT]=relval[gjj] ;
    else if (relval[gjj+1]>relval[gjj+2]) relval[YBASE+COUNT]=relval[gjj+1] ;
    else relval[YBASE+COUNT]=relval[gjj+2] ;
    gjj+=3 ;                                       /* clean data stack */
    if (--COUNT<0) return ;                                    /* quit */
    ci=cmark ; di=dmark ;
    }


void cupdat() {                        /* finish update sum, test loop */

   relval[YBASE+COUNT]+=relval[gjj++] ;       /* a y-increment is done */
   if (--COUNT<0) return ;                                     /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
   }


void cupdatli() {          /* finish update sum, set limits, test loop */

   register int  i ;

   i=YBASE+COUNT ;
   relval[i]+=relval[gjj+2] ;                 /* a y-increment is done */
   if (relval[i]>relval[gjj]) relval[i]=relval[gjj] ;
   else if (relval[i]<relval[gjj+1]) relval[i]=relval[gjj+1] ;
   gjj+=3 ;                                             /* clean stack */
   if (--COUNT<0) return ;                                     /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
   }


void cdend() {                                /* finish sum, test loop */

   sderiv[YBASE+COUNT]=relval[gjj++] ;           /* derivative is done */
   if (--COUNT<0) return ;                                     /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
   }


void cdendli() {       /* finish derivative sum, set limits, test loop */

   register int  i ;

   i=YBASE+COUNT ;
   sderiv[i]=relval[gjj+2] ;                     /* derivative is done */
   if (IDERIV>0) {               /* test only once per DT step */
      if (relval[i]>relval[gjj]) relval[i]=relval[gjj] ;
      else if (relval[i]<relval[gjj+1]) relval[i]=relval[gjj+1] ;
      }
   gjj+=3 ;                                        /* clean data stack */
   if (--COUNT<0) return ;                                     /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
   }

void cdotend() {                          /* finish DOT sum, test loop */

   relval[gjj+1]+=relval[gjj++]*relval[YBASE+COUNT] ;
   if (--COUNT<0) return ;                                     /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
   }

void csumend() {                      /* finish DOT a = x*1, test loop */

   relval[gjj]+=relval[YBASE+COUNT] ;
   if (--COUNT<0) return ;                                     /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
   }


void cmax() {                                        /* winner-take-all */

   register int  I, Imax ;  double  MAX ;

   Imax=I=YBASE+NROW1-1 ; MAX=relval[I] ; relval[I]=0 ;

   while (--I>=YBASE) {
      if (MAX<relval[I]) { MAX=relval[I] ; Imax=I ; }
      relval[I]=0 ;
      }
   relval[Imax]=MAX ;
   }


void cMAX() {                                        /* winner-take-all */

   register int  I, Imax, VBASE ;  double  MAX ;
   
   VBASE=dptr[di++] ;
   Imax=I=VBASE+dptr[di++]-1 ; MAX=relval[I] ; relval[I]=0 ;

   while (--I>=VBASE) {
      if (MAX<relval[I]) { MAX=relval[I] ; Imax=I ; }
      relval[I]=0 ;
      }
   relval[Imax]=MAX ;
   }
/* ---------------------------------------------------------------------

************>           CLEARN y=W(x)lrate,crit               <**********

                simple competitive learning for crit<0
                pseudo-ART for crit>0
                FSCL (modified "conscience") for crit=0
                Oja's algorithm for lrate<0, crit=0

     Template matrix W has NROW1 rows, XCOL1 columns.  XBASE, YBASE,
     WBASE are indices of x[1], y[1], W[1,1] in the relval[] data array.

     DESIRE must declare an auxiliary array, say h[n]immediately after
     y[n] (as in ARRAY ...,y[n], h[n], ...) for FSCL and pseudo-ART.
     h[n] is initialized to 0.

     NROW1 and YBASE are global variables whose values are compiled
     before cclearn() is called.  XCOL1, WBASE, XBASE, and the relval[]
     indices of the learning rate lrate, the vigilance flag/parameter
     crit, and the fast-learn flag fast are compiled into the data-
     index array dptr[].

     ----------------------------------------------------------------- */

   void cclearn() { 

     register int  i, k ; 
     int rowbase, imin, WBASE, XCOL1, XBASE, CRIT, fast, rowbase2, imin2 ;
     double  lrate, crit, sum, MIN ;
     

     XCOL1=dptr[di++] ; WBASE=dptr[di++] ; XBASE=dptr[di++] ;
     lrate=relval[dptr[di++]] ; crit=relval[dptr[di++]] ;
     fast=dptr[di++] ;                                        /* fast-learn flag */

     if (crit<0) CRIT=-1 ; else if (crit==0) CRIT=0 ; else CRIT=1 ;

                     /* find row i for best template match; start with i=NROW1-1 */

L1 : MIN=1.0E+237 ;                  /* make the initial value of MIN very large */
     i=NROW1 ; imin=imin2=i-1 ;
     rowbase=WBASE+XCOL1*i ;                                    /* WBASE+XCOL1*i */

L3 : if (--i>=0) {                        /* compare matches for successive rows */
        rowbase-=XCOL1 ;                                    /* next template row */

/* ----------------------------------------------------------------------------- */
        if (CRIT>0) {                                     /* for pseudo-ART only */
           if (relval[YBASE+i]<0) goto L3 ;                      /* skip the row */
           }
/* ----------------------------------------------------------------------------- */

        relval[YBASE+i]=0 ;                                 /* initialize y[i+1] */
        sum=0 ; k=XCOL1 ;                                 /* compute match error */
        while (--k>=0) sum+=(relval[rowbase+k]-relval[XBASE+k])*
              (relval[rowbase+k]-relval[XBASE+k]) ;                   /* L2 norm */

/*************** or = fabs(relval[rowbase+k]-relval[XBASE+k]) ;    L1 norm *******/

/* ----------------------------------------------------------------------------- */
        if (CRIT==0) sum*=relval[YBASE+NROW1+i] ;               /* for FSCL only */
/* ----------------------------------------------------------------------------- */

        if (MIN>sum) { MIN=sum ; imin2=imin ; imin=i ; }     /* imin2 is for Oja */
        goto L3 ;
        } 

     rowbase=WBASE+XCOL1*imin ;                         /* selected template row */
     
/* ----------------------------------------------------------------------------- */
     if (CRIT>0) {                                         /* crit>0, pseudo-ART */
     
        if (MIN>crit) {                                             /* bad match */

           if (MIN>1.0E+236) derror(232) ;           /* we used up all templates */

           if (relval[YBASE+NROW1+imin]) {              /* is this row comitted? */
              relval[YBASE+imin]=-1 ;          /*yes, mark this row as bad match */
              goto L1 ;                            /* and repeat the competition */
              }                
           }          /* otherwise update in spite of bad match but don't commit */

        else relval[YBASE+NROW1+imin]=1 ;         /* good match, commit this row */ 
        }
/* ----------------------------------------------------------------------------- */      
                    
     k=XCOL1 ;                                             /* reset k, do update */
     
     if (fast>0) {                                                 /* fast learn */
        while (--k>=0) relval[rowbase+k]=relval[XBASE+k] ;
        goto L4 ;
        }
        
     if (lrate>0)  while (--k>=0)
          relval[rowbase+k]+=lrate*(relval[XBASE+k]-relval[rowbase+k]) ;
/* ----------------------------------------------------------------------------- */
     else if (lrate<0) {                       /** lrate<0, use Oja's algorithm **/
        if (CRIT!=0) while (--k>=0)
          relval[rowbase+k]-=lrate*(relval[XBASE+k]-relval[rowbase+k]) ;
        else {
           rowbase2=WBASE+XCOL1*imin2 ;
           while (--k>=0) {
              relval[rowbase+k]-=lrate*(relval[XBASE+k]-relval[rowbase+k]) ;
              relval[rowbase2+k]+=0.05*lrate*(relval[XBASE+k]-relval[rowbase2+k]) ;
              }
           }
        }
/* ------------------------------------------------------------------------------ */
       /* clear the y-array (but not the "committed" array) and mark the imin row */

L4 : i=NROW1 ; while (--i>=0) relval[YBASE+i]=0 ;
     relval[YBASE+imin]=1 ;
     }


void cbarf() {                                      /* INTP operation */

     register int  i, k, rowbase, WBASE, XCOL1, XBASE ;
     double  lrate, thresh, sum ;

     XCOL1=dptr[di++] ; WBASE=dptr[di++] ; XBASE=dptr[di++] ;
     thresh=relval[dptr[di++]] ;

     i=NROW1 ; rowbase=WBASE+XCOL1*i ;                /* WBASE+XCOL1*i */

     while (--i>=0) {           /* compare matches for successive rows */
        rowbase-=XCOL1 ;                          /* next template row */
        sum=0 ; k=XCOL1 ;                       /* compute match error */
        while (--k>=0) sum+=(relval[rowbase+k]-relval[XBASE+k])*
                                  (relval[rowbase+k]-relval[XBASE+k]) ;
        if (sum>thresh) relval[YBASE+i]=0 ;
        else relval[YBASE+i]=1 ;               /* select this template */
        }
     }


void cconv() {                                      /* shift operation */

   register int XBASE, indexx ;

   XBASE=dptr[di++] ;
   indexx=dptr[di++]+COUNT ;
   if (indexx<NROW1 && indexx>=0) relval[--gjj]=relval[XBASE+indexx] ;
   else relval[--gjj]=0 ;
   }


void cplearn() {                                /* PLEARN y=W*x;lrate */

/* ---------------------------------------------------------------------
               - learns principal-components transformation

     The weight matrix W has NROW1 rows, XCOL1 columns.  XBASE, YBASE,
     WBASE are indices of x[1], y[1], W[1,1] in the relval[] data array.

     NROW1 and YBASE (dimension and base of the vector y) are global
     variables whose values are determined by cyinit() before cplearn() is
     called.  The dimension XCOL1 of the vector x, the bases WBASE, XBASE,
     and the relval[] index of the learning rate lrate are compiled into
     the data-index array dptr[].

     ----------------------------------------------------------------- */

        register int  i, j, k, XCOL1, WBASE, XBASE, xindex, yindex, Windex ;
        double  lrate, sum ;

        XCOL1=dptr[di++] ; WBASE=dptr[di++] ; XBASE=dptr[di++] ;
        lrate=relval[dptr[di++]] ;

        Windex=WBASE ; yindex=YBASE ;
        for (i=0 ; i<NROW1 ; i++) {
           xindex=XBASE ; sum=0 ;
           for (k=0 ; k<XCOL1 ; k++)
           sum+=relval[Windex++]*relval[xindex++] ;
           relval[yindex++]=sum ;
           }                                                 /* y=W*x */

        Windex=WBASE ;
        for (i=0 ; i<NROW1 ; i++) {
           xindex=XBASE ;
           for (k=0 ; k<XCOL1 ; k++) {
              sum=0 ; for (j=0 ; j<=i ; j++) {             /* note <= */
                 sum+=relval[WBASE+j*XCOL1+k]*relval[YBASE+j] ;
                 }                                     /* xx=LT(W%*y) */

              relval[Windex++]+=lrate*relval[YBASE+i]*(relval[xindex++]-sum) ;
              }                                         /* end k-loop */
           }                                            /* end i-loop */
        }

