/* GEARER.C    Variable-order/variable-step Integration - UNIX              */
/*             Registered Copyright 1991  G.A. Korn                 1/13/92 */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/*** changed COMINT to zCOMINT from relval[mmm+15] to allow for MM, 9/20/02 */
                                                                    
/*  Complete comments are in the original PASCAL files for DESCTOP and      */
/*  DESIRE/387.  C arrays have dimensions increased by one, so that         */
/*  PASCAL code could be converted with only minor changes.                 */
/* ------------------------------------------------------------------------ */

#include "declare.h"
#include "global.h"

extern void derror() ;  extern void derve1() ;  extern void jacob() ;


#define UROUND 7.1E-15             /* THE UNITY ROUND-OFF FOR CDC 180/830   */

#define N nn1
#define XT0 relval[mmm+5]                                              /* t */
#define H0 relval[mmm+3]                                              /* DT */
#define EPS relval[mmm+9]                                          /* ERMAX */

#define Y0 relval
#define SAVE1 sderiv
#define SAVE2 dpsv

#define NLOOP for (I=1 ; I<=N ; I++)

int aINDEX ;  double TOUT ;

static int     REPDRIVE,LINESTORAGE,
               REPINTERP,I,MF,                                 /* N now nn1 */
               NSTEP,NFE,NJE,NQUSED,

               IDOUB,IREDO,METH,MITER,IWEVAL,LMAX,
               IRET,L,NOLD,NSTEPJ,MAXDER,M,NEWQ,IER,

               N0,NQ,JSTART,NSQ,KFLAG,MF0,

               IPIV[EQUMAX+1] ;


static double  ER,HUSED,TOUTP,             /* TOUT, XT0, H0, EPS  elsewhere */

               T,H,HMIN,HMAX,EPS0,EPSJ,
               RMAX,HOLD,TOLD,RH,RC,CRATE,EPSOLD,OLDLO,E,
               EDN,EUP,BND,R,HLO,RO,CON,PHLO,D,D1,
               PR1,PR2,PR3,ENQ1,ENQ2,ENQ3,

        EL[14], TQ[5], Y[EQUMAX+1][14],
        PPW[EQUMAX+1][EQUMAX+1],
        YMAX[EQUMAX+1], XERROR[EQUMAX+1],     /* Y0[EQUMAX+1,Y1[EQUMAX+1],] */
        PW[EQUMAX*EQUMAX+1] ;           /* SAVE1[EQUMAX+1], SAVE2[EQUMAX+1] */

/* ------------------------------------------------------------------------ */

 void INTERP() {                                           /*** unchanged ***/

   int  L,I,J ;  double  S,S1 ;

   for (I=1 ; I<=N0 ; I++) Y0[I]=Y[I][1] ;
   L=JSTART+1;
   S=(TOUT-T)/H;
   S1=1.0;
   for (J=2 ; J<=L ; J++) {
      S1*=S; for (I=1 ; I<=N0 ; I++) Y0[I]+=S1*Y[I][J] ;
      }
   }                                                            /* INTERPOL */

/* ------------------------------------------------------------------------ */

void COSET() {

   int  K ;  double  C[13][3][4] ;                              /* changed */

               C[1][1][1]=1.0; C[2][1][1]=1.0; C[3][1][1]=2.0; C[4][1][1]=1.0;
               C[5][1][1]=0.3158; C[6][1][1]=0.07407; C[7][1][1]=0.01391;
               C[8][1][1]=0.002182; C[9][1][1]=0.0002945;
               C[10][1][1]=0.00003492; C[11][1][1]=0.000003692;
               C[12][1][1]=0.0000003524;

               C[1][2][1]=1.0; C[2][2][1]=1.0; C[3][2][1]=0.5; C[4][2][1]=0.1667;
               C[5][2][1]=0.04167;

               C[1][1][2]=2.0; C[2][1][2]=12.0; C[3][1][2]=24.0; C[4][1][2]=37.89;
               C[5][1][2]=53.33; C[6][1][2]=70.08; C[7][1][2]=87.97;
               C[8][1][2]=106.9; C[9][1][2]=126.7; C[10][1][2]=147.4;
               C[11][1][2]=168.8; C[12][1][2]=191.0;

               C[1][2][2]=2.0; C[2][2][2]=4.5; C[3][2][2]=7.333;
               C[4][2][2]=10.42; C[5][2][2]=13.7;

               C[1][1][3]=12.0; C[2][1][3]=24.0; C[3][1][3]=37.89;
               C[4][1][3]=53.33; C[5][1][3]=70.08; C[6][1][3]=87.97;
               C[7][1][3]=106.9; C[8][1][3]=126.7; C[9][1][3]=147.4;
               C[10][1][3]=168.8; C[11][1][3]=191.0; C[12][1][3]=1.0;

               C[1][2][3]=3.0; C[2][2][3]=6.0; C[3][2][3]=9.167;
               C[4][2][3]=12.5; C[5][2][3]=1.0;


               if (METH==1) {
                  MAXDER=12;

/* ++++++++> */   switch(NQ) {

                  case 1:  EL[1]=1.0; break ;
                  case 2:  EL[3]=EL[1]=0.5; break ;

                  case 3:
                           EL[1]=4.1666666666667E-01;
                           EL[3]=0.75;
                           EL[4]=1.666666666667E-01;
                           break ;
                  case 4:
                           EL[1]=0.875;
                           EL[3]=9.1666666666667E-01;
                           EL[4]=3.3333333333333E-01;
                           EL[5]=4.1666666666667E-02;
                           break ;
                  case 5:
                           EL[1]=3.4611111111111E-01;
                           EL[3]=1.0416666666667;
                           EL[4]=4.8611111111111E-01;
                           EL[5]=1.1416666666667E-01;
                           EL[6]=8.3333333333333E-03;
                           break ;
                  case 6:
                           EL[1]=3.2986111111111E-01;
                           EL[3]=1.1416666666667;
                           EL[4]=0.625;
                           EL[5]=1.7708333333333E-01;
                           EL[6]=0.025;
                           EL[7]=1.3888888888889E-03;
                           break ;
                  case 7:
                           EL[1]=3.1559193121693E-01;
                           EL[3]=1.225;
                           EL[4]=7.5185185185185E-01;
                           EL[5]=2.5520833333333E-01;
                           EL[6]=4.8611111111111E-02;
                           EL[7]=4.8611111111111E-03;
                           EL[8]=1.9841269841270E-04;
                           break ;
                  case 8:
                           EL[1]=3.422453703704E-01;
                           EL[3]=1.2964285714286;
                           EL[4]=8.6851851851852E-01;
                           EL[5]=3.3576388888889E-01;
                           EL[6]=7.7777777777778E-02;
                           EL[7]=1.0648148148148E-02;
                           EL[8]=7.9365079365079E-04;
                           EL[9]=2.4801587301587E-05;
                           break ;
                  case 9:
                           EL[1]=2.9486800044092E-01;
                           EL[3]=1.3589285714286;
                           EL[4]=9.7655423280423E-01;
                           EL[5]=0.4171875;
                           EL[6]=1.1135416666667E-01;
                           EL[7]=0.01875;
                           EL[8]=1.9345238095238E-03;
                           EL[9]=1.1160714285714E-04;
                           EL[10]=2.7557319223986E-06;
                           break ;
                 case 10:
                           EL[1]=2.8697544642857E-01;
                           EL[3]=1.4144841269841;
                           EL[4]=1.0772156084656;
                           EL[5]=4.9856701940035E-01;
                           EL[6]=0.1484376;
                           EL[7]=2.9060670987654E-02;
                           EL[8]=3.7202380952381E-03;
                           EL[9]=2.9966584656685E-04;
                           EL[10]=1.3778659611993E-05;
                           EL[11]=2.7557319223986E-07;
                           break ;
                 case 11:
                           EL[1]=2.8018959644394E-01;
                           EL[3]=1.4644841269841;
                           EL[4]=1.1715146502646;
                           EL[5]=5.7935819003527E-01;
                           EL[6]=1.8832286155203E-01;
                           EL[7]=4.1430362654321E-02;
                           EL[8]=6.2111441798942E-03;
                           EL[9]=6.2526667989418E-04;
                           EL[10]=4.0417401528513E-05;
                           EL[11]=1.5156525573192E-06;
                           EL[12]=2.5052108385442E-08;
                           break ;
                 case 12:
                           EL[1]=2.7426554003160E-01;
                           EL[3]=1.5099386734387;
                           EL[4]=1.2622711640212;
                           EL[5]=6.5923418209877E-01;
                           EL[6]=2.3045800264550E-01;
                           EL[7]=5.5697246105232E-02;
                           EL[8]=9.4394841269841E-03;
                           EL[9]=1.1192749669312E-03;
                           EL[10]=9.0939153439153E-05;
                           EL[11]=4.8225308641975E-06;
                           EL[12]=1.5031265031265E-07;
                           EL[13]=2.0876756987868E-09;
                           break ;
/* ++++++++++> */      }                                     /* switch */

                     }                                           /* if */
                  else {
                     MAXDER=5;

/* ++++++++++> */    switch(NQ) {

                 case 1:      EL[1]=1.0; break ;
                 case 2:
                              EL[1]=6.6666666666667E-01;
                              EL[3]=3.3333333333333E-01;
                              break ;
                 case 3:
                              EL[1]=5.4545454545455E-01;
                              EL[3]=5.4545454545455E-01;
                              EL[4]=9.0909090909091E-02;
                              break ;
                 case 4:
                              EL[1]=0.48;
                              EL[3]=0.7;
                              EL[4]=0.2;
                              EL[5]=0.02;
                              break ;
                 case 5:
                              EL[1]=4.3795620437956E-01;
                              EL[3]=8.2116788321168E-01;
                              EL[4]=3.1021897810219E-01;
                              EL[5]=5.4744525547445E-02;
                              EL[6]=3.6496350364964E-03;
                              break ;

/* ++++++++++> */      }                                          /* switch */

                     }                                              /* else */
                  for (K=1 ; K<=3 ; K++) TQ[K]=C[NQ][METH][K];
                  TQ[4]=TQ[2]/(2*NQ+4) ;                         /* changed */
                  }                                            /* END COSET */

/* ------------------------------------------------------------------------ */

void DEC() {        /* (VAR A:MATRIXTYPE;VAR IP:VECTORTYPE2)    changed !!! */
                          /* we substituted GLOBAL arguments A=PPW, IP=IPIV */

    int  I,J,K,M,KP1,NM1 ;  double  T ;

                     IER=0;
                     IPIV[N]=1;
                     if (N!= 1) {
                        NM1=N-1;
                        for (K=1 ; K<=NM1 ; K++) {
                           KP1=K+1;
                           M=K;
                           for (I=KP1 ; I<=N ; I++)
                           if (fabs(PPW[I][K]) > fabs(PPW[M][K]))  M=I;
                           IPIV[K]=M;
                           T=PPW[M][K];
                           if (M!=K) {
                              IPIV[N]= -IPIV[N];
                              PPW[M][K]=PPW[K][K];
                              PPW[K][K]=T ;
                              }
                           if (T==0.0) goto L7 ;
                           T=1.0/T;
                           for (I=KP1 ; I<=N ; I++) PPW[I][K]= -PPW[I][K]*T ;
                           for (J=KP1 ; J<=N ; J++) {
                              T=PPW[M][J];
                              PPW[M][J]=PPW[K][J];
                              PPW[K][J]=T;
                              if (T!=0.0) for (I=KP1 ; I<=N ; I++)
                                                   PPW[I][J]+=PPW[I][K]*T ;
                              }
                           }
                        }
                     K=N;
                     if (PPW[N][N]!=0.0) return ;

L7 :                 IER=K ;
                     IPIV[N]=0 ;
                     }                                           /* END DEC */

/* ------------------------------------------------------------------------ */

void STORAGE() {

   int  I,J,K ;

          K=0;
          if (LINESTORAGE) NLOOP for (J=1 ; J<=N0 ; J++) PW[++K]=PPW[J][I] ;
          else NLOOP for (J=1 ; J<=N0 ; J++) PPW[J][I]=PW[++K] ;
          }

/* ------------------------------------------------------------------------ */

void PSET() {                               /* changed PEDERV and interface */

   int I,J,K,J1 ;  double R,D,R0,YJ ;

               /*    if MITER = 1, CALL PEDERV AND MULTIPLY BY SCALER.      */
               /*    if MITER = 2, MAKE N CALLS TO DIFFUN TO APPROXIMATE J. */

                  N=N0;

                  if (MITER!=2) {                 /* user-supplied Jacobian */
                     jacob() ;
                     for (I=1 ; I<=NSQ ; I++) PW[I]=CON*relval[mmm+maxsysp+I] ;

                     K=1 ;
                     NLOOP { PW[K]+=1.0; K+=N0+1 ; }
                     K=0;
                     for (J=1 ; J<=N0 ; J++) NLOOP PPW[J][I]=PW[++K] ;
                     }

                  else {                               /* computed Jacobian */
                     D=0.0;
                     NLOOP D+=SAVE2[I]*SAVE2[I];
                     R0=fabs(H)*sqrt(D)*1000.0*UROUND;
                     J1=0;
                     for (J=1 ; J<=N ; J++) {
                        YJ=Y[J][1] ;
                        R=EPSJ*YMAX[J] ;
                        if (R0>R) R=R0 ;
                        Y[J][1]+=R ;
                        D=CON/R ;

                        NLOOP relval[I]=Y[I][1] ;
/* DIFFUN */            derve1() ; if (errnum>0) return ;
/*****                  NLOOP SAVE1[I]=sderiv[I] ;                 *****/
/** redundant ?? **/
                        NLOOP PW[I+J1]=(SAVE1[I]-SAVE2[I])*D ;
                        Y[J][1]=YJ;
                        J1+=N0 ;
                        }
                     K=1 ;                           /* ADD IDENTITY MATRIX */
                     NLOOP { PW[K]+=1.0; K+=N0+1 ; }
                     LINESTORAGE=FALSE; STORAGE();
                     }
                                                   /* LU DECOMPOSITION ON P */
                  DEC() ;                     /* changed!! global arguments */
                  LINESTORAGE=TRUE; STORAGE() ;
                  }                                                 /* PSET */

/* ------------------------------------------------------------------------ */

void SOL() { /* (VAR MATRIXTYPE A ; VECTORTYPE1 B ; VECTORTYPE2 IP) changed */
                 /* we substituted GLOBAL arguments A=PPW, B=SAVE1, IP=IPIV */

    int  I,K,M,NM1,KP1,KM1,KB ;  double  T ;

                if (N!=1) {
                   NM1=N-1;
                   for (K=1 ; K<=NM1 ; K++) {
                      KP1=K+1;
                      M=IPIV[K];
                      T=SAVE1[M];
                      SAVE1[M]=SAVE1[K];
                      SAVE1[K]=T;
                      for (I=KP1 ; I<=N ; I++) SAVE1[I]+=PPW[I][K]*T ;
                      }
                   for (KB=1 ; KB<=NM1 ; KB++) {
                      KM1=N-KB;
                      K=KM1+1;
                      SAVE1[K]/=PPW[K][K];
                      T= -SAVE1[K];
                      for (I=1 ; I<=KM1 ; I++) SAVE1[I]+=PPW[I][K]*T ;
                      }
                   }
                SAVE1[1]/=PPW[1][1] ;
                }                                                /* END SOL */

/* ------------------------------------------------------------------------ */

void STIFF() {                /* some returns in-line; changed DIFFUN calls */

          int  k,I,J,K,J1,J2,IRET1,                  /** START no longer used **/
                  MEO,MIO,MFOLD=0,
          FLAG130=TRUE,
          FLAG150=FALSE,FLAG170=FALSE,FLAG200=FALSE,FLAG400=FALSE,
          IRETFLAG=FALSE,NEXTLOOP=FALSE,CONSTFLAG=FALSE ;

       double  CRAT,FN,R1 ;

        /******         MFOLD=0 ;    this was needed ?????? **** */

               N=N0 ;  MF=MF0 ; EPS=EPS0 ;
               EL[2]=OLDLO=1.0;
               KFLAG=0;
               TOLD=T;
                                                        /* killed CASE here */

               if (JSTART>0) FLAG200=TRUE ;                     /* continue */
               else if (JSTART==0) {                          /* first call */

/* **********     NLOOP Y1[I]=Y[I,1];
                  DIFFUN(Y1,SAVE1); 1st derivative call is in SRUN!
                  In DESIRE/387, DIFFUN0 is in INTER; moves derivatives
                               into SAVE1 and sets IDERIV < 0.
                  NLOOP SAVE1[I]=sderiv[I] ;                      ********* */

                  IDERIV= -1 ;                            /* IDERIV<0 */

                  NLOOP Y[I][2]=H*SAVE1[I];
                  METH=MF/10;
                  MITER=MF-10*METH;
                  L=2;
                  IDOUB=3;
                  RMAX=10000.0;
                  RC=0.0;
                  CRATE=1.0;
                  EPSOLD=EPS;
                  HOLD=H;
                  MFOLD=MF;
                  NOLD=N;
                  NSTEP=NSTEPJ=NJE=0 ;
                  NQ=NFE=IRET=1 ;
                  }

               else {                                           /* JSTART<0 */
                  if (MF!=MFOLD) {
                     MEO=METH;
                     MIO=MITER;
                     METH=MF/10;
                     MITER=MF-10*METH;
                     MFOLD=MF;
                     if (MITER != MIO) IWEVAL=MITER;
                     if (METH != MEO) {
                        IDOUB=L+1;
                        IRET=1 ;
                        }
                     else FLAG150=TRUE ;
                     }
                  else FLAG150=TRUE ;
                  }                                       /* JSTART choices */

               do {                                      /* while (FLAG130) */
                  if (FLAG200==FALSE) {
                     if (FLAG170==FALSE) {
                        if (FLAG150==FALSE) {
                           COSET();
                           LMAX=MAXDER+1;
                           RC*=EL[1]/OLDLO;
                           OLDLO=EL[1] ;
                           }                           /* if FLAG150==FALSE */
/* FLAG150=TRUE */      else {
                           FLAG150=FALSE;
                           if (EPS==EPSOLD && N==NOLD) {
                              IRET1=1;
                              IRETFLAG=CONSTFLAG=TRUE ;
                              }
                           else {
                              EPSOLD=EPS;
                              NOLD=N;
                              IRET=1 ;
                              }
                           }
                        if (CONSTFLAG==FALSE) {
                           FN=N;
                           EDN=FN*(TQ[1]*EPS)*(TQ[1]*EPS);
                           E=FN*(TQ[2]*EPS)*(TQ[2]*EPS);
                           EUP=FN*(TQ[3]*EPS)*(TQ[3]*EPS);
                           BND=FN*(TQ[4]*EPS)*(TQ[4]*EPS) ;
                           }
                        else CONSTFLAG=FALSE ;
                        }                              /* if FLAG170==FALSE */

/* FLAG170=TRUE */   else {
                        IRET1=2;
                        IRETFLAG=TRUE;
                        FLAG170=FALSE ;
                        }
                     if (IRETFLAG==FALSE) IRET1=IRET ;
                     else IRETFLAG=FALSE;

 /* ++++++++> */     switch(IRET1) {

                  case 1:    if (H==HOLD) NEXTLOOP=TRUE ;
                             else {
                                RH=H/HOLD;
                                H=HOLD;
                                IREDO=3 ;
                                }
                             break ;

                  case 2:    if (HMIN/fabs(H)>RH) RH=HMIN/fabs(H); break ;
                  case 3:    NEXTLOOP=TRUE ; break ;

/* ++++++++++> */      }                                     /* switch */

                     if (NEXTLOOP==FALSE) {
                        if (HMAX/fabs(H)<RH && RMAX<HMAX/fabs(H)) RH=RMAX ;
                        else if (HMAX/fabs(H) < RH) RH=HMAX/fabs(H) ;
                        else if (RMAX<RH) RH=RMAX;
                        R1=1.0;
                        for (K=2 ; K<=L ; K++) {
                           R1*=RH; NLOOP Y[I][K]*=R1 ;
                           }
                        H*=RH; RC*=RH;
                        IDOUB=L+1;
                        if (IREDO==0) {
                           RMAX=10.0 ;
                           goto L9999 ;                     /* RETURNSTIFF2 */
                           }
                        }
                     }                                /* if FLAG200== FALSE */

/*FLAG200=TRUE*/  do {                                     /* while (NQ==1) */

                     FLAG200=NEXTLOOP=FALSE;
                     if (fabs(RC-1.0)>0.3 || NSTEP>=NSTEPJ+20) IWEVAL=MITER;
                     T+=H;
                     for (J1=1 ; J1<=NQ ; J1++)
                        for (J2=J1 ; J2<=NQ ; J2++) {
                           J=NQ+J1-J2;
                           NLOOP Y[I][J]+=Y[I][J+1] ;
                           }
                     do {                           /* while (IEVAL==MITER) */

                        NLOOP XERROR[I]=0.0;
                        M=0;

                        NLOOP relval[I]=Y[I][1] ;
/* DIFFUN */            derve1() ; if (errnum) return ;
                        NLOOP SAVE2[I]=sderiv[I] ;

                        NFE++ ;
                        if (IWEVAL>0) {
                           IWEVAL=0;
                           RC=1.0;
                           NJE++ ;
                           NSTEPJ=NSTEP;

/* ++++++++> */            switch(MITER) {

                      case    1 : break ;                  /* analytic */
                      case    2 : NFE+=N; break ;        /* difference */
                      case    3 :                          /* diagonal */
                                  R=EL[1]*0.1 ;
                                  NLOOP {
                                     PW[I]=Y[I][1]+R*(H*SAVE2[I]-Y[I][2]) ;
                                     relval[I]=PW[I] ;
                                     }
/* DIFFUN */                      derve1() ; if (errnum) return ;
/*****                            NLOOP SAVE1[I]=sderiv[I] ;       *****/

                                  NFE++ ;
                                  HLO=H*EL[1];
                                  NLOOP {
                                     RO=H*SAVE2[I]-Y[I][2];
                                     PW[I]=1.0;
                                      D=0.1*RO-H*(SAVE1[I]-SAVE2[I]);
                                      SAVE1[I]=0.0;
                                      if (fabs(RO) >= UROUND*YMAX[I]) {
                                         if (fabs(D)==0.0) {

/* was PROCE420() ------------------------- */

               T=TOLD;
               RMAX=2.0;
               for (J1=1 ; J1<=NQ ; J1++) for (J2=J1 ; J2<=NQ ; J2++) {
                  J=(NQ+J1)-J2;
                  for (k=1 ; k<=N ; k++) Y[k][J]-=Y[k][J+1] ;
                  }                        /* can't use NLOOP, I is in use! */
               if (fabs(H)<=HMIN*1.00001) {
                  KFLAG= -3 ;
                  goto L9999 ;                              /* RETURNSTIFF3 */
                  }
               RH=0.25;
               IREDO=1;
               FLAG170=TRUE ;
               goto L130 ;
               }
   /* ---------------------------- */

                                         PW[I]=0.1*RO/D;
                                         SAVE1[I]=PW[I]*RO ;
                                         }
                                      }
                                   FLAG400=TRUE ;
                                   break ;

/* ++++++++++> */           }                                    /* switch */

                           if (FLAG400==FALSE) {
                              CON= -H*EL[1];
                              PSET(); if (errnum) return ;
                              if (IER!=0) {

/* was PROCE420() ----------------------- */

               T=TOLD;
               RMAX=2.0;
               for (J1=1 ; J1<=NQ ; J1++) for (J2=J1 ; J2<=NQ ; J2++) {
                  J=(NQ+J1)-J2;
                  NLOOP Y[I][J]-=Y[I][J+1] ;
                  }
               if (fabs(H)<=HMIN*1.00001) {
                  KFLAG= -3 ;
                  goto L9999 ;                            /* RETURNSTIFF3 */
                  }
               RH=0.25;
               IREDO=1;
               FLAG170=TRUE ;
               goto L130 ;
               }
    /* ---------------------------- */

                              NLOOP SAVE1[I]=H*SAVE2[I]-(Y[I][2]+XERROR[I]) ;
                              LINESTORAGE=FALSE ; STORAGE();
                              SOL();
                              FLAG400=TRUE ;
                              }
                           D=0.0;
                           NLOOP {
                              XERROR[I]+=SAVE1[I];
                              D+=SAVE1[I]*SAVE1[I]/(YMAX[I]*YMAX[I]);
                              SAVE1[I]=Y[I][1]+EL[1]*XERROR[I] ;
                              }
                           }                                    /* IWEVAL>0 */

                        do {                                /* while (M!=3) */
                           if (FLAG400==FALSE) {
                              if (MITER==0) {
                                 D=0.0;
                                 NLOOP {
                                    R=H*SAVE2[I]-Y[I][2];

                         D+=(R-XERROR[I])*(R-XERROR[I])/(YMAX[I]*YMAX[I]) ;

                                    SAVE1[I]=Y[I][1]+EL[1]*R;
                                    XERROR[I]=R ;
                                    }
                                 }
                              else {

/* ++++++++> */                  switch(MITER) {

                             case  1 :
                             case  2 :

                        NLOOP SAVE1[I]=H*SAVE2[I]-(Y[I][2]+XERROR[I]) ;

                                       LINESTORAGE=FALSE ; STORAGE();
                                       SOL() ;
                                       break ;

                             case  3 : PHLO=HLO;
                                       HLO=H*EL[1];
                                       if (HLO != PHLO) {
                                          R=HLO/PHLO;
                                          NLOOP {
                                             D=1.0-R*(1.0-1.0/PW[I]);
                                             if (fabs(D)==0.0) {
                                                IWEVAL=MITER;
                                                goto L220 ;
                                                }
                                             PW[I]=1.0/D ;
                                             }
                                          }
                  NLOOP SAVE1[I]=PW[I]*(H*SAVE2[I]-Y[I][2]-XERROR[I]) ;
                  break ;

/* ++++++++++> */      }                                     /* switch */
                               D=0.0;
                               NLOOP {
                                  XERROR[I]+=SAVE1[I];
                                  D+=SAVE1[I]*SAVE1[I]/(YMAX[I]*YMAX[I]);
                                  SAVE1[I]=Y[I][1]+EL[1]*XERROR[I] ;
                                  }
                              }                                 /*else */
/* FLAG400=TRUE */         }

                        FLAG400=FALSE;
                        if (M!=0) {
                           if (0.9*CRATE>D/D1) CRATE*=0.9 ;
                           else CRATE=D/D1;
                           }
                        if (2.0*CRATE<1.0) CRAT=2.0*CRATE ;
                        else CRAT=1.0;
                        if (D*CRAT<=BND) goto L450;
                        D1=D;
                        M++ ;

                        if (M!=3) NLOOP relval[I]=SAVE1[I] ;
/* DIFFUN */            derve1() ; if (errnum) return ;
                        NLOOP SAVE2[I]=sderiv[I] ;

                        } while (M!=3) ;

                     NFE+=2;
                     if (IWEVAL != -1) {

/* was PROCE 420() ------------------------------------- */

               T=TOLD;
               RMAX=2.0;
               for (J1=1 ; J1<=NQ ; J1++) for (J2=J1 ; J2<=NQ ; J2++) {
                  J=(NQ+J1)-J2;
                  NLOOP Y[I][J]-=Y[I][J+1] ;
                  }
               if (fabs(H)<=HMIN*1.00001) {
                  KFLAG= -3 ;
                  goto L9999 ;                            /* RETURNSTIFF3 */
                  }
               RH=0.25;
               IREDO=1;
               FLAG170=TRUE ;
               goto L130 ;
               }

/* ------------------------------------------------ */

                     else IWEVAL=MITER;

 L220 :              continue ;
                     } while (IWEVAL==MITER ) ;

 L450 :           if (MITER!= 0) IWEVAL= -1;
                  NFE+=M;
                  D=0.0;
                  NLOOP D+=XERROR[I]*XERROR[I]/(YMAX[I]*YMAX[I]) ;
                  if (D<=E) {
                     KFLAG=IREDO=0;
                     NSTEP++ ;
                     HUSED=H;
                     NQUSED=NQ;
                     for (K=1 ; K<=L ; K++)
                        NLOOP Y[I][K]+=EL[K]*XERROR[I];
                     if (IDOUB!=1) {
                        IDOUB-- ;
                        if (IDOUB<=1)
                           if (L!=LMAX) NLOOP Y[I][LMAX]=XERROR[I];
                        goto L9999 ;
                        }
                       else {
                          PR3=1.0E+20;
                          if (L!=LMAX) {
                             D1=0.0;

NLOOP D1+=(XERROR[I]-Y[I][LMAX])*(XERROR[I]-Y[I][LMAX])/(YMAX[I]*YMAX[I]);

                            if (D1<=0) { errnum=228 ; return ; }

                            ENQ3=0.5/(L+1);
                            PR3=exp(ENQ3*log(D1/EUP))*1.4+1.4E-06 ;
                            }
                          }                                      /* IDOUB=1 */
                       }                                            /* D<=E */
/* D>E */
                    else {
                       KFLAG-- ;
                       T=TOLD;
                       for (J1=1 ; J1<=NQ ; J1++)
                               for (J2=J1 ; J2<=NQ ; J2++) {
                          J=(NQ+J1)-J2;
                          NLOOP Y[I][J]-=Y[I][J+1] ;
                          }
                       RMAX=2.0;
                       if (fabs(H) <= HMIN*1.00001) {
                          KFLAG= -1 ;
                          goto L9999 ;                      /* RETURNSTIFF5 */
                          }
                       if (KFLAG > -3) {
                          IREDO=2;
                          PR3=1.0E+20 ;
                          }
                       else goto L640 ;
                       }                                       /* else D>=E */
                    ENQ2=0.5/L;
                    PR2=exp(ENQ2*log(D/E))*1.2+1.2E-06;
                    PR1=1.0E+20;
                    if (NQ != 1) {
                       D=0.0;
                       NLOOP D+=Y[I][L]*Y[I][L]/(YMAX[I]*YMAX[I]);
                       ENQ1=0.5/NQ;
                       PR1=exp(ENQ1*log(D/EDN))*1.3+1.3E-06 ;
                       }
                    if (PR2 <= PR3) {
                       if (PR2 <= PR1) {
                          NEWQ=NQ;
                          RH=1.0/PR2 ;
                          }
                       else {
                          NEWQ=NQ-1;
                          RH=1.0/PR1 ;
                          }
                       }
                    else {
                       if (PR3 >= PR1) {
                          NEWQ=NQ-1;
                          RH=1.0/PR1 ;
                          }
                       else {
                          NEWQ=L;
                          RH=1.0/PR3;
                          if (RH < 1.1) {
                             IDOUB=10 ;
                             goto L9999 ;                   /* RETURNSTIFF6 */
                             }
                          NLOOP Y[I][NEWQ+1]=XERROR[I]*EL[L]/L;
                          NQ=NEWQ;
                          L=NQ+1;
                          IRET=2;
                          goto L130 ;
                          }
                       }
                    if (KFLAG == 0 && RH < 1.1) {
                    IDOUB=10 ;
                    goto L9999 ;                            /* RETURNSTIFF6 */
                    }
                 if (NEWQ==NQ) {
                    FLAG170=TRUE;
                    goto L130 ;
                    }
                 else {
                    NQ=NEWQ;
                    L=NQ+1;
                    IRET=2;
                    goto L130 ;
                    }

L640 :           if (KFLAG == -7) {
                    KFLAG= -2 ;
                    goto L9999 ;                            /* RETURNSTIFF4 */
                    }
                 RH=0.1;
                 if (HMIN/fabs(H) > RH) RH=HMIN/fabs(H);
                 H*=RH;

                 NLOOP relval[I]=Y[I][1] ;
/* DIFFUN */     derve1() ; if (errnum) return ;
/*****           NLOOP SAVE1[I]=sderiv[I] ;                            *****/

                 NFE++ ;
                 NLOOP Y[I][2]=H*SAVE1[I];
                 IWEVAL=MITER;
                 IDOUB=10 ;
                 } while ( NQ == 1) ;

              NQ=1;
              L=2;
              IRET=3;

L130 :        continue ;
              } while (FLAG130==TRUE) ;
           return ;

L9999 :    NLOOP SAVE1[I]=SAVE2[I]=XERROR[I]=0.0 ;
           HOLD=H;
           JSTART=NQ;
           }                                                      /*  STIFF */

/* ------------------------------------------------------------------------ */

void  DRIVE() {                 /* CALLED BY srun(); USES STIFF(), INTERP() */

      int  I,J,KGO,INDX,NHOUT,   FLAG,FLAG1,FLAG2 ;  double  AYI,FLOATN ;

      FLAG=FLAG1=FLAG2=FALSE ;

/* +++++++++> */ switch(aINDEX) {

    case  -1 :                                               /*  aINDEX=-1  */
               if (size<=12) MF=size+1 ; else MF=size+7 ;   /* just in case */

               TOUT+=zCOMINT ;                 /* tnext=tnext+COMINT <****/
               if ((T-TOUT)*H<0) {
                  JSTART= -1;
                  N0=N;
                  EPS0=EPS;
                  MF0=MF;
                  FLAG=FLAG1=TRUE;
                  }
               else {
                  INTERP() ;         /* interpolation: note PASCAL comments */
                  aINDEX= -5 ;
                  goto L7 ;
                  }                                                 /* else */
               break ;

    case   0 :                                                 /* aINDEX=0  */
               TOUT+=zCOMINT ;                 /* tnext=tnext+COMINT <****/
               HMAX=fabs(TOUT-TOUTP)*10;
               if ((T-TOUT)*H>=0) {
                  INTERP();
                  goto L999 ;
                  }
               FLAG=FLAG1=TRUE ;
               break ;
    case   1 :                                    /* aINDEX=1, initial call */

/***              if ((EPS<=0.0) etc....  - taken care of in SRUN.C
                  tnext is also set up in SRUN.C                          ***/

               if (size<=12) MF=size+1 ; else MF=size+7 ;

               NLOOP {
                  YMAX[I]=fabs(Y0[I]);
                  if (YMAX[I]==0.0) YMAX[I]=1.0;
                  Y[I][1]=Y0[I] ;
/* YMAX */        }
               N0=N;
               T=XT0;
               H=H0;

               if (T+H==T) { errnum=221 ; return ; }      /* this stops us! */

               HMIN=fabs(H0);
               HMAX=fabs(XT0-TOUT)*10.0;
               EPS0=EPS;
               MF0=MF;

               JSTART=0;                         /* for first call to STIFF */
               N0=N;
               NSQ=N0*N0;
               EPSJ=sqrt(UROUND);
               NHOUT=0;
               FLAG=TRUE ;
               break ;
    case   2 :                                                /*  aINDEX=2  */
               TOUT+=zCOMINT ;                 /* tnext=tnext+COMINT <****/
               HMAX=fabs(TOUT-TOUTP)*10.0;
               if ((T-TOUT)*H>=0.0) goto L888 ;         /* was RETURNDRIVE2 */
               if ((T+H-TOUT)*H>0.0) {
                  if (fabs(T-TOUT)<=256.0*UROUND*HMAX) goto L999 ;
                  H=(TOUT-T)*(1.0-4.0*UROUND);
                  JSTART= -1 ;
                  }
               FLAG=FLAG1=TRUE;
               break ;
    case   3 :                                                 /* aINDEX=3  */
               TOUT+=zCOMINT ;                 /* tnext=tnext+COMINT <****/
               FLAG=FLAG1=TRUE;
               break ;

 default :     aINDEX= -4 ; goto L7 ;

/* ++++++++++> */       }                                         /* switch */

      while (FLAG) {                              /* START INTEGRATION HERE */

         FLAG=FALSE;
         if (FLAG1==TRUE) {
            if (T+H==T) { errnum=221 ; return ; }              /* deadlock1 */
            FLAG1=FALSE ;
            }

         STIFF(); if (errnum) return ;            /* ***** HERE WE GO! **** */

         IDERIV=1 ;                               /* state vars. valid again */
         KGO=1-KFLAG;

/* ++++++++++> */   switch(KGO) {

case   1 :                                                       /* KFLAG=0 */

         D=0.0;
         NLOOP {
            AYI=fabs(Y[I][1]) ;
            if (AYI>YMAX[I]) YMAX[I]=AYI ;
            D+=AYI*AYI/(YMAX[I]*YMAX[I]) ;
/*YMAX*/    }
         D*=UROUND*UROUND/(EPS*EPS) ;
         FLOATN=N;
         if (D>FLOATN) {                      /* deadlock: EPS is too small */
            errnum=223 ; return ;
            }
         if (aINDEX==3) goto L888 ;                     /* was RETURNDRIVE2 */
         if (aINDEX==2) {
            if ((T+H-TOUT)*H>0.0) {
               if (fabs(T-TOUT)<= 256.0*UROUND*HMAX) goto L999;
               if ((T-TOUT)*H>=0.0) goto L888 ;         /* was RETURNDRIVE2 */
               H=(TOUT-T)*(1.0-4.0*UROUND);
               JSTART= -1 ;
               }
            }
         else if ((T-TOUT)*H >=0.0) {
            INTERP();                 /* interpolation: see PASCAL comments */
            goto L999 ;
            }
         FLAG1=FLAG=TRUE ;
         break ;

  case 2 : errnum=224 ; return ;                   /* KFLAG= -1; fabs(H)=HMIN */

  case 3 : errnum=225 ; return ;  /* KFLAG= -2; requested error is too small */

  case 4 : errnum=226 ; return ;    /* KFLAG= -3; corrector did not converge */

/* ++++++++++> */       }                                         /* switch */

         if (FLAG2==TRUE) {
            FLAG2=FALSE;
            if (NHOUT==10) { errnum=227 ; return ; }          /* unsolvable */
            NHOUT++ ;
            HMIN*=0.1 ;
            H*=0.1 ;                                     /* H was decreased */
            JSTART= -1;
            FLAG=FLAG1=TRUE ;
            }
         }                                                         /* while */
       goto L7 ;

L888 : TOUT=T;                                          /* was RETURNDRIVE2 */
       NLOOP Y0[I]=Y[I][1];
L999 : aINDEX=KFLAG;                                    /* was RETURNDRIVE1 */
       TOUTP=TOUT;
       H0=HUSED;
       if (KFLAG!=0) H0=H ;
L7 :   if (NFE>10000) { errnum=220 ; return ; }
       relval[mmm+5]=TOUT ;

       }     /*  end of DRIVE(); TOUT is now t, not tnext; relval[mmm+3]=H0 */
