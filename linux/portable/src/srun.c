/* SRUN.C      Simulation-run Control, calls integration - LINUX       */
/*             UNIX Version for XWINDOWS                               */
/*             Registered Copyright 2002  G.A. Korn                    */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/**        added error message for nn1>EQumax; also typ        5/20/97 */
/**        add ctl-c handler                                   8/01/01 */

/** to accommodate MM, change COMINT from relval[mmm+15] (now MM)
         to zCOMINT and kill relval[mmm+16] (was nn1)
         add MM routine to get fewer display point             9/18/02 */
/** zCOMINT, IDERIV, ICOM, INITE1, ztnext no longer in relval  9/21/02 */
/*   correction to initialize relval[mmm+15] so MM>=1          9/29/02 */ 

/* ------------------------------------------------------------------- */

#include "declare.h"
#include "global.h"


extern void derror() ;   extern void derve1() ;   extern void DRIVE() ;
extern void drstart() ;  extern void disply() ;   extern void dflush() ;

extern void ru1() ;      extern void ru2() ;      extern void ru3() ;
extern void ru4() ;      extern void ru5() ;      extern void ru6() ;
extern void ru7() ;      extern void ru8() ;


extern int aINDEX ;      extern double TOUT ;           /* in GEARER.C */
extern int typeflg ;


void typex() {                                         /* TYPE routine */

  int  lj ;

  fprintf(outptr,"%#-13.5e",relval[mmm+5]) ;                   /* write t */
  lj=1 ; do fprintf(outptr,"%#13.5e",*bufdisp[lj++]) ; 
                                          while (bufdisp[lj]!=NULL) ;
  putc('\n',outptr) ; if (ferror(outptr)) derror(106) ;
  }                                                           /* typex */


void dstor()  {                                /*  for STASH statement */

  int lj ;

  lj=0 ; do                                             /* t is first! */
     fwrite(bufstash[lj++],sizeof(double),1,fd[maxchan]) ;
                                           while (bufstash[lj]!=NULL) ;
  if (ferror(fd[maxchan])) derror(106) ;
  }                                                          /* dstor */


void recovr() {                   /*  for compiled "recover" statement */

   int  lj ;

   lj=0 ; do                                            /* t is first! */
      fread(bufrecvr[lj++],sizeof(double),1,fd[maxchan-1]) ;
                                           while (bufrecvr[lj]!=NULL) ;
   if (ferror(fd[maxchan-1])) derror(18) ;
   }                                                         /* recovr */


void reset0() {           /*  reset DT, t, state variables, and INITE1 */

       int  lj ;

       if (size) {                                 /* true integration */
          relval[mmm+3]=relval[mmm-3] ;                    /* reset DT */
          for (lj=1 ; lj<=nn1 ; lj++) relval[lj]=storesv[lj] ;
          }
       relval[mmm+5]=relval[mmm-2] ;                        /* reset t */
       INITE1=1 ;                           /* INITE1>0, re-initialize */
       }

 /********     reset arrays, if any (no longer used!)
                  
     void reset1() {                               
       
       int  lj ;
         
       strcpy(fname,"SYSPIC.DAT") ;
       if ((lj=open(fname,O_RDONLY))==-1) derror(11) ;
       if (read(lj,&IDERIV,
          sizeof(double)*(stashflag-mmm))==-1) derror(18) ;
       if (close(lj)==-1) derror(78) ;
       }                                           **************/         

/*  --------------------------------------------------------------------
   NOTE: the "relval" array contains:

        relval[i] for i=1 to i=maxnsv    state variables
                          to i=mmm   saved initial t, DT, 2 spares
                          to i=mmm+maxsysp system variables
                          to i=gjj (maxrlvar less stack space)
                              REAL variables as per DESIRE symbol table

        nn1 is no longer in relval
        relval mmm+ 19 to 23 are n longer in relval
       
        relval[0] etc. is unused to correspond to PASCAL version
   ------------------------------------------------------------------ */

void srun() {                              /*  simulation RUN routine */

       int  lj, dcounter ; double  anpts, tmm ;   /* dcounter for MM displays <***/

       runf=1 ;              /* to keep display label in command mode */

       if (relval[mmm+15]<1) relval[mmm+15]=1 ;  /* MM must be >=1    */
       
       if (relval[mmm+1]<=1) relval[mmm+1]=251 ; /* AN=NN output pts. */
       if (relval[mmm+4]<=0) {                                /* TMAX */
          if (size) derror(87) ;
          else relval[mmm+4]=relval[mmm+1]-1 ;             /* irule 0 */
          }
       zCOMINT=relval[mmm+4]/(relval[mmm+1]-1) ;        /* COMINT <*****/

       if (INITE1>0) ICOM=1 ;   /* initially, ICOM>0 only if INITE1>0 */
       else ICOM= -1 ;             /* for "continued-run" store, etc. */

       if (size) {                              /* "true" integration */
          if (relval[mmm+5]==1.1E-275) relval[mmm+5]=0 ; /**!!**//* t */
          if (relval[mmm+3]<=0) relval[mmm+3]=zCOMINT/2 ;    /*DT <****/
          anpts=relval[mmm+4]/relval[mmm+3]+1 ;
          if (relval[mmm+1]>anpts) relval[mmm+1]=anpts ;        /* AN */

          relval[mmm-3]=relval[mmm+3] ;          /* save DT for reset */
          for (lj=1 ; lj<= nn1 ; lj++)
          storesv[lj]=dpsv[lj]=relval[lj] ;       /* save state vars.
                           for reset, make duplicates for integration */
          hdt=relval[mmm+3]/2 ;                               /* DT/2 */

          if ((size>3)&&(size!=5)) {  /* prepare for variable-step integration */

             if (relval[mmm+9]<=0) relval[mmm+9]=0.001 ;     /* ERMAX */
             if (size<9) {
                if (relval[mmm+7]<=0 || relval[mmm+7]>zCOMINT)  
                          relval[mmm+7]=zCOMINT ;        /* DTMAX <****/
                          
                if (relval[mmm+8]<=0 || relval[mmm+8]>relval[mmm+7])
                          relval[mmm+8]=relval[mmm+3]/16 ;   /* DTMIN */
                if (relval[mmm+10]<=0 || relval[mmm+10]>relval[mmm+9])
                       relval[mmm+10]=0.01*relval[mmm+9] ;   /* ERMIN */
                if (relval[mmm+2]>nn1) derror(93) ; /*CHECKN too large*/
                }                          /* 3 < irule < 9 and not 5 */
             else if (jcstart==0 && (size==10 || size==14))
                                derror(229) ;     /* missing JACOBIAN */
             }                                 /* irule > 3 and not 5 */
          }                                             /* irule != 0 */
       else if (relval[mmm+5]==1.1E-275) relval[mmm+5]=1 ; 
                                               /***!!!***/ /* irule 0 */
       t0=relval[mmm-2]=relval[mmm+5] ;        /* save t0=t for reset */
       tmm=t0+relval[mmm+4] ;                        /* tmm = t0+TMAX */
       ztnext=t0+zCOMINT ;                /* tnext = t0 + COMINT <*****/

       dflag=dispp && dbegin ; sflag=disk && sbegin ;

/* ------------------------------------------------------------------ */

       if (dflag) {                          /* start display, if any */

          lj=dbegin ;               /* start of display label, if any */

          if (dmode<0) {                   /* "type"; do header label */
             if (!typeflg) {                  /* ## suppresses header */
                WRITELN ; 
                fprintf(outptr,"\n\nt,   ") ;
                if (ferror(outptr)) derror(106) ;
                while (warea[lj]!=lf && warea[lj]!='|') 
                                      putc(warea[lj++],outptr) ;      
                }
             fprintf(outptr,"\n\n") ;     /* could be printer or file */
             }                                           /* if "type" */

          else if (dispp>0) drstart() ;  /* dmode>=0, start CRT graph */
          }                               /* unless we have display 2 */                 
/* ------------------------------------------------------------------ */

       if (rbegin) {
          if (fd[maxchan-1]==NULL) { chanlno=maxchan-1 ; derror(64) ; }
          }                                               /* not open */
       if (sflag) {                           /* connect .TIM file(s) */
          strcpy(fname,pname) ;
          lj=0 ;
          if (fname[lj]=='.') lj++ ; if (fname[lj]=='.') lj++ ;
          do lj++ ; while (fname[lj]!='.') ;         /* get extension */
          fname[lj+1]='T' ; fname[lj+2]='I' ; fname[lj+3]='M' ;
          fname[lj+4]=0 ;

          if (fd[maxchan]!=NULL)
             { chanlno=maxchan ; derror(63) ; }               /* busy */

          if ((fd[maxchan]=fopen(fname,"w"))==NULL) derror(11) ; 
          }
/* ------------------------------------------------------------------ */
                                                /* communication loop */

        /*  NOTE: on "continued" runs, displays and stash files
                  DUPLICATE the last point of a run as the first point
                  of the "continued" run. "Continued" runs reproduced
                  from .RCV files necessarily have THE SAME t0, TMAX,
                  and NN as the original runs, so that the duplicated
                  points are reproduced correctly.

                  Time histories reproduced from STORE or DELAY arrays,
                  however, do NOT contain duplicated points and can be
                  used anywhere in a subsequent run.                  */

/*  ------------------------------------------------------------------ */
       IDERIV=1 ;           /* IDERIV>0, state variables valid */
       dcounter=0 ;                                /* initialize dcounter  <*****/

   /* initially, ICOM>0 only if INITE1>0;  otherwise, for continued-run
    STORE etc., ICOM<0, so the duplicate endpoints are not stored.
    Now make the initial derivative call; this scheme ensures that
    defined variables can be computed for output */

/*  ------------------------------------ */

       if (size>8) {        /* use GEAR module, initialize if INITE1>0 */

          if (nn1>EQUMAX) derror(100) ;    /* too many state variables */
          TOUT=ztnext ;                  /* tnext for GEARER.C */
          if (INITE1>0) aINDEX=1 ;   /* ** was in gearstart ** */

          do {   /* because of duplicate derve1, ICOM, INITE1 are OFF! */

             if (rbegin) { recovr() ; if (errnum) goto L5 ; }
             derve1() ; if (errnum) goto L5 ;
             
             if (dcounter == 0) {  /*  display only once in MM communication points <***/
               if (dflag) { if (dmode>=0) disply() ; else typex() ; }
               if (sflag) dstor() ;
               }                                     
             if (++dcounter==relval[mmm+15]) dcounter=0 ;    /*   <*****/                             

             ICOM=INITE1= -1 ;     /* ICOM<0, INITE1<0 for store, etc. */

             DRIVE(); if (errnum) goto L5 ;    /* driver routine in GEAR
                                        also increments TOUT by COMINT */

             ICOM=1 ;                /* ICOM>0, mark next output point */

             if (spare1) {                       /* look at ctl-c flag */
                spare1=0 ;
                if (lj=getchar()!=lf) {errnum=306 ; goto L55 ; }
                }

             } while (relval[mmm+5]-tmm<0) ;    /* t - (t0 + TMAX) < 0 */
          }                                           /* if Gear module */

       else do {             /* NOT Gear module; ICOM, INITE1 are OFF! */

          if (rbegin) { recovr() ; if (errnum) goto L5 ; }
          derve1() ; if (errnum) goto L5 ; /* initial values of defined variables */

          if (dcounter == 0) {  /*  display only once in MM communication points <***/
             if (dflag) { if (dmode>=0) disply() ; else typex() ; }
             if (sflag) dstor() ;
             }                                     
          if (++dcounter==relval[mmm+15]) dcounter=0 ;          /*   <*****/   
         
          ICOM=INITE1= -1 ;    /* ICOM<0, INITE1<0
                                                       for store, etc. */
          switch(size) {

case 0 : relval[mmm+5]=ztnext ; /* dummy integrator: t = tnext */
                           break ;
case 1 : ru1() ; break ;                         /* integration loops */
case 2 : ru2() ; break ;
case 3 : ru3() ; break ;
case 4 : ru4() ; break ;
case 5 : ru5() ; break ;
case 6 : ru6() ; break ;
case 7 : ru7() ; break ;
case 8 : ru8() ; break ;

             } if (errnum) goto L5 ;

          ztnext+=zCOMINT ;                 /* tnext=tnext+COMINT <*****/
          ICOM=1 ;                   /* ICOM>0, mark next output point */
  
          if (spare1) {                          /* look at ctl-c flag */
             spare1=0 ;
             if (lj=getchar()!=lf) {errnum=306 ; goto L55 ; }
             }

          } while (relval[mmm+5]-tmm<0) ;       /* t - (t0 + TMAX) < 0 */

/* ------------------------------------------------------------------- */
                                                         /* last point */
       if (rbegin) { recovr() ; if (errnum) goto L5 ; }
       derve1() ; if (!errnum) goto L6 ;

                     /* clean up; some of those errors are not errors! */

L5 :   if (errnum==303) { errnum=0 ; goto L6 ; }  /* derve1 terminated */
       if (errnum==305) {
          printf("\n Warning: END OF FILE or error on recover -    ") ;
          errnum=0 ;                        /* EOF or error on recover */
          }
          if (errnum>=304) errnum=0 ;                           /* eof */
       goto L55 ;         /* close stash and/or recovr file(s), if any */

L6 :   if (dflag) { if (dmode>=0) disply() ; else typex() ; }  /* last */
       if (sflag) dstor() ;                                   /* point */

L55 :  if (sflag) {                        /* close stash file, if any */
          if (fclose(fd[maxchan])==EOF)  derror(95) ;
          fd[maxchan]=NULL ;
          }
       if (rbegin) {                     /* close recover file, if any */
          if (fclose(fd[maxchan-1])==EOF) derror(95) ;
          fd[maxchan-1]=NULL ;
          }
       if (!dflag || dmode<0) WRITELN ; else dflush() ;
       if (errnum) derror(errnum) ;
       }                                                       /* srun */

