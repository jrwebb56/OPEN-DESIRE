/* aexcut3.c     Commands for DESIRE Interpreter  - LINUX                */

/*             Registered Copyright 2012  G.A. Korn                    */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/***         added new random function for GNU              6/13/97    */
/*           add keep+                                      5/12/99    */
/*           add ekiller on reload, rld                     5/18/99    */
/*           add ekiller on new                             5/20/99    */
/*           change editing                                 7/16/01    */
/*           ctl-c handling                                 8/07/01    */
/*           killed error 125                               8/9/01     */
/*           modified new, NEW, erun                        4/2/02     */
/*           killed ekiller after reload, rld, use csh      12/16/02   */
/*           killed reference to spare4                     6/14/04    */
/*           added interpreted vector keyword              10/15/05    */
/*           added zz , like erun                          11/22/06    */
/**          exchanged list and list+, keep and keep+ in comments 8/15/07 **/ 
/*           added AVG                                      1/14/12    */

/* ------------------------------------------------------------------- */

#include <string.h>
       
#include "declare.h"
#include "global.h"

extern void free (void *__ptr) __THROW;
extern void derror() ;  extern void pdtime() ;  extern void drun00() ;
extern void if00() ;    extern void openf() ;   extern void closef() ;
extern void not0() ;    extern void lod00() ;   extern void dump() ;
extern void new00() ;   extern void seekx() ;   extern void writefile() ;
extern void forlop() ;  extern void auto0() ;   extern void showstrt() ;
extern void vector() ;  extern void matrix() ;  extern void dot(int) ;
extern void pic0() ;    extern void reset0() ;  extern void trac00() ;
extern void restor() ;  extern void fft() ;     extern void whil0() ;
extern void displa() ;  extern void rept0() ;   extern void plott() ;
extern void srun() ;    extern void irule() ;
extern void print() ;   extern void def() ;     extern void unpack() ;
extern void dlist() ;   extern void prfile() ;  extern void goto00() ;
extern void old00() ;   extern void declare() ; extern void getinput() ;
extern void cvalexp() ; extern void call() ;    extern void prepr1() ;
extern void writarr() ; extern void newload() ; extern void QuitGraph() ;

extern int getsub() ;   extern double evalexp() ;

/*** FILE *FD ;   ***/                    /* file descriptor for stash */

ptr instal()  {                                   /*  installs symbols */

  ptr P ;

     if (vartyp<0) derror(19) ; if (gir+1>=gjj) derror(40) ;

     P=(struct symnode*)malloc(sizeof(struct symnode)) ;
     P->SWITCH=TRUE ;
     P->link=hashtab[hashval] ;
     strcpy(P->name,symbol) ;
     P->symtype=1 ;
     hashtab[hashval]=P ;
     P->valptr=gir ;
     relval[gir++]=0 ;                           /* 0 in case of error */
     return P ;
     }                                                       /* instal */


ptr getvar()  {           /* look up variable and type, return pointer */

  register unsigned char curr ; int lj ;  ptr P ;
extern int eflag ;                              /* defined in prfile.c */
       hashval=lj=0 ; curr=warea[gi] ;
       if (curr>='A' && curr<='Z' || curr>='a' && curr<='z') goto L7 ;  /* 1st char. must be alpha */
       if (curr=='[') errnum=31 ; else errnum=14 ; return NULL ;

L7 :   do { symbol[lj++]=curr ; hashval+=curr ; curr=warea[++gi] ; }
       while (curr>='A' && curr<='Z' || curr>='a' && curr<='z'
               || curr>='0' && curr<='9' || curr=='$') ;
       if (lj>varsize) derror(15) ; symbol[lj]=EOS ;

       hashval%=maxhash ; P=hashtab[hashval] ;          /* hash lookup */

       if (curr=='[' || start>0) vartyp=-1 ;                  /* array */
       else if (curr=='(') vartyp=0 ;         /* function or procedure */
       else vartyp=1 ;

L10 :  if (P!=NULL) {                        /* fix collisions, if any */
          if (strcmp(P->name,symbol)) { P=P->link ; goto L10 ; }
          if (vartyp>0) { if (P->symtype>0) goto L14 ; goto L12 ; }
          else if (vartyp<0) { if (P->symtype<0) goto L14 ; goto L12 ; }

          if (P->symtype==0 || P->symtype>=8 || P->symtype<-3) {
             if (P->SWITCH) return P ; derror(52) ;       /* recursion */
             }
          P=P->link ; goto L10 ;               /* collision, next link */

L12 :     errnum=73 ; return P ;   /* symbol used for vrble. and array */
L14 :     if (!P->SWITCH) derror(71) ;               /* local variable */
          }
       return P ;
       }                                                     /* getvar */

extern int eflag ;                              /* defined in prfile.c */
void execute() { /*  interprets statements ; switch sequence matches
                           keyword table. On entering, gi points past
                                          1st token; on leaving, to lf */

   int lj, sub, ip ;  double val1, val2 ; ptr  q ;

       if (tracer) {
          if (runf && (tracer==1 || tracer==3 || tracer==5 || tracer==7)) {
     /*        attrset(A_REVERSE) ;   */                  /* reverse video */
             fdn=stdout ;
             unpack(gi-4,gi-4+warea[gi-2],1) ;
     /*        attrset(0) ;           */                  /* restore video */
             }
          }

L10 : switch (warea[gi-1]) {

  /* else */    case 11   : if (warea[gi]==vendls) gi++ ;
                            else {
                               seekx() ;
                               if (warea[gi++]!=vendls) derror(111) ;
                               } ; break ;

  /* connect */ case 12   : openf() ; break ;

  /* if */      case 13   :

           if00() ;
           if (warea[gi]!=vthen) derror(10) ;
           lj=gi+1 ; seekx() ;
           if (warea[gi]!=chr11) derror(111) ;    /* checks error even */
           if (old) gi=lj+1 ; else gi+=2 ;          /*   in dead code! */
           goto L10 ;                               /* no break needed */

  /* while */   case 14   : whil0() ; break ;
  /* repeat */  case 15   : rept0() ; break ;
  /* for */     case 16   : forlop() ; break ;

  /* until */   case 17   :

           if (stktop==NULL) derror(27) ;
           if (stktop->head!='R' || gi!=stktop->tskptr) derror(27) ;
           if00() ;                                            /* test */
           if (!old) gi=stktop->radd ;                /* success, loop */
           else goto L1000 ;                         /* no, don't loop */
           break ;

  /* next */    case 18   :

           if (stktop==NULL) derror(27) ;
           if (stktop->head!='F' || gi!=stktop->tskptr) derror(27) ;
           q=stktop->cvar ;
           relval[q->valptr]+=stktop->step ;
           val1=relval[q->valptr]-stktop->fval ;
           if (stktop->step<0) val1=-val1 ;
           if (val1<=0) gi=stktop->radd ;                      /* loop */
           else {                                        /* don't loop */
              relval[q->valptr]+=-stktop->step ;
              goto L1000 ;
              }                                                /* else */
           break ;

 /* end while */ case 19  :

          if (stktop==NULL) derror(27) ;
          if (stktop->head!='W' || gi!=stktop->tskptr) derror(27) ;
          gi=stktop->radd ;
          if00() ;
          if (!old) { gi=stktop->tskptr ; goto L1000 ; }
          break ;

   /* end */     case 20  :

          if (!runf) derror(22) ;
          if (stktop==NULL || stktop->head!='C') derror(4) ;
          q=stktop->cvar ;
          gi=stktop->radd ;
          goto L900 ;                               /* no break needed */

  /* proceed */ case 21   : break ;                      /* just go on */
  /* STOP */    case 22   : if (runf) derror(302) ; derror(22) ;
  /* drunr */   case 23   : drun00(0) ; srun() ; reset0() ; break ;
/* PROCEDURE */ case 24   : def(1) ; break ;
  /* erase */   case 25   : dlist(-1) ; break ;
  /* list+ */    case 26   : prfile(1) ; break ;

  /* run */     case 27   : if (runf) derror(199) ;
                            if (endtxt>linsiz+2) {
                               gi=linsiz+1 ;
                               prepr1() ;
                               goto L20 ;               /* run in-line */
                               }
                            break ;

  /* data */    case 28   :

                   while (warea[gi]!=lf && warea[gi]!='|') gi++ ; break ;

  /* go to */   case 29   : goto00(1) ; if (!runf) goto L20 ;   /* run */
                            break ;

  /* input */   case 30   : getinput(0) ; break ;
  /* save */    case 31   : writefile() ; break ;
                                                     /* .. 32, then 33 */
 /* FUNCTION */ case 34   : def(0) ; break ;
  /* old */     case 35   : old00(1) ; break ;
  /* load */    case 36   : lod00() ; break ;
  /* clear */   case 37   : prepr1() ; break ;
  /* reset */   case 38   : reset0() ; break ;
  /* chain */   case 39   : old00(0) ; goto L20 ;     /* run; no break */
  /* FFT */     case 40   : fft() ; break ;
                                                         /* to     41  */

  /* dump */    case 42   : system("clear") ; dump() ; break ;
  /* call */    case 43   : call(0) ; break ;
  /* time */    case 44   : system("clear") ; pdtime() ; break ;
  /* read */    case 45   : getinput(1) ; break ;
/* discn'ct */  case 46   : closef() ; break ;

  /*   delete   case 47   : unsave() ; break ;   not used in C version */

  /* drun */    case 48   : drun00(0) ; srun() ; break ;
  /* list  */  case 49   : prfile(0) ; break ;
  /* new */     case 50   : new00(0) ; system("./ekiller.old") ; break ;
  /* irule */   case 51   : if (!runf) system("clear") ; irule() ; break ;

                                                  /* case 52  was size */

  /* help */    case 53   : system("clear") ; prfile(-4) ; break ;
  /* display */ case 54   : displa() ; break ;
  /* STATE */   case 55   : declare(3) ; break ;
                                                     /* eof   case 56  */

  /* go */      case 57   : if (INdex==0) derror(43) ;      /* no STOP */
                            gi=INdex ; INdex=0 ; goto L20 ;     /* run */

  /* note */    case 58   : not0() ; break ;
/* dimension */ case 59   : declare(0) ; break ;

  /* edit */    case 60   : prfile(-2) ; break ;
 /* auto */     case 61   : auto0() ; system("clear") ; break ;
  /* ed */      case 62   : prfile(-5) ; break ;

  /* bye */     case 63   : system("clear") ; SPARE4=1 ;
                            system("./ekiller.old") ;
                            prfile(-3) ;            /* no break needed */

  /* trace */   case 64   : trac00() ; break ;
  /* write */   case 65   : print(0) ; break ;
  /* plot */    case 66   : plott() ; break ;
  /* INTEGER */ case 67   : declare(1) ; break ;
  /* VECTOR */  case 68   : vector() ; break ;
  /* DOT */     case 69   : dot(0) ; break ;
  /* MATRIX */  case 70   : matrix() ; break ;
  /* sh */      case 71   : prfile(-1) ; break ;
  /* DYNAMIC */ case 72   : derror(301) ;
  /* restore */ case 73   : restor() ; break ;
  /* PIC */     case 74   : system("clear") ; pic0() ; break ;

                                                   /*   75  JACOBIAN   */
  /* LABEL */   case 76   : goto00(0) ; break ;
  /* MACRO */   case 77   : def(-1) ; break ;
  /* COMPLEX */ case 78   : declare(-1) ; break ;
                                                  /* invoke    79   :  */

/* LET */ case 80   :                                   /* LET in-line */

    q=getvar() ; if (errnum) derror(errnum) ;
    if (vartyp==0) derror(84) ;
    if (q==NULL) q=instal() ;             /* for simple REAL variables */
    else if (q->symtype<0) sub=getsub(q) ;
    if (warea[gi++]!='=') derror(29) ;

    if (q->symtype==1) {
       relval[q->valptr]=val1=evalexp() ; if (errnum) derror(errnum) ;
       }
    else if (q->symtype==-1) {
       relval[intval[intval[q->valptr]+q->valptr+1]+sub]
                        =val1=evalexp() ; if (errnum) derror(errnum) ;
       }
    else if (q->symtype==2) {
       val1=evalexp() ; if (errnum) derror(errnum) ;
       if (val1>maxint || val1<-maxint) derror(20) ;
       intval[q->valptr]=val1 ;                           /* truncated */
       }
    else if (q->symtype==3) {
       cvalexp(&val1,&val2) ;
       relval[q->valptr]=val1 ; relval[q->valptr+1]=val2 ;
       }
    else if (q->symtype==-2) {
       val1=evalexp() ; if (errnum) derror(errnum) ;
       if (val1>maxint || val1<-maxint) derror(20) ;
       intval[intval[q->valptr]+q->valptr+1+sub]=val1 ;
       }
    else if (q->symtype==-3) {
       cvalexp(&val1,&val2) ;
       sub=intval[intval[q->valptr]+q->valptr+1]+2*sub ;
       relval[sub]=val1 ; relval[sub+1]=val2 ;
       }
    else derror(84) ;

    if (tracer>3) {
     /*  attrset(A_REVERSE) ;    */                       /* reverse video */
       if (abs(q->symtype)==3)
         printf("A: %s%c%g%s%g%s",q->name,'=',val1,"+j*(",val2,")\n") ;
       else printf("A: %s%c%g\n",q->name,'=',val1) ;
  /*     attrset(0) ;   */                                /* restore video */
          }
    break ;

/* ------------------------------------------------------------------- */

 /* COMMENT */ case 81   : while (warea[gi]!=lf) gi++ ; break ;

  /* stash */  case 94   : break ;             /* saves arrays in file */

/***   strcpy(fname,"SYSPIC.DAT") ;
       if ((FD=fopen(fname,"w"))==NULL) derror(11) ; lj=fileno(FD) ;
       if (write(lj,&relval[mmm+20], <IDERIV <********************************
                     sizeof(double)*(gir-mmm))==-1) derror(106) ;
       if (fclose(FD)!=0) derror(78) ;
       stashflag=gir-1 ;
       break ;                       KILLED!            ***/              

/* recover */ /*** case 98    : if (stashflag) reset1() ; break ; ***/

  /* exit */  case 104   :

      if (!runf) derror(22) ;
      if (warea[gi]=='|') {
         if (warea[gi+1]!=vrem && warea[gi+1]!=chr11 &&
                                 warea[gi+1]!=vendls) derror(81) ; }
      else if (warea[gi]!=lf) derror(33) ;

      if (stktop==NULL) derror(98) ;
      if (stktop->head=='C') { gi=stktop->radd ; goto L900 ; }
      gi=stktop->tskptr ;
      if (stktop->head=='R') {
         while (warea[gi]!=lf && warea[gi]!='|') gi++ ;
         }
      goto L1000 ;                                  /* no break needed */

/* SEED */  case 107 : val1=evalexp() ; if (errnum) derror(errnum) ;
                                        srand48(val1) ; break ; /* GNU */

/* ARRAY */ case 108 : declare(0) ; break ;

 /* SHOW */ case 111 :

  if (warea[gi]==lf || warea[gi]=='|') { 
     flag=1 ; showstrt() ; break ; 
     }
  if (warea[gi]=='*') { 
     gi++ ; flag=-1 ; showstrt() ; break ; 
     } 
 
  if (!flag) derror(112) ; 
  start=1 ; q=getvar() ; start=0 ;                         /* get array */
  if (errnum) derror(errnum) ;
  if (q==NULL || q->symtype!=-1) derror(70) ;
  lj=q->valptr ;

  if (flag>0) sub=80 ; else sub=40 ;
  if (warea[gi]==',') {                    /* we have a character count */
     gi++ ;                                               /* skip comma */
/*  main3rreg,.c 
     val1=evalexp() ; if (errnum) derror(errnum) ;
     if (val1>0 && val1<80) sub=val1 ;                     /* truncated */
     }

  WRITELN ;
/*  main3rreg,.c 
  if (intval[lj]==1) writarr(intval[lj+1],sub,intval[lj+2]) ;
  else writarr(intval[lj+1]*intval[lj+2],sub,intval[lj+3]) ;
  WRITELN ; break ;

/* keep*/ case 113 : prfile(4) ; break ;  
/*reload*/ case 115 : system("./ekiller.old") ; newload() ;  break ;                          
/* keep+ */ case 116 : prfile(2) ; break ;

/*erun*/   case 117 :                /* reload SYSPIC.lst after edit */ 
 
    runf=FALSE ; append=TRUE ; eofflg=FALSE ;           /* for prcomp */
    if ((inptr=fopen("SYSPIC.lst","r"))==NULL) {
       ncol=0 ; inptr=stdin ; append=FALSE ;
       derror(11) ;
       }
    SPARE1=1 ;                             /* causes "run" in prcomp.c */
    relval[mmm+13]=1.0 ; cc=15 ; size=1 ;        /* reset run defaults */
    tracer=0 ; flag2=FALSE ;
    break ;

/*rld*/    case 118 : newload() ; break ;                     
/*NEW*/    case 119 : new00(1) ; break ;

/*zz*/   case 120 :    /* same as erun, reload SYSPIC.lst after edit */ 
 
    runf=FALSE ; append=TRUE ; eofflg=FALSE ;           /* for prcomp */
    if ((inptr=fopen("SYSPIC.lst","r"))==NULL) {
       ncol=0 ; inptr=stdin ; append=FALSE ;
       derror(11) ;
       }
    SPARE1=1 ;                             /* causes "run" in prcomp.c */
    relval[mmm+13]=1.0 ; cc=15 ; size=1 ;        /* reset run defaults */
    tracer=0 ; flag2=FALSE ;
    break ;

/*Vector*/ case 123 : vector() ; break ;
/* AVG */        case 131 : dot(2) ; break ;

           default  : derror(300) ; break ;
       }                                                     /* switch */
       goto L40 ;

     

/* ------------------------------------------------------------------- */
/*                        run routine in-line                          */
/* ------------------------------------------------------------------- */

L20 :  WRITELN ; runf=TRUE ; 

L21 :  gi+=5 ;                           /* skip line number and token */
       execute() ;                                  /* note recursion! */
       if (gi>=endtxt-2) derror(301) ;                         /* done */
       if (flag2) derror(302) ;                        /* stepwise run */

       if (spare1) {
          spare1=0 ;  
          if (lj=getchar()!=lf) derror(301) ; /* lf continue, else stop */
          }

       goto L21 ;                                /* end of run in-line */

/* ------------------------------------------------------------------- */

L900 : flag1=FALSE ;       /* for RETURN, EXIT; clear nested-def. flag */

       q->SWITCH=TRUE ;                        /* undo recursion check */
       q=q->next ;
       while(q!=NULL) {                    /* turn local variables OFF */
          q->SWITCH=FALSE ;
          q=q->next ;
          }
L1000 : tptr=stktop ;                             /* pop control stack */
        stktop=stktop->dslink ;
        if (tptr!=NULL) free(tptr) ;

/* ------------------------------------------------------------------  */
                                       /* execute leaves with gi on lf */

L40 : if (warea[gi]=='|') { gi+=2 ; goto L10 ; }  /* more on this line */
      if (warea[gi]!=lf) derror(33) ;               /* bad termination */
      }                                                     /* execute */

