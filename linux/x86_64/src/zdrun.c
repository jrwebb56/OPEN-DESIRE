/* zdrun.c  Compiler forOPEN DESIRE - Linux                              */
/*                                                                       */
/* for threaded code,  loop unrolling                                    */
/*                                                                       */
/*             Registered Copyright 2012  G.A. Korn                      */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/
/*                                                                       */
/*           original version                            6/17/94         */
/*           added ## file modification for type command 8/4/96          */
/*           added delta, DELTA                          3/11/97         */
/*           removed reference to cval int  I1, I2, K1, K2, Fstart, X1end, X2end, N2 ;     5/7/98          */
/*           removed gjj references                      8/20/98         */
/*           unrolled Vector loops                       8/23/98         */
/*           added new Vector features              8/25/98, 8/30/98     */
/*           fixed parenthesis-count bug                  9/1/98         */
/*           added Delta (Vectr delta)                    9/4/98         */
/*           added possibility only for shiftflag         9/7/98         */
/*           changed A*x, A%*x for count=1               9/11/98         */
/*           unrolled DOT loops                          9/12/98         */
/*           restored symtype instead of vartyp in LET   9/18/98         */
/*           permit subscripted state variables          12/21/98        */
/*           mask vector compilation                     12/24/98        */
/*           allow DD(x,tau for delay and tdelay          4/15/99        */ 

/***         ALL-C VERSION                                1/14/99        */
/*           support for func2; func temporary version    4/23/99        */
/*           fixed Vector v{ival}                         8/5/99         */

/*           killed relval[mmm+16]                        9/18/02        */
/*           INITE1 no longer in relval                   9/20/02        */
/*           change 11 entries like csin to ccsin to
                  avoid conflict with new built-in functions 1/14/05     */
/*           add new func1, func2	                  7/27/05	 */
/*	     added gauss()                                5/4/09         */

/**          new threaded code V14                        9/1/07        **/
/*           added AVG                                            1/17/12        */

/* NOTE: maxrlvar, not maxrlvar+40 is the highest relval data address,   */
/* because the top of the relval array also serves as the compiled-data  */
/* stack in this c version, starting down from maxrlvar+40.  Thus,       */
/* no stack checks are needed for the compiler; the interpreter          */
/* still has them.  Similarly, the operator stacks for both the compiler */
/* and the interpreter use the same array warea as the program text!     */
/* All this dates from the old PDP-11 code; nowadays, we have lots of    */
/* memory and would use separate arrays.  It does not hurt anything,     */
/* it just is confusing.                                                 */
/* -------------------------------------------------------------------   */

#include <string.h>
#include <stdlib.h>
#include <setjmp.h>    
#include <errno.h>
#include "declare.h"
#include "global.h"

extern ptr getvar() ;        extern ptr instal() ;    extern double evalexp() ;
extern int getsub() ;        extern double ROUND() ;

extern void goto00() ;     extern void call() ;     extern void derror() ;
extern void showstrt() ;   extern void cshow() ;    extern void gitchn() ;

int  typeflg ;               /* ## flag to make computer-readable file */
/*** int  shiftflag ; <** not used; recompile whenever index-shifting? */  

    void *COMPILE[maxcomp+1] ;    /* compiler function pointers */

/* pointers to virtual-machine instructions */

  	 void *cpush0=NULL ;   void *cpushvar=NULL ;    void *cpop=NULL ;
	 void *cret=NULL ;         void *jret=NULL ;             void *cpopd=NULL ;
	 void *cadd=NULL ;        void *csub=NULL ;          void *cmult=NULL ;
	 void *cdiv=NULL ;         void *cpwr=NULL ;
	 void *CADD=NULL ;       void *CSUB=NULL ;        void *CMULT=NULL ;
	 void *CDIV=NULL ;         void *CPWR=NULL ; 
	 void *ccsin=NULL ;        void *cccos=NULL ;       void *cctan=NULL ;
	 void *ccexp=NULL ;       void *cln=NULL ;            void *cclog=NULL ;
	 void *cctanh=NULL ;      void *ccsinh=NULL ;      void *cccosh=NULL ;
	 void *ccasin=NULL ;      void *ccacos=NULL ;      void *ccatan=NULL ;
	 void *ccatanx=NULL ;    void *csigmoid=NULL ;   void *csigsig=NULL ;
	 void *cran=NULL ;          void *cran1=NULL ;        void *cfabs=NULL ;
	 void *ccsqrt=NULL ;      void *clim=NULL ;            void *csgn=NULL ;
	 void *cswtch=NULL ;     void *ccomp=NULL ;        void *cround=NULL ;
	 void *csat=NULL ;         void *cdeadz=NULL ;        void *cdeadc=NULL ;
	 void *crect=NULL ;        void *ctri=NULL ;              void *cSAT=NULL ;
	 void *ccsinc=NULL ;      void *crecip=NULL ;        
	 void *cterm=NULL ;       void *csample=NULL ;     void *cif=NULL ;
	 void *ctdela=NULL ;       void *cdelay=NULL ;        void *ctrkhld=NULL ;
	 void *cout=NULL ;         void *cfunc=NULL ;          void *cget=NULL ;
	 void *cstore=NULL ;      void *cstep=NULL ;          void *ccount1=NULL ;
	 void *cpatrn=NULL ;      void *cconv=NULL ;         void *cyendli=NULL ;
	 void *cdend=NULL ;
	 void *cupdat=NULL ;      void *cupdatli=NULL ;      void *cmax=NULL ;
	 void *cmin=NULL ;          void *cyend=NULL ;
	 void *cyinit=NULL ;         void *caik=NULL ;            void *caki=NULL ; 
	 void *cainit=NULL ;         void *cpusharr=NULL ;    void *ccshow=NULL ;
	 void *cmaskq=NULL ;     void *cpushv2=NULL ;    void *cshowstrt=NULL ; 
	 void *cclearn=NULL ;      void *cplearn=NULL ;      void *cbarf=NULL ;    
	 void *cMAX=NULL ;         void *cPATRN=NULL ;     void *cAIK=NULL ;
	 void *cAKI=NULL ;
	 void *cfunc2=NULL ;      void *cfuzz=NULL ;     /*  void *cselect=NULL ; */
	 void *cfunc1=NULL ;      void *cxfunc2=NULL ;    void *cgauss=NULL ;        /* &&& <***/
/* --------------------------------------------------------------------------------------------------------------------- */

 void drun00(int spare2) {                                      /* DRUN compiler */

    int gisav,cstart,li,lj,cflag,smpflg,sub,jjj,kkk,ggg,maxflag,MASKBASE ;
    double val ;  ptr  ppp,q,qqq,PP ;

    int  VBASE,VROW,ddtflag ;                     /* for Vector compilation */

/*----------------------------------------------------------------------------------------------*/
                                                        
/*** variables for virtual-machine instructions **/

  int  cmark,dmark,NROW1,NCOL1,YBASE,COUNT ;  /* global for *COMPILEd loops */

  double x,y ;  int  I, K, itau, rj, irow ;             /*****    <*****/

/*----------------------------------------------------------------------- */
  
  int  I1, I2, K1, K2, Ystart, X1end, X2end, N2, Fstart ;
  double x1, x2, HiMEMBER1, LoMEMBER1, HiMEMBER2, LoMEMBER2, Del1, Del2 ;

  /*************/

  int  i, k, j ; 
  int  count, Aindex ;    
  int rowbase, imin, WBASE, XCOL1, XBASE, CRIT, fast, rowbase2, imin2 ;
  double  lrate, crit, sum, MIN, MAX, thresh ;
  int  xindex, yindex, Windex ; 

  int  ROWEND, XEND, COLEND ;
  int  xbase, indexx, Imax ;

  jmp_buf djump,bjump,jjump ;        /* type declared in setjmp.h */

    if (spare2==2) {   /* derivative routine (derve1), USED BY srun, integr. rules 
                   NOTE: stack and array overflow is checked by COMPILER */

       di=ci=0 ;  gjj=maxrlvar+40 ;
       if (setjmp(bjump)) return ;
       goto *COMPILE[ci++] ; 
       }   

   else if (spare2==3) {                /* Jacobian routine (jacob), USED BY pederv */

       di=jdstart ; ci=jcstart ; gjj=maxrlvar+40 ;
        if (setjmp(jjump)) return ;
        goto *COMPILE[ci++] ; 
        }

/* ---------------------------------------------------------------------------------------------------------- */
    else {                                     /**  make pointers to virtual-instruction labels **/

   	cpush0=&&xcpush0 ;  cpushvar=&&xcpushvar ; cpop=&&xcpop ;
	cret=&&xcret ;      jret=&&xjret ;        cpopd=&&xcpopd ;

	cadd=&&xcadd ;      csub=&&xcsub ;      cmult=&&xcmult ;
	cdiv=&&xcdiv ;      cpwr=&&xcpwr ;

	CADD=&&xCADD ;      CSUB=&&xCSUB ;     CMULT=&&xCMULT ;
	CDIV=&&xCDIV ;      CPWR=&&xCPWR ; 

	ccsin=&&xccsin ;    cccos=&&xcccos ;     cctan=&&xcctan ;
	ccexp=&&xccexp ;    cln=&&xcln ;          cclog=&&xcclog ;
	cctanh=&&xcctanh ;  ccsinh=&&xccsinh ;    cccosh=&&xcccosh ;
	ccasin=&&xccasin ;  ccacos=&&xccacos ;    ccatan=&&xccatan ;
	ccatanx=&&xccatanx ; csigmoid=&&xcsigmoid ; csigsig=&&xcsigsig ;
	cran=&&xcran ;       cran1=&&xcran1 ;      cfabs=&&xcfabs ;
	ccsqrt=&&xccsqrt ;   clim=&&xclim ;        csgn=&&xcsgn ;
	cswtch=&&xcswtch ;   ccomp=&&xccomp ;      cround=&&xcround ;
	csat=&&xcsat ;       cdeadz=&&xcdeadz ;     cdeadc=&&xcdeadc ;
	crect=&&xcrect ;  cSAT=&&xcSAT ;   ctri=&&xctri ;

	cterm=&&xcterm ;     csample=&&xcsample ;  cif=&&xcif ;
	ctdela=&&xctdela ;   cdelay=&&xcdelay ;    ctrkhld=&&xctrkhld ;
	cout=&&xcout ;       cfunc =&&xcfunc ;     cget=&&xcget ;
	cstore=&&xcstore ;   cstep=&&xcstep ;      ccount1=&&xccount1 ;
  
	cpatrn=&&xcpatrn ;     cconv=&&xcconv ;    cyendli=&&xcyendli ;
	cdend=&&xcdend ;

	cupdat=&&xcupdat ;    cupdatli=&&xcupdatli ;  cmax=&&xcmax ;
	cmin=&&xcmin ;        cyend=&&xcyend ;

	cyinit=&&xcyinit ;       caik=&&xcaik ;       caki=&&xcaki ; 
	cainit=&&xcainit ;       cpusharr=&&xcpusharr ; ccshow=&&xccshow ;
	cmaskq=&&xcmaskq ;    cpushv2=&&xcpushv2 ;  cshowstrt=&&xcshowstrt ; 
	cclearn=&&xcclearn ;     cplearn=&&xcplearn ;    cbarf=&&xcbarf ;    

	cMAX=&&xcMAX ;        cPATRN=&&xcPATRN ;  cAIK=&&xcAIK ;
	cAKI=&&xcAKI ;

	cfunc2=&&xcfunc2 ;   cfuzz=&&xcfuzz ;   /** cselect=&&xcselect ; **/
	cfunc1=&&xcfunc1 ;   cxfunc2=&&xcxfunc2 ;  cgauss=&&xcgauss ; /* &&& <***/
                                                                                                                     
                      goto L999 ;

/* ------------------------------------- actual drun00 starts here ------------------------------------------------------- */ 
      
void cpldot(int iii) {  /* DOT or AVG; data-stack checks omitted */

    int  i, k, NROW, NCOL, XBASE, AROW, ACOL, ABASE, XXBASE ;
    ptr  p, q, r ;

    start=1 ; 
    if (ci>maxcomp) derror(90) ;
      
    L10 : p=getvar() ; if (errnum) derror(errnum) ;    /* get first vector */
    if (p==NULL) derror(70) ;
    i=p->valptr ; if (intval[i]!=1 || p->symtype!=-1) derror(84) ;
    NROW=intval[i+1] ;                            /* number of terms */
    XBASE=intval[i+2] ;                /* index base of first vector */   

    if (warea[gi++]!='*') derror(60) ;                    /* check * */

    COMPILE[ci++]=cpush0 ;           /* sum=0 */

    if (warea[gi]=='1') {                           /* a simple sum! */
	for (k=0; k<NROW ; k++) {
	    dptr[di++]=XBASE+k ;
	    COMPILE[ci++]=CADD ;
	    if (di>maxparam || ci>maxcomp) derror(90) ;
	    }         
	gi++ ; goto L20 ;
        }
         
    q=getvar() ; if (errnum) derror(errnum) ;        /* x or A next? */
    if (q==NULL || q->symtype!=-1) derror(70) ;
    if (di>maxparam-4 || ci>maxcomp-4) derror(90) ;
    i=q->valptr ;

    if (intval[i]==1) {                /* multiplier is only a vector */
	if (intval[i+1]!=NROW) derror(74) ;        /* not conformable */

	for (k=0; k<NROW ; k++) {
	    dptr[di++]=XBASE+k ;
	    COMPILE[ci++]=cpushvar ;
              
	    dptr[di++]=intval[i+2]+k ;           /* index 2nd vector */
	    COMPILE[ci++]=CMULT ; COMPILE[ci++]=cadd ;             
	    }         
       }
    /******* omitted in WINDOWS version **/
         
    else {                           /* multiplier is matrix*vector */
	if (intval[i]!=2) derror(84) ;        /* not a matrix, error */
	AROW=intval[i+1] ;  
	ACOL=intval[i+2] ; 
	ABASE=intval[i+3] ;                   
	if (warea[gi]=='*') {
            gi++ ;                                /* A*x term, skip * */
            if (AROW!=NROW) derror(74) ;           /* must match rows */
            r=getvar() ; if (errnum) derror(errnum) ;   /* get vector */
            if (r==NULL || r->symtype!=-1) derror(70) ;
            i=r->valptr ;
            if (intval[i]!=1 || intval[i+1]!=ACOL) derror(74) ; 
            XXBASE=intval[i+2] ;

            for (k=0; k<NROW ; k++) {
		dptr[di++]=XBASE+k ;
		COMPILE[ci++]=cpushvar ;
		dptr[di++]=ACOL-1 ;                 /* (1) sum to ACOL-1 */
		dptr[di++]=ABASE+k*ACOL ;          /* (2) index ROW base */    
		dptr[di++]=XXBASE ;              /* (3) 2nd vector index */    
		COMPILE[ci++]=cAIK ;              
		COMPILE[ci++]=cmult ; 
		COMPILE[ci++] =cadd ;                                         
	        }
            }  
                
        else if (warea[gi]=='%') {
	    gi++ ;                                            /* skip % */
	    if (ACOL!=NROW) derror(74) ;        /* ncol must match rows */
	    if (warea[gi++]!='*') derror(60) ;  /* check for * and skip */
           
	    r=getvar() ; if (errnum) derror(errnum) ;    /* get vector */
	    if (r==NULL || r->symtype!=-1) derror(70) ;
	    i=r->valptr ;
	    if (intval[i]!=1 || intval[i+1]!=AROW) derror(74) ; 
	    XXBASE=intval[i+2] ;          
	    for (k=0; k<NROW ; k++) {
               
		dptr[di++]=XBASE+k ;
		COMPILE[ci++]=cpushvar ;
                    
		dptr[di++]=AROW-1 ; /* (1) sum to nrow(A)-1 = ncol(A%)-1 */ 
		dptr[di++]=ACOL ;                   /* (2) $$$  */
		dptr[di++]=ABASE+ACOL*(AROW-1)+k ;    /* (3) column base */
		dptr[di++]=XXBASE ;                  /* (4) vector index */ 
		COMPILE[ci++]=cAKI ;    
		COMPILE[ci++]=cmult ; 
		COMPILE[ci++]=cadd ;  
	        }
	   }
      }

    L20 :   if (iii==2) {                                                        /* AVG */
          relval[dptr[di++]]=NROW ;                           /* get NROW */                 
          COMPILE[ci++]=CDIV ;              /*divide by NROW, NO pop */  
          }  
    
/** else if (warea[gi]=='+') { gi++ ; goto L10 ; }  no more terms! **/

    start=0 ;                                                 /* else sum is done */
           
  }                                                                    /* end cpldot */                               
                                                         

/* ------------------------------------------------------------------- */

void vardata(int i)   {     /*  compiles data pointers for a REAL variable */

    ptr  p ;

    p=getvar() ; if (errnum) derror(errnum) ;
    if (vartyp==0) derror(84) ;
    if (p==NULL) { if (!i) derror(200) ; p=instal() ; }
    if (p->symtype==1) dptr[di++]=p->valptr ;
    else if (p->symtype==-1)
	dptr[di++]=intval[p->valptr+intval[p->valptr]+1]+getsub(p) ;
    else derror(84) ;
    if (di>maxparam) derror(90) ;
    }


void arraydata() {        
    /* compiles data pointers for an array
       and also sets up counter and pointer */                              
    ptr  p ;
    
    if (di>maxparam-4) derror(90) ;
    start=1 ; p=getvar() ; start=0 ; if (errnum) derror(errnum) ;
    if (p==NULL) derror(70) ;
    if (p->symtype!=-1 || intval[p->valptr]!=1) derror(84) ;
    dptr[di++]=intval[p->valptr+1] ;             /* array dimension */
    dptr[di++]=intval[p->valptr+2] ;         /* address of 1st item */ 
    dptr[di]=dptr[di-2] ;                                /* counter */
    dptr[++di]=dptr[(di++)-2] ;                          /* pointer */
    }

void arraydata2(int i) {  /* compiles data pointers for an array 
                                    with either 1 dimension (i=0) or 2 dimensions */   
    ptr  p ;  

    start=1 ; p=getvar() ; start=0 ; if (errnum) derror(errnum) ;
    if (p==NULL) derror(70) ;
    if (p->symtype!=-1) derror(84) ;
       
    if (i==0) {                                    /* 1 dimension */
	if (intval[p->valptr]!=1) derror(84) ;
	if (di>maxparam-4) derror(90) ;
	dptr[di++]=intval[p->valptr+1] ;        /* array dimension */
	dptr[di++]=intval[p->valptr+2] ;    /* address of 1st item */  
                       }
    else {                                        /* 2 dimensions */
	if (intval[p->valptr]!=2) derror(84) ;
	if (di>maxparam-6) derror(90) ;          
	dptr[di++]=intval[p->valptr+1] ;    /* 1st array dimension */
	dptr[di++]=intval[p->valptr+2] ;    /* 2nd array dimension */          
	dptr[di++]=intval[p->valptr+3] ;    /* address of 1st item */
                        }
          }
       
/* ------------------------------------------------------------------ */

void cplexp(int ii)  {     /* expression compiler (for scalars or vectors) */

    int  prcnt, level, lj, lcount, opflg, first, sflg, gkk, iii ;
    int  ival, XBASE, XXBASE, XROW, ABASE, rflag ;
    double  sign, arg ;  
    ptr  p, q, pp ;

    prcnt=0 ; level=flevel ;         /* level stores initial flevel */
    opflg=FALSE ; 
       
    L15 :  first=1 ;                      /* first operand MUST be pushed! */
    warea[++wptr]=blank ;              /* initialize operator stack */

    if (warea[gi]=='-') {                        /* unary operator? */

	/***> */  COMPILE[ci++]=cpush0 ;                               
	if (ci>maxcomp) derror(90) ;
	goto L50 ;
                       }
    if (warea[gi]=='+') goto L60 ; goto L10 ; /* check for unary + */

/* ------------> push previously addressed operand; push next operator */

    L49 :                                          /* COMPILE operand push */
    /**>*/ COMPILE[ci++]=cpushvar ;                                                                          
    if (ci>maxcomp) derror(90) ;                                   

/* ------------------------------------------------------------------- */

    L50 :  warea[++wptr]=warea[gi] ;                      /* push operator */

    L60 :  gi++ ;                                    /* get a new operator */
    L10 :  if (warea[gi]=='(') { prcnt++ ; gi++ ; goto L15 ; }

/* ------------------------------------------------------------------- */
/*              this code compiles an operand push                     */
/* ------------------------------------------------------------------- */

    if (warea[gi]<'0' || warea[gi]>'9') {       /* is it a literal? */
	if (warea[gi]!='.') goto L31 ;             /* not a literal */
	arg=0 ; goto L11 ;                               /* fraction */
                       }
    arg=0 ; do {                     /* get integer part of literal */
	arg=arg*10+warea[gi++]-'0' ;
                       } while (warea[gi]>='0' && warea[gi]<='9') ;
    if (warea[gi]!='.') goto L20 ;
    L11 :  sign=1 ;                                        /* get fraction */
    L12 :  gi++ ;
    if (warea[gi]<'0' || warea[gi]>'9') goto L20 ;
    sign*=0.1 ;
    arg+=sign*(warea[gi]-'0') ;
    goto L12 ;

    L20 :  if (warea[gi]=='E' || warea[gi]=='e') {   /* deal with E-format */
	lcount=0 ;                             
	if (warea[++gi]=='+') sign=10 ;
	else if (warea[gi]=='-') sign=0.1 ;
	else derror(54) ;
	gi++ ;
	while (warea[gi]>='0' && warea[gi]<='9')
	    lcount=lcount*10+warea[gi++]-'0' ;
	for (lj=1 ; lj<=lcount ; lj++) arg*=sign ;
                       }                                                
    /* put the literal into relval and push the literal */
                                            
    relval[gir]=arg ; if (gir+1>=maxrlvar) derror(40) ;
    dptr[di++]=gir++ ; 
    if (first) {

	/***> */  COMPILE[ci++]=cpushvar ;

	if (ci>maxcomp || di>maxparam) derror(90) ;
	first=0 ; sflg=1 ;
                       }
    else sflg=0 ;
    goto L25 ;
       
/* ------------------------------------------------------------------- */
    L31 :                                      /* operand is not a literal */
	start=0 ; gkk=gi ; p=getvar() ;
	if (errnum==0) {                          /* scalar or function */
           
	    /* <------------------------------------------ */
                        
	    if (p==NULL) derror(200) ;                      /* undefined */
          
	    if (vartyp>0) {                      /* unsubscripted scalar */
		dptr[di++]=p->valptr ; 
		if (first) {
                 
		    /***> */        COMPILE[ci++]=cpushvar ;
                                                                  
		    if (ci>maxcomp || di>maxparam) derror(90) ;
		    first=0 ; sflg=1 ;
                                                 }
		else sflg=0 ;
	                       }                                                               
         
	    else if (vartyp<0) {            /* subscripted, COMPILE push */
		dptr[di++]=intval[intval[p->valptr]+
				  p->valptr+1]+getsub(p);            
		if (first) {

		    /***> */        COMPILE[ci++]=cpushvar ;
                              
		    if (ci>maxcomp || di>maxparam) derror(90) ;
		    first=0 ; sflg=1 ;
                                                 }
		else sflg=0 ;
	                       }

	    else {                            /* vartyp=0, it's a function */
		first=0 ; sflg=1 ;    /* a data item is already on the stack */
		gi++ ;                       /* skip ( ; note flevel++ later */
		if (p->valptr>0) {                  /* user-defined function */
		    p->SWITCH=FALSE ;          /* prevent recursion */
		    pp=p->next ;            /* pp points at first argument */
		    while (pp!=NULL && warea[gi]!=')') {
			if (pp->symtype!=1) derror(218) ;
			cplexp(ii) ;       /* COMPILE push argument */
			dptr[di++]=pp->valptr ;
			/****> **/      COMPILE[ci++]=cpop ;   /* pop */
			
			if (ci>maxcomp || di>maxparam) derror(90) ;
			if (warea[gi]==',') gi++ ;
			pp->SWITCH=TRUE ;  /* turn local variable ON */
			pp=pp->next ;
		                       }                                         /* END while */
		    if (pp!=NULL || warea[gi]!=')') derror(56) ;
		    lcount=gi ;      /* save return - no stack needed! */
		    gi=p->valptr ; cplexp(ii) ;  /* compile the function */
		    p->SWITCH=TRUE ;            /* undo recursion check */
		    gi=lcount ;           /* return from function compile */
		    pp=p->next ;          /* turn local variables OFF */
		    while (pp!=NULL) {
			pp->SWITCH=FALSE ; pp=pp->next ;
		                      }
		    }              /* if (p->valptr>0), user-defined function */
		else {         /* p->valptr<=0, library function */
		    if (p->valptr<=-150) derror(52) ;
		    flevel++ ;        /* we had opening parenthesis earlier */
                                      /* compile argument unless ... */
		    if (warea[gi]!=')' || p->valptr!=-11) cplexp(ii) ; 
		    else {              /* ... we need ran with push!! */
			COMPILE[ci++]=cran ;
			if (ci+5>maxcomp) derror(90) ;
			goto L113 ; 
		                       }
		    if (ci+5>maxcomp) derror(90) ;

		    		    switch(p->valptr) {

		  	    /*ln*/     
			case -1  : COMPILE[ci++]=cln ; break ;
			    /*sin*/    
			case -3  : COMPILE[ci++]=ccsin ; break ;
			    /*cos*/    
			case -4  : COMPILE[ci++]=cccos ; break ;
			    /*exp*/    
			case -5  : COMPILE[ci++]=ccexp ; break ;
			    /*abs*/    
			case -6  : COMPILE[ci++]=cfabs ; break ;
			    /*sqrt*/   
			case -7  : COMPILE[ci++]=ccsqrt ; break ;
			    /* 8 is trnc */
			    /*lim*/    
			case -9  : COMPILE[ci++]=clim ; break ;
			    /*sgn*/   
			case -10  : COMPILE[ci++]=csgn ; break ;
			    /*ran*/   
			case -11  : COMPILE[ci++]=cran1 ; break ;
			    /*atan*/  
			case -12  : COMPILE[ci++]=ccatan ; break ;
			    /*round*/ 
			case -13  : COMPILE[ci++]=cround ; break ;
	
					    /* ********* */
			    
			case -20  : COMPILE[ci++]=cclog ;    break ;
			    /*tan*/   
			case -21  : COMPILE[ci++]=cctan ; break ;
			    /*sinh*/  
			case -22  : COMPILE[ci++]=ccsinh ; break ;
			    /*cosh*/  
			case -23  : COMPILE[ci++]=cccosh ; break ;
			    /*acos*/  

			case -24  : COMPILE[ci++]=ccacos ; break ;
			    /*asin*/  
			case -25  : COMPILE[ci++]=ccasin ; break ;
			    /*tanh*/  
			case -26  : COMPILE[ci++]=cctanh ; break ;
			    /*atan2*/ 
			case -27  :
			    if (warea[gi++]!=',') derror(44) ;
			    /****> ****/           
			    if (ci+2>maxcomp) derror(90) ;
			    cplexp(ii) ;     /* push second argumentr */
			    COMPILE[ci++]=ccatanx ;
			    break ;
			    /*swtch*/ 
			case -28  : COMPILE[ci++]=cswtch ; break ;
			    /*SAT*/   
			case -29  : COMPILE[ci++]=cSAT ; break ; 
			    /* -30 not used here */
			    /*sat*/   
			case -31  : COMPILE[ci++]=csat ; break ;
			    /*deadz*/ 
			case -32  : COMPILE[ci++]=cdeadz ; break ;
			    /*deadc*/ 
			case -33  : COMPILE[ci++]=cdeadc ; break ;
			    /*rect*/  
			case -34  : COMPILE[ci++]=crect ; break ;
			    /*comp*/  
			case -35  :
			    /****> ****/    
			    if (ci+3>maxcomp) derror(90) ; 
                                                   /* for 2 extra argumts. */
			    if (warea[gi++]!=',') derror(44) ; 
			    /* push 2nd argument */
			    cplexp(ii) ;
			    if (warea[gi++]!=',') derror(44) ;  
			    /* push 3rd argument */
			    cplexp(ii) ;
			    COMPILE[ci++]=ccomp ;       
			    break ;
			    /*tri*/ 
			case -36  : COMPILE[ci++]=ctri ; break ;
			    /*sigmoid*/ 
			case -37  : COMPILE[ci++]=csigmoid ; break ;
			    /*SIGMOID*/ 
			case -38  : COMPILE[ci++]=csigsig ; break ;
			    /*sinc*/ 
			case -39  : COMPILE[ci++]=ccsinc ; break ;
			    /**** func1(x;Farray) ***/  
			case -41 :   
			    if (ci+2>maxcomp) derror(90) ;
			    if (di>maxparam-4) derror(90) ;
			    if (warea[gi]!=',' && warea[gi]!=';') 
				derror(25) ;  
			    gi++ ; 				
			    arraydata2(0) ;
			    /* locate the F array; get dimension, Xstart */
			    dptr[di-2]/=2 ;
			    /* dimension/2 */
			    dptr[di++]=dptr[di-1]+dptr[di-2] ;     
                                                   /* Ystart=Xstart+dimension/2 */
			    COMPILE[ci++]=cfunc1 ; 
			    break ;
			    /**** func2(x1,y2;Farray,X1array,X2array) ***/  
			case -42 : 
			    if (ci+2>maxcomp) derror(90) ;
			    if (di>maxparam-7) derror(90) ;
			    if (warea[gi++]!=',') derror(25) ;          
                                                   /* push 2nd argument */
			    cplexp(ii) ;
			    if (warea[gi]!=',' && warea[gi]!=';') 
				derror(25) ;  gi++ ; 
			    arraydata2(1) ;  
			    /* locate the F array; get N1, N2, Fstart */ 
			    if (warea[gi++]!=',') derror(25) ;  
			    arraydata2(0) ;	
			    /* locate the X array, N1, X1start */
			    if (warea[gi++]!=',') derror(25) ;  
			    arraydata2(0) ;	
			    /* locate the Y array, N2, X2start */
			    if (dptr[di-4] != dptr[di-7]  || dptr[di-2] != dptr[di-6]) derror(84) ; 
			    /* N1, N2 must match */        
			    dptr[di-4]+=dptr[di-3] ;
			    /* X1end=X1start+N1 */
			    dptr[di-2]+=dptr[di-1] ; 
			    /* X2end=X2start+N2 */          
			    COMPILE[ci++]=cxfunc2 ; 
			    break ;
			    
			    /* gauss*/   
			case -43 : COMPILE[ci++]=cgauss ; break ;	/** &&& <***/
			    
			default : strcpy(symbol,p->name); derror(200) ;
		                       }										         /* end switch */
		    
		    L113 :      if (warea[gi]!=')') derror(7) ;  
                                                 /* missing ) */
		    flevel-- ;            
		    }              /* else p->valptr<=0, library function */
		gi++ ;                           /* skip ')' */
	                       }
	    /* END else vartyp=0, function */
                                            
/* function is on the stack now, and flevel=level */         
        
	       }                        /* END if errnum=0, scalar or function */

	/* ---- VECTOR COMPILATION ------  push vector element ----------- */
	else {
	    if (errnum!=73) derror(errnum) ;    /* true error */
	    if (ii==0) derror(233) ;        /* we need a Vector expression */
	    /*** shiftflag=0 ; ***/   
	    /* (killed) shiftflag for repeated calls */
    	    errnum=0 ; gi=gkk ;  start=1 ;  
	    /* try to get vector item */
	    p=getvar() ; if (errnum) derror(errnum) ;      
	    if (p==NULL || p->symtype!=-1) derror(70) ; 
	    /* not a defined vector */
	    iii=p->valptr ;
	    if (intval[iii]>2) derror(75) ;
	    /* neither vector nor matrix */
	    XROW=intval[iii+1] ;   
	    /* number of vector or matrix rows */
	    XBASE=intval[iii+2] ; 
	    /* vector base index if simple vector operand */
	    /* %%----------- check for A*x, A%*x, or pattern matrix --- */
    	    if (intval[iii]==2) {
		/* operand starts with a recctangular matrix */
		ABASE=intval[iii+3] ;
		/* index to matrix base */
		if (di>maxparam-3) derror(90) ;
		/* XBASE is column number or N, XROW is row number of A */     
		if (warea[gi]=='*') {
		    gi++ ;         /* A*x term, skip * *//** changed! <**/
		    if (XROW!=VROW) derror(74) ; 
		    /* must match result rows */    
		    dptr[di++]=XBASE-1 ; 
		    /* (1) sum to ncol-1 = XBASE-1 */
		    /* skip * */
		    dptr[di++]=ABASE+ii*XBASE-1 ;  
		    /* (2) index ROWEND */
		    q=getvar() ; if (errnum) derror(errnum) ;
		    /* get the vector */
		    if (q==NULL || q->symtype!=-1) derror(70) ;
		    iii=q->valptr ;
		    if (intval[iii]!=1 || intval[iii+1]!=XBASE) derror(74) ; 
		    XXBASE=intval[iii+2] ;   
		    if (XXBASE==VBASE && VBASE>maxnsv) derror(85) ;
		    /* illegal recursion */
		    dptr[di++]=XXBASE+XBASE-1 ;
		    /* (3) vector XEND index */        
		    rflag=-1 ;
		    }                    
		else if (warea[gi]=='%') {           
		    gi++ ;                    /* skip % */
		    if (warea[gi++]!='*') derror(60) ; 
		    /* check for * and skip */
		    if (XBASE!=VROW) derror(74) ;
		    /* ncol must match result rows */    
		    dptr[di++]=XROW-1 ; 
		    /* (1) sum to nrow(A)-1 = ncol(A%)-1 */ 
		    dptr[di++]=VROW ; 
		    /* (2) VROW */
		    dptr[di++]=ABASE+VROW*(XROW-1)+ii-1 ;  
		    /* (3) COLEND */
		    q=getvar() ; if (errnum) derror(errnum) ;
		    /* get the vector */
		    if (q==NULL || q->symtype!=-1) derror(70) ;
		    iii=q->valptr ;
		    if (intval[iii]!=1 || intval[iii+1]!=XROW) derror(74) ;
		    XXBASE=intval[iii+2] ;   
		    if (XXBASE==VBASE && VBASE>maxnsv) derror(85) ;
		    /* illegal recursion */
		    dptr[di++]=XXBASE+XROW-1 ;     
		    /* (4) vector XEND index */
		    if (di>maxparam) derror(90) ;                             
		    rflag=1 ;
	                           }
       		else if (warea[gi]=='#') {
		    /* pattern-row matrix */
		    gi++ ;  
		    /* skip # */        
		    if (XBASE!=VROW) derror(74) ;
		    /* must match result rows */
		    dptr[di++]=XROW ; 
		    /* (1) XROW is number N of patterns! */          
		    dptr[di++]=VROW ; 
		    /* (2) vector dimension */
		    dptr[di++]=ABASE+ii-1 ;  
		    /* (3) indexes matrix column  */
		    rflag=2 ;
		    }
		else derror(75) ;       /* syntax error */    
	                       }                           /* END term has recctangular matrix  */
	    
/* ------------------------------------------------------------------ */
	    else {      /* must be a simple vector, is it shifted? */
		if (XROW!=VROW) derror(74) ; 
		/* must match result rows */             
		rflag=0 ;  
		/* in case it is not shifted */
       		if (warea[gi]=='{') {  
		    /* index shift */
		    /***  shiftflag=1 ; ***/  
		    /* killed, marks need to recompile */
		    gi++ ;               /* skip the bracket */
		    start=0 ;            /* evalexp deals with scalar! */
		    ival=evalexp() ; if (errnum) derror(errnum) ;
		    start=1 ;
		    if (ival>0) {                     
			if (XBASE==VBASE && ! ddtflag) derror(85) ;
			/* illegal recursion! */
			if (ival>=maxint) derror(20) ;
		                      }
		    else if (ival<=-maxint) derror(20) ;      
		    if (warea[gi++]!='}') derror(25) ;    
		    /* skip bracket */
		    if (ii+ival>VROW || ii+ival<=0) rflag=3 ;
		    /* out of range */          
		    XBASE=XBASE+ival ; 
		    /* shift index */                          
		    }   
		/* if {, index shift */
	                      }                  /* END else vector */
     
/* ----------------------------------------------------------------- */
	    if (!rflag) { 
		/* push simple vector, may be shifted if in-range */
		dptr[di++]=XBASE+ii-1 ;  
		if (di>maxparam) derror(90) ;                          
		if (first) {     
		    /* vector or shifted vector, range ok */
		    /**> */  COMPILE[ci++]=cpushvar ;
		    if (ci>maxcomp) derror(90) ;
		    first=0 ; sflg=1 ;
		    }
		else sflg=0 ;
	                       }                               /* END vector item, go  to L25 */
      
/* --------------------------------- */

	    else {  
		/* push A*x, A%*x, or out-of-range shifted vector */
		if (rflag<0){                            /* A*x */
		    /***> */ COMPILE[ci++]=cAIK ;                
		    }                     
		else if (rflag==1){                       /* A%*x */
		    /***> */ COMPILE[ci++]=cAKI ;      
		    }                           
		else if (rflag==2){            /* pattern-row matrix */ 
		    /***> */ COMPILE[ci++]=cPATRN ; 
		    }
		else {              /* out of shift range, push 0 */ 
		    /***> */ COMPILE[ci++]=cpush0 ; 
		    }      
		if (ci>maxcomp-1) derror(90) ;                        
		if (first) { first=0 ; sflg=1 ; }
		/* item is already on stack */      
		else {
		    /* pop into into a relval location */
		    sflg=0 ;  
		    dptr[di++]=gir ; if (gir+2>=maxrlvar) derror(40) ;
                    
		    /***> */  COMPILE[ci++]=cpop ;
		    if (di>maxparam-1 || ci>maxcomp-1) derror(90) ;
                          
		    dptr[di++]=gir++ ; 
		    /* address for subsequent cADD etc. */
		    }   /* note cpop, cpush use different addressing! */     
	             }                                             /* END if (rflag) */
       
	    start=0 ;                                                 
	    }                                          /* END push vector item */
/* ------------------------------------------------------------------- */

	L25 :  if (warea[gi]!='+' && warea[gi]!='-') goto L30 ;
	if (warea[wptr]==blank) {            /* push low-prec. operator */
	    if (sflg) goto L50 ; goto L49 ;
	    }
	if (sflg) goto L42 ;
	L41 :  switch(warea[wptr]) {                    /* !sflg */
	    case '+' : COMPILE[ci++]=CADD ; break ;
	    case '-' : COMPILE[ci++]=CSUB ; break ;
	    case '*' : COMPILE[ci++]=CMULT ; break ;
	    case '/' : COMPILE[ci++]=CDIV ; break ;
	    default     : COMPILE[ci++]=CPWR ; break ;          /* X^Y */
	    }                                                  /* switch */

	if (ci>maxcomp) derror(90) ;
	if (warea[--wptr]==blank) {
	    if (opflg) goto L70 ; goto L50 ;
	    }                                             /* else go on! */

	L42 :  do {        /* loop until operator stack is empty */

	    switch(warea[wptr]) {

                case '+' : COMPILE[ci++]=cadd ; break ;
                case '-' : COMPILE[ci++]=csub ; break ;
                case '*' : COMPILE[ci++]=cmult ; break ;
                case '/' : COMPILE[ci++]=cdiv ; break ;

                default     : COMPILE[ci++]=cpwr ; break ;        /* X^Y */
	         }                                                /* switch */

	    if (ci>maxcomp) derror(90) ;
          
	    } while (warea[--wptr]!=blank) ;

	if (opflg) goto L70 ; goto L50 ;

	L30 :  if (!sflg) {
	    if (warea[gi]=='*' || warea[gi]=='/') {
		if (warea[wptr]!=blank) {
                 
		    if (warea[wptr]=='*') COMPILE[ci++]=CMULT ;
		    else if (warea[wptr]=='/') COMPILE[ci++]=CDIV ;
		    else if (warea[wptr]=='^') COMPILE[ci++]=CPWR ;
		    else goto L49 ;

		    if (ci>maxcomp) derror(90) ;
		    wptr-- ;
                        }                                                /* if */
                                                                 
		if (warea[wptr]!=blank) goto L322 ; goto L50 ;
	                       }                                            /* if * or / */
             
	    if (warea[gi]=='^') goto L49 ;
	    if (warea[wptr]!=blank) { opflg=TRUE ; goto L41 ; }
	    goto L70 ;
	    }                                              /* if (!sflg) */

	if (warea[gi]!='*' && warea[gi]!='/') goto L323 ;
	L321 : if (warea[wptr]==blank) goto L50 ;
	/*  operator stack is done */
	/* loop */
	L322 : if (warea[wptr]=='*') COMPILE[ci++]=cmult ;     
	else if (warea[wptr]=='/') 
	    COMPILE[ci++]=cdiv ;                                           
	else if (warea[wptr]=='^') 
	    COMPILE[ci++]=cpwr ;             
	else goto L50 ;
	if (ci>maxcomp) derror(90) ;
	wptr-- ;
	goto L321 ;                                             /* loop */
	L323 : if (warea[gi]=='^') goto L50 ;
	if (warea[wptr]!=blank) { opflg=TRUE ; goto L42 ; }
	L70 :  wptr-- ;       /* pop operator from operator stack */
	if (warea[gi]==')') {
	    if (level==flevel && prcnt!=0) {   
		prcnt-- ;
		gi++ ;
		opflg=FALSE ;
		sflg=1 ;      /* a data item is on stack! */
		goto L25 ;
	                       }
	    }                                       /* if ')' */
	if (prcnt!=0) derror(7) ;
                       }                                               /* cplexp */

/* ------------------------------------------------------------------- */
void cplarray(int k)  /* VECTOR, UPDATE, MAT d/dt are OBSOLETE, still work */

/* k=0: VECTOR; k=1: UPDATE; k=2: MAT d/dt; k=3: PLEARN; k=4: CLEARN;
   k=5: INTP; k=-1: DELTA; k=-2: MATRIX */
{
    
    int  i,gkk,kf,addflag,tflag,muflag,maxflag,minflag,NROW,NCOL,XCOL ;
    int  matrixflag ;
    double  ival ;  ptr  q, pp ;

    start=1 ;                                    /* makes vartyp=-1 */
    p=getvar() ; if (errnum) derror(errnum) ;          /* get array */
    if (p==NULL) derror(70) ;
    i=p->valptr ;
    if (ci>maxcomp-2 || di>maxparam-3) derror(90) ;
    dptr[di++]=NROW=intval[i+1] ;                /* was addrss=NROW */
    dptr[di++]=NCOL=intval[i+2] ;       /* was addr1, YBASE or NCOL */

    if (k<0) {                                   /* MATRIX or LEARN */
	if (p->symtype!=-1 || intval[i]!=2) derror(84) ;
	dptr[di++]=intval[i+3] ;                   /* was addr3=ABASE */
	COMPILE[ci++]=cainit ;     /* start sum loop, save return pt. */
	if (warea[gi++]!='=') derror(29) ;                 /* skip = */
           
	if (warea[gi]=='[') {                   /* read mask, if any */
	    gi++ ;                                          /* skip [ */
	    pp=getvar() ; if (errnum) derror(errnum) ;
	    if (pp==NULL || pp->symtype!=-1) derror(70) ;
	    i=pp->valptr ;
	    if (intval[i+1]!=NROW || intval[i+2]!=NCOL) derror(74) ;
	    dptr[di++]=intval[i+3] ;           /* was xinadr=maskbase */
	    if (warea[gi++]!=']') derror(25) ;              /* skip ] */
	    COMPILE[ci++]=cmaskq ;
	    if (ci>maxcomp || di>maxparam) derror(90) ;
	}                                           /* if [, mask */
             
    }                                 /* if k<0, MATRIX or DELTA */
 
    else {  /* k>=0, VECTOR, UPDATE, MAT d/dt, PLEARN, CLEARN, INTP */

	if (p->symtype!=-1 || intval[i]!=1 || k==2 && intval[i+2]>maxnsv)
	    derror(84) ;   /* need array and, for MAT d/dt, STATE */

	if (warea[gi]=='^') {                     /* winner-take-all */
	    if (k>1) derror(76) ;
	    gi++ ;                                          /* skip ^ */
	    maxflag=TRUE ;
	}
	else maxflag=FALSE ;

	if (warea[gi++]!='=') derror(29) ;                 /* skip = */
	COMPILE[ci++]=cyinit ;                  /* start vector loop */

	if(k>=3) {                        /* PLEARN, CLEARN, or INTP */
	    pp=getvar() ; if (errnum) derror(errnum) ;       /* get A */
	    if (pp==NULL || pp->symtype!=-1) derror(70) ;
	    i=pp->valptr ;

	    if (warea[gi]=='(' && k==4) goto L3 ;           /* CLEARN */
	    if (warea[gi]!='*') derror(60) ;
	    L3 :         gi++ ;

	    if (intval[i]!=2 || intval[i+1]!=NROW) derror(74) ;
	    dptr[di++]=XCOL=intval[i+2] ;      /* was addr3=XCOL of A */
	    if (ci>maxcomp-1 || di>maxparam-4) derror(90) ;
	    dptr[di++]=intval[i+3] ;             /* was xoutadr=ABASE */
	    q=getvar() ; if (errnum) derror(errnum) ;
	    if (q==p) derror(85) ;               /* illegal recursion */
	    if (q==NULL || q->symtype!=-1) derror(70) ;
	    i=q->valptr ;
	    if (intval[i]!=1 || intval[i+1]!=XCOL) derror(74) ;
	    dptr[di++]=intval[i+2] ;               /* was addr2=XBASE */

	    if (warea[gi]==')' && k==4) goto L4 ;          /* CLEARN */
	    if (warea[gi]!=';') derror(25) ;
	    L4 :         gi++ ;              /* skip ) or ; */

	    start=0 ;                          /* get lrate or thresh */
	    pp=getvar() ; if (errnum) derror(errnum) ;
	    if (pp==NULL) derror(70) ;
	    if (pp->symtype==1) dptr[di++]=pp->valptr ;
	    else if (pp->symtype==-1)
		dptr[di++]=intval[pp->valptr+intval[pp->valptr]+1]+getsub(pp) ;
	    else derror(70) ;

	    if (k==3) COMPILE[ci++]=cplearn ;    /* PLEARN y=A*x;rate */

	    else if (k==4) {                /* CLEARN y=A(x)rate,crit */
                if (warea[gi++]!=',') derror(25) ;       /* skip comma */
                pp=getvar() ; if (errnum) derror(errnum) ; /* get crit */
                if (pp==NULL) derror(70) ;
                if (pp->symtype==1) dptr[di++]=pp->valptr ;
                else if (pp->symtype==-1)
     
		    dptr[di++]=intval[pp->valptr+
				      intval[pp->valptr]+1]+getsub(pp) ;

                else derror(70) ;

                if (warea[gi]=='#') { gi++ ; dptr[di++]=1 ; }
                else dptr[di++]=-1 ;         /* checks fast learn mode */
 
                COMPILE[ci++]=cclearn ;
	    }                                            /* CLEARN */

	    else COMPILE[ci++]=cbarf ;           /* INTP y=A*x;thresh */
	    if (ci>maxcomp || di>maxparam) derror(90) ;
	    start=0 ; return ;
	}                          /* k>2, PLEARN, CLEARN or INTP */
    }                                  /* else k>=0, VECTOR etc. */

/* compile successive terms of a sum of product terms. Product
   factors can be of various different types. */

    muflag=FALSE ;                                      /* no * yet */
    if (warea[gi]!='-') addflag=0 ;                /* no + or - yet */
    else {                                    /* leading minus sign */
	gi++ ;                                            /* skip -  */
	addflag=-1 ;                   /* mark for later subtraction */
	COMPILE[ci++]=cpush0 ;                             /* push 0 */
	if (ci>maxcomp) derror(90) ;
    }

    L10 :  if (warea[gi]=='(') {                   /* primitive difference */
	gi++ ; kf=-30 ; goto L11 ;                        /* skip ( */
    }
    start=0 ;                  /* try for scalar, or array function */
    kf=0 ; gkk=gi ; pp=getvar() ;
    if (errnum==0) {                          /* scalar or function */
	if (pp==NULL) derror(59) ;
	if (pp->symtype==1) {              /* scalar; factor is done */
	    dptr[di++]=pp->valptr ;   /* was addr2, address of scalar */
	    COMPILE[ci++]=cpushvar ;
	    if (ci>maxcomp || di>maxparam) derror(90) ;
	    goto L20 ;
	}
	if (pp->symtype==-1) {                 /* subscripted scalar */
	    dptr[di++]=intval[pp->valptr+intval[pp->valptr]+1]+getsub(pp) ;
	    COMPILE[ci++]=cpushvar ;
	    if (ci>maxcomp || di>maxparam) derror(90) ;
	    goto L20 ;
	}

	if (pp->symtype!=0) derror(200) ;          /* not a function */
	gi++ ;                                   /* function, skip ( */
	if ((kf=pp->valptr)!=-11) goto L11 ;       /* array function */
	COMPILE[ci++]=cran ;                   /* random-noise array */
	if (ci>maxcomp) derror(90) ;
	if (warea[gi++]!=')') { if (warea[gi++]!=')') derror(7) ; }
	/* check ) without or with dummy argument */
	goto L20 ;                          /* random factor is done */
    }                                             /* if errnum=0 */
    else if (errnum!=73) derror(errnum) ;            /* true error */

    /* vector or matrix*vector, retry getvar with start=1 */

    errnum=0 ; gi=gkk ;            /* retry getvar() for array type */

    L11 :  start=1 ; q=getvar() ; if (errnum) derror(errnum) ;
    if (q==NULL || q->symtype!=-1) derror(70) ;
    i=q->valptr ;

    if (k>=0) goto L12 ;             /* VECTOR, MAT d/dt, or UPDATE */

/* --------------------------------------------------- MATRIX or DELTA */

    if (intval[i+1]!=NROW) derror(74) ;          /* not conformable */
    matrixflag=1 ;

    if (intval[i]==2) {                        /* simple array term */
	if (intval[i+2]!=NCOL) derror(74) ;       /* not conformable */
	dptr[di++]=intval[i+3] ;                  /* was addr2=ABASE */
	COMPILE[ci++]= cpusharr ;
	if (ci>maxcomp || di>maxparam) derror(90) ;
	goto L25 ;
    }
    else if (intval[i]!=1) derror(84) ;         /* need vector pair */
    dptr[di++]=intval[i+2] ;        /* was addr2=base of 1st vector */
    if (ci>maxcomp-1 || di>maxparam-1) derror(90) ;

    if (warea[gi]=='*') { gi++ ; minflag=0 ; }     /* outer product */
    else if (warea[gi]=='&') {gi++ ; minflag=1 ; }      /* min(x,y) */
    else derror(60) ;

    q=getvar() ; if (errnum) derror(errnum) ;       /* get vector 2 */
    if (q==NULL) derror(70) ;
    i=q->valptr ;
    if (intval[i+1]!=NCOL) derror(74) ;          /* not conformable */
    dptr[di++]=intval[i+2] ;       /* was xinadr=base of 2nd vector */
    if (!minflag) COMPILE[ci++]=cpushv2 ;  /* push, multiply  vectors */
    else COMPILE[ci++]=cmin ;                           /* min(x,y) */
    goto L25 ;                             /* MATRIX factor is done */

/* --------------------------------------- VECTOR, MAT d/dt, or UPDATE */

    L12 :  matrixflag=0 ;
    if (intval[i]==1) {                           /* it is a vector */
	if (intval[i+1]!=NROW) derror(74) ;       /* not conformable */
	dptr[di++]=intval[i+2] ;                  /* was addr2=XBASE */
	if (ci>maxcomp-1 || di>maxparam) derror(90) ;
	if (warea[gi]=='{') {                         /* index shift */
	    gi++ ;                                    /* skip bracket */
             
	    /***   shiftflag=1 ; ***/         /* marks need to recompile */
          
	    start=0 ;                   /* evalexp deals with scalar! */
	    ival=evalexp() ; if (errnum) derror(errnum) ;
	    start=1 ;
	    if (ival>0) {                 /* watch illegal recursion! */
                if (k<2 && q==p) derror(85) ;
                if (ival>maxint) derror(20) ;
	    }
	    else if (ival<-maxint) derror(20) ;
	    dptr[di++]=(int)ROUND(ival) ;    /* was addr4=index shift */
	    if (di>maxparam) derror(90) ;
	    if (warea[gi++]!='}') derror(25) ;        /* skip bracket */
	    COMPILE[ci++]=cconv ;
	}                                    /* if {, convolution */
	else COMPILE[ci++]=cpusharr ;               /* simple vector */
                }                                                  /* vector */

    else {               /* term is matrix*vector or pattern matrix */
	if (intval[i]!=2) derror(84) ;               /* not a matrix */
	if (ci>maxcomp-1 || di>maxparam-3) derror(90) ;

	if (warea[gi]!='*') {         /* pattern matrix or transpose */
	    if (intval[i+2]!=NROW) derror(74) ;    /* not conformable */
	    dptr[di++]=XCOL=intval[i+1] ;  /* was addr3=nrow of trans-
					      pose, or number N of patterns */
	    dptr[di++]=intval[i+3] ;             /* was xoutadr=ABASE */

	    if (warea[gi]=='#') {                   /* pattern matrix */
                              gi++ ;                                       /* skip # */
                              COMPILE[ci++]=cpatrn ;
                              goto L25 ;                                   /* skip # */
	       }
	    if (warea[gi++]!='%') derror(60) ;      /* missing % or * */
	    tflag=TRUE ;                                 /* transpose */
	    }                           /*pattern matrix or transpose */

	else {                                           /* A*x term */
	    if (intval[i+1]!=NROW) derror(74) ;    /* not conformable */
	    dptr[di++]=XCOL=intval[i+2] ;           /* was addr3=XCOL */
	    dptr[di++]=intval[i+3] ;             /* was xoutadr=ABASE */
	    tflag=FALSE ;
	    }                                         /* no transpose */
	if (warea[gi++]!='*') derror(60) ;                /* check * */

	q=getvar() ; if (errnum) derror(errnum) ;      /* get vector */
	if (q==NULL || q->symtype!=-1) derror(70) ;
	if (k<2 && q==p) derror(85) ;           /* illegal recursion */
	i=q->valptr ;
	if (intval[i]!=1 || intval[i+1]!=XCOL) derror(74) ;
	dptr[di++]=intval[i+2] ;                  /* was addr2=XBASE */
	if (!tflag) COMPILE[ci++]=caik ;      /* push matrix product */
	else COMPILE[ci++]=caki ;
    }                                           /* matrix*vector */

/* ------------------------------------------------------------------- */
    L25 :  if (kf!=0) {               /* COMPILE vector or matrix function */

	if (warea[gi++]!=')') {  /* argument is primitive dif or sum */
	    /* (vector - + ... ) */

	    start=0 ; gkk=gi ; p=getvar() ;         /* try for scalar */
	    if (errnum) {                 /* try for vector or matrix */
               		 if (errnum!=73) derror(errnum) ;         /* true error */
               		 errnum=0 ; gi=gkk ;
               		 start=1 ; p=getvar() ; if (errnum) derror(errnum) ;
             		 if (p==NULL || p->symtype!=-1) derror(70) ;
          		 i=p->valptr ;
		/***> */        
		if (!matrixflag) {                           /* vector */
		    if (intval[i]!=1 || intval[i+1]!=NROW) derror(74) ;
		    dptr[di++]=intval[i+2] ;
		     }
               	                       else {                                      /* matrix? */
		    if (intval[i]!=2 || intval[i+1]!=NROW
			|| intval[i+2]!=NCOL) derror(74) ;
		    dptr[di++]=intval[i+3] ;
		     }
              	  COMPILE[ci++]=cpusharr ; if (ci>maxcomp) derror(90) ;
              	  if (warea[gkk-1]=='-') COMPILE[ci++]=csub ;
               	 else if (warea[gkk-1]=='+') COMPILE[ci++]=cadd;
               	 else derror(44) ;
              	  }

                else {                                          /* scalar */
                	if (p==NULL) derror(70) ;
               	if (p->symtype==1) dptr[di++]=p->valptr ;
              	else if (p->symtype==-1) 
		dptr[di++]=intval[p->valptr+intval[p->valptr]+1]+getsub(p) ;
                else derror(70) ;
                if (warea[gkk-1]=='-') COMPILE[ci++]=CSUB ;
                else if (warea[gkk-1]=='+') COMPILE[ci++]=CADD;
                else derror(44) ;
                }

         if (ci>maxcomp || di>maxparam) derror(90) ;
         if (warea[gi++]!=')') derror(7) ;            /* must be ) */
         }

   switch(kf) {
	    /*ln*/   
	    case   -1  : COMPILE[ci++]=cln ; break ;
		/*sin*/  
	    case   -3  : COMPILE[ci++]=ccsin ; break ;
		/*cos*/  
	/* ------------------------------------------------------------------- */    case   -4  : COMPILE[ci++]=cccos ; break ;
		/*exp*/  
	    case   -5  : COMPILE[ci++]=ccexp ; break ;
		/*abs*/  
	    case   -6  : COMPILE[ci++]=cfabs ; break ;
		/*sqrt*/ 
	    case   -7  : COMPILE[ci++]=ccsqrt ; break ;
		/* -8 is trnc */
		/*lim*/  
	    case   -9  : COMPILE[ci++]=clim ; break ;
		/*sgn*/  
	    case  -10  : COMPILE[ci++]=csgn ; break ;
		/* -11 is ran */
		/*atan*/ 
	    case   -12  : COMPILE[ci++]=ccatan ; break ;
		/*round*/
	    case   -13  : COMPILE[ci++]=cround ; break ;
		/*log*/  
	    case   -20  : COMPILE[ci++]=cclog ; break ;
		/*tan*/  
	    case   -21  : COMPILE[ci++]=cctan ; break ;
		/*sinh*/ 
	    case   -22  : COMPILE[ci++]=ccsinh ; break ;
		/*cosh*/  
	    case  -23  : COMPILE[ci++]=cccosh ; break ;
		/*acos*/  
	    case  -24  : COMPILE[ci++]=ccacos ; break ;
		/*asin*/  
	    case  -25  : COMPILE[ci++]=ccasin ; break ;
		/*tanh*/ 
	    case/* ------------------------------------------------------------------- */   -26  : COMPILE[ci++]=cctanh ; break ;
		/* -27 is atan2 */
		/*swtch*/ 
	    case   -28  : COMPILE[ci++]=cswtch ; break ;
		/*SAT*/ 
	    case   -29  : COMPILE[ci++]=cSAT ; break ;
		/*difr*/ 
	    case   -30  : break ;
		/*sat*/  
	    case   -31  : COMPILE[ci++]=csat ; break ;
		/*deadz*/ 
	    case   -32  : COMPILE[ci++]=cdeadz ; break ;
		/*deadc*/ 
	    case   -33  : COMPILE[ci++]=cdeadc ; break ;
		/*rect*/ 
	    case   -34  : COMPILE[ci++]=crect ; break ;
		/* -35 is comp */
		/*tri*/ 
	    case   -36  : COMPILE[ci++]=ctri ; break ;
		/*sigmoid*/ 
	    case   -37  : COMPILE[ci++]=csigmoid ; break ;
		/*SIGMOID*/ 
	    case   -38  : COMPILE[ci++]=csigsig ; break ;
		/*sinc*/ 
	    case   -39  : COMPILE[ci++]=ccsinc ; break ;
		/*recip*/ 
	    case   -40  : COMPILE[ci++]=crecip ; break ;
	    
	    case -43 : COMPILE[ci++]=cgauss ; break ;	/** &&& <***/

	    default : strcpy(symbol,pp->name) ; derror(200) ;
	    }                                                                   /* end switch */
	    
    if (ci>maxcomp) derror(90) ;
    }                              /* kf!=0, function */

/* -----------------------------------  now a factor has been compiled */

    L20 :  if (ci>maxcomp-1) derror(90) ;
    if (muflag)   COMPILE[ci++]=cmult ;      /* there was a preceding *   */
	
    if (warea[gi]=='*') {
	gi++ ; muflag=TRUE ;              /* skip * ; another factor */
	goto L10 ;
                       }
    muflag=FALSE ;                     /* was last factor in a term */

    if (addflag>0) COMPILE[ci++]=cadd ;
    else if (addflag<0) COMPILE[ci++]=csub;       /* preceding +,-  */

    if (warea[gi]=='+') {                     /* a sum term is done */
	gi++ ; addflag=1 ;                  /* skip + ; another term */
	goto L10 ;
                       }
    else if (warea[gi]=='-') {                /* a sum term is done */
	gi++ ; addflag=-1 ;                 /* skip - ; another term */
	goto L10 ;
                       }

/*  now the sum is compiled; get min,max, if any, and finish the loop  */

    start=0 ;                         /* restore getvar() for scalars */
    if (warea[gi]==';') {                        /* must get min, max */
        gi++ ;                                       /* skip semicolon */

        cplexp(0) ;                                             /* min */
        if (warea[gi++]!=',') derror(44) ;     /* check and skip comma */

        cplexp(0) ;                                              /* max */

        if (k==0 || k==-2) COMPILE[ci++]=cyendli ;   /* VECTOR, MATRIX */
        else if (k==2) derror(76) ;                        /* MAT d/dt */
        else COMPILE[ci++]=cupdatli ;                 /* UPDATE, DELTA */
       }                                                              /* if semicolon */
    else {                                               /* no bounds */
        if (k==0 || k==-2) COMPILE[ci++]=cyend ;     /* VECTOR, MATRIX */
        else if (k==2) COMPILE[ci++]=cdend ;               /* MAT d/dt */
        else COMPILE[ci++]=cupdat ;                   /* UPDATE, DELTA */
        if ((k==0 || k==1) && maxflag) {
	    if (ci>maxcomp) derror(90) ; COMPILE[ci++]=cmax ;
	    }
        }
    if (ci>maxcomp) derror(90) ;
    }                                                     /* cplarray */
/* ------------------------------------------------------------------- */

    /* COMPILER PASS 1 */

L999 :    if (warea[gi]!='*') disk=TRUE ;                 /* stash ON/OFF */
    else { disk=FALSE ; gi++ ; }
    gisav=gi ;                /* save location following drun or *  */

    if (warea[gi]!=lf && warea[gi]!='|') {           /* labeled simulation run */
	goto00(1) ;                              /* to lf past label */
	cflag=TRUE ;              /* cflag marks "label" compilation */
	do gisav++ ;                  /* go to end of drun statement */
	while (warea[gisav]!=lf && warea[gisav]!='|') ;
	cstart=gi ;    /* cstart on lf past label, start compilation */
	gi+=4 ;                              /* start on first token */
	if (!runf) derror(207) ;
	if (dynlin==0) derror(215) ;                  /* no DYNAMIC */
	goto L8 ;
                       }

    cflag=FALSE ;                      /* not a "label" compilation */
    if (xlength) return ;                      /* already compiled */

    if (!runf) derror(207) ;
    if (dynlin==0) derror(215) ;                      /* no DYNAMIC */

    while (warea[gi]!=lf) gi++ ; gi=gi+4 ;   /* find token after lf */
    do {          /* get cstart behind DYNAMIC to start compilation */
	if (warea[gi]==vdynmc) {             /* on lf behind DYNAMIC */
	    cstart=gi+1 ; gi=gi+5 ; goto L8 ;
	    }
	gi+=warea[gi-1] ;                 /* advance by a whole line */
                       } while (gi<endtxt) ;                    /* cstart is on lf */
    derror(215) ;                        /* no DYNAMIC - redundant? */

    /* now find all d/dt; define new state variables, if any */

    L7 :   do gi++ ; while (warea[gi]!=lf && warea[gi]!='|') ;
    if (warea[gi]==lf) {               /* find start of a statement */
	gi+=4 ; if (gi>=endtxt) goto L10 ;                  
                      }
    else gi++ ;                                         /* it was | */

    L8 :   if (warea[gi]==vlabel || warea[gi]==vstop) goto L10 ;
    if ( warea[gi]!=vddt) goto L7 ;              /* next statement */

    gi++ ;                     /* found a state variable, skip d/dt */
    qqq=getvar() ; if (errnum) derror(errnum) ;       /* check name */
       
    if (qqq!=NULL) {               /* already defined; must be REAL */
          
	if (qqq->symtype==1) {                      /* unsubscripted */
	    if (qqq->valptr<=maxnsv) goto L7 ;   /* already in STATE */    
                                    
	    if (++nn1>maxnsv) derror(100) ;         /* else count... */
	    if ((size>8)&&(nn1>EQUMAX)) derror(100) ;  
             
	    relval[nn1]=relval[qqq->valptr] ;  /* ... and relocate it */
	    qqq->valptr=nn1 ; 
	    }
                                                                 
	else if (qqq->symtype==-1) {                 /* subscripted */
	    if (intval[qqq->valptr+intval[qqq->valptr]+1]+getsub(qqq)
		<=maxnsv) goto L7 ;   /* already in STATE array */
	    derror(218) ;                   /* must be in STATE array */
	    }         
	else derror(218) ;                          /* must be REAL */
                      }
          
     else {                                                                  /* not defined, install it */
	if (vartyp<=0) derror(84) ; /* nix on subscripted, function! */
	if (++nn1>maxnsv) derror(100) ;  /* count new state variable */

	qqq=(struct symnode*)malloc(sizeof(struct symnode)) ;
	qqq->SWITCH=TRUE ;
	qqq->link=hashtab[hashval] ;
	strcpy(qqq->name,symbol) ;
	qqq->symtype=1 ;
	hashtab[hashval]=qqq ;
	relval[nn1]=0 ;                        /* initialize it to 0 */
	qqq->valptr=nn1 ; 
                       }                                              /* end else */
    goto L7 ;

/* ------------------------------------------------------------------- */
    /* COMPILER PASS 2 */

    L10 :  typeflg=0 ;                           /* no ## type command yet */
    if (!nn1) size=0 ;                        /* use irule 0 if nn1=0 <*****/

    gjj=maxrlvar+40 ; wptr=1 ; flevel=0 ;         /**> reset stacks */

    dbegin=sbegin=rbegin=jcstart=0 ;      /* no display, type, ...  */
                   
    /*** shiftflag=0 ; ***/                    /* no index-shift yet */
      
    smpflg=FALSE ;                      /* ... Jacobian, SAMPLE yet */
    di=ci=0 ;                             /* initialize compilation */
    gi=cstart ;

    L20 :  gi+=5 ;           /* gi parses successive DYNAMIC segment lines */

/********> **/

    L30 :  lj=warea[gi-1] ;                                   /* get token */

    switch(lj) {        /* "break" checks ci, L51 does not need to */

	/*if*/      
	case 13 : cplexp(0) ; COMPILE[ci++]=cif ; break ;

	    /*end*/      
	case 20 :
	    if (stktop==NULL) derror(22) ;
	    if (stktop->head!='C') derror(4) ;
	    q=stktop->cvar ;
	    gi=stktop->radd ;
	    flag1=FALSE ;                   /* clear nested-definition flag */
	    q->SWITCH=TRUE ; q=q->next ;
	    while (q!=NULL) { q->SWITCH=FALSE ; q=q->next ; }
	    tptr=stktop ;
	    stktop=stktop->dslink ;
	    free(tptr) ;
	    goto L51 ;                   /* no break needed */

	    /*STOP*/     
	case 22 : goto L60 ;

	    /*VECTOR*/   
	case 68 : cplarray(0) ; goto L51 ;

	    /*DOT*/      
	case 69 :
	    ppp=getvar() ; 
	    if (errnum) derror(errnum) ;
	    if (vartyp==0) derror(84) ;
	    if (ppp==NULL) ppp=instal() ;
	    if (ppp->symtype==1) sub=ppp->valptr ;
	    else if (ppp->symtype==-1)
		sub=intval[ppp->valptr+intval[ppp->valptr]+1]+getsub(ppp) ;
	    else derror(84) ;
	    if (warea[gi++]!='=') derror(29) ;             /* check, skip = */
	    cpldot(0) ;
	    dptr[di++]=sub ; if (di>maxparam) derror(90) ;
	    COMPILE[ci++]=cpop ;                            /* COMPILE  pop */
	    break ;                                             /* CASE DOT */

	    /*MATRIX*/   
	case 70 : cplarray(-2) ; goto L51 ;

	    /*JACOBIAN*/ 
	case 75 : if (jcstart) derror(230) ; /* only one Jacobian */
	    COMPILE[ci++]=cret ;          /* derve1 is done */
	    jdstart=di ; jcstart=ci ;   /* Jacobian markers */
	    break ;

	    /*label*/    
	case 76 : goto L60 ;

	    /*invoke*/   
	case 79 : call(1) ; goto L51 ;

	    /*LET*/      
	case 80 :
	    ppp=getvar() ; 
	    if (errnum) derror(errnum) ;
	    if (vartyp==0) derror(84) ; 
	    if (ppp==NULL) ppp=instal() ;
	    if (ppp->symtype==1) {
		if (warea[gi++]!='=') derror(29) ;   /* check, skip = */
		cplexp(0) ;
		dptr[di++]=ppp->valptr ;
	    }
	    else if (ppp->symtype==-1) {
		sub=intval[ppp->valptr+intval[ppp->valptr]+1]+getsub(ppp) ;
		if (warea[gi++]!='=') derror(29) ;    /* check, skip = */
		cplexp(0) ;
		dptr[di++]=sub ; /* preserve order of statements */
	    }  
	    if (di>maxparam) derror(90) ;
	    /**> **/  
	    /* COMPILE  pop */
	    COMPILE[ci++]=cpop ; 
	    break ;                                             /* CASE LET */

	    /*comment*/  
	case 81 : while (warea[gi]!=lf) gi++ ; goto L51 ;

	    /*term*/     
	case 82 : cplexp(0) ; COMPILE[ci++]=cterm ;
	    break ;

	    /*d/dt*/     
	case 83 :
	    ppp=getvar() ;/* already installed and checked! pop derivative */
	    if (ppp->symtype==1) {
		if (warea[gi++]!='=') derror(29) ;         /* check, skip = */
		cplexp(0) ;
		dptr[di++]=ppp->valptr ;
	                       }
	    else if (ppp->symtype==-1) {
		sub=intval[ppp->valptr+intval[ppp->valptr]+1]+getsub(ppp) ;
		if (warea[gi++]!='=') derror(29) ;         /* check, skip = */
		cplexp(0) ;
		dptr[di++]=sub ;      /* preserve statement order! */
	                       }  
	    if (di>maxparam) derror(90) ;
	    /**> **/ COMPILE[ci++]=cpopd ; 
	    break ;

	    /*dispt*/   
	case 85 : dmode=0 ; li=12 ; goto L90 ;            /*  <****/

	    /*CLEARN*/   
	case 86 : cplarray(4) ; goto L51 ;

	    /*tdelay*/  
	case 87 : vardata(1) ;                /* tdelay y=X(x,tau */
	    if (warea[gi++]!='=') derror(29) ;
	    arraydata() ;
	    if (warea[gi]!=','&& warea[gi]!='(') derror(25) ;
	    gi++ ;
	    vardata(0) ;
	    if (warea[gi++]!=',') derror(25) ;
	    vardata(0) ;
	    COMPILE[ci++]=ctdela ;
	    break ;

	    /*store*/   
	case 88 : arraydata() ;                      /* store X=x */
	    if (warea[gi++]!='=') derror(29) ;
	    vardata(0) ;
	    COMPILE[ci++]=cstore ;
	    break ;

	    /*SAMPLE*/  
	case 89 : if (smpflg) derror(8) ;            /* SAMPLE nn */
	    if (di>maxparam-2) derror(90) ;
	    smpflg=TRUE ;
	    if ((dptr[di]=evalexp())<0) derror(110) ;
	    di+=2 ;                       /* nn and counter */
	    COMPILE[ci++]=csample ;
	    break ;
	    /*get*/     
	case 90 : vardata(1) ;                         /* get x=X */
	    if (warea[gi++]!='=') derror(29) ;
	    arraydata() ;
	    COMPILE[ci++]=cget ;
	    break ;

	    /*type*/    
	case 91 :  if (warea[gi]=='#') {        /* file or printer */
	    gi++ ;                  
	    if (warea[gi]=='#') {   /* suppress file header */      
		gi++ ; typeflg=1 ; }
	    gitchn() ;
	    if ((outptr=fd[chanlno])==NULL) derror(64) ;
	    if (warea[gi++]!=',') derror(44) ;
	}
	else outptr=stdout ;         /* type to console */
	    dmode=-1 ; li=5 ; goto L90 ;
	    /*delay*/   
	case 92 : vardata(1) ;                 /* delay y=X(x,tau */ 
                           if (warea[gi++]!='=') derror(29) ;
	    arraydata() ;
	    if (dptr[di-4]<256) derror(2) ;    /* too small */
	    if (warea[gi]!=','&& warea[gi]!='(') derror(25) ;
	    gi++ ;
	    vardata(0) ;
	    if (warea[gi++]!=',') derror(25) ;
	    vardata(0) ;
	    COMPILE[ci++]=cdelay ;
	    break ;

	    /*trkhld*/  
	case 93 : vardata(1) ;                 /* trkhld y,y0=p,x */      
                           if (warea[gi++]!=',') derror(25) ;
	    vardata(1) ;
	    if (warea[gi++]!='=') derror(29) ;
	    vardata(0) ;
	    if (warea[gi++]!=',') derror(25) ;
	    vardata(0) ;
	    COMPILE[ci++]=ctrkhld ;
	    break ;

	    /*stash*/   
	case 94 : if (sbegin) derror(8) ; sbegin=gi ; /* only one */
	    li=20 ;                           /* max. count */
	    goto L45 ;

	    /*OUT*/     
	case 95 : COMPILE[ci++]=cout ; break ;

	    /*func*/    
	case 96 : vardata(1) ;      /* func y=Y(x       changed 4/99 */
	    if (warea[gi++]!='=') derror(29) ;
	    arraydata() ;
	    if (warea[gi++]!='(') derror(7) ;
	    dptr[di-4]/=2 ;                  /* dimension/2 */
	    dptr[di-2]=dptr[di-4]+dptr[di-3] ;    /* Ystart */
	    vardata(0) ;
	    COMPILE[ci++]=cfunc ;
	    break ;

	    /*dispxy*/  
	case 97 : dmode=1 ; li=12 ; goto L90 ;            /*   <***/

	    /*recover*/ 
	case 98 : if (rbegin) derror(8) ; rbegin=gi ; /* only one */
	    li=20 ;                           /* max. count */
	    goto L45 ;

	    /*MAT d/dt*/ 
	case 99 : cplarray(2) ; goto L51 ;    /* REAL STATE array */

	    /*step*/ 
	case 100 : COMPILE[ci++]=cstep ; break ;

	    /*DISPXY*/ 
	case 105 : dmode=2 ; li=12 ; goto L90 ;            /*   <***/

	    /*count1*/ 
	case 106 : arraydata() ;                  /* count CTR=x,Y */
	    if (warea[gi++]!='=') derror(29) ;
	    vardata(0) ;
	    if (warea[gi++]!=',') derror(25) ;
	    arraydata() ;
	    COMPILE[ci++]=ccount1 ;
	    break ;
	    /* 107, 108 not used */

	    /*UPDATE*/ 
	case 109 : cplarray(1) ; goto L51 ;

	    /*LEARN*/  
	case 110 : cplarray(-1) ; goto L51 ;

	    /*SHOW*/  
	case 111 : if (ci>maxcomp-2 || di>maxparam-3) derror(90) ;
	    if (warea[gi]==lf || warea[gi]=='|') {
		flag=1 ; COMPILE[ci++]=cshowstrt ;
		break ;
	                       }
	    if (warea[gi]=='*') {
		gi++ ; flag=-1 ; COMPILE[ci++]=cshowstrt ;
		break ;
	                       }
	    if (!flag) derror(112) ; 
	    start=1 ; q=getvar() ; start=0 ;   /* get array */
	    if (errnum) derror(errnum) ;
	    if (q==NULL || q->symtype!=-1) derror(70) ;
	    li=q->valptr ;
	    if (flag>0) dptr[di++]=80 ; else dptr[di++]=40 ;
	    if (warea[gi]==',') {
		gi++ ;                         /* skip comma */
		val=evalexp() ; if (errnum) derror(errnum) ;
		if (val>0 && val<=80) dptr[di-1]=(int)val ;
	                       }
	    if (intval[li]==1) { 
		dptr[di++]=intval[li+1] ;    /* dimension #1 */
		dptr[di++]=intval[li+2] ;   /* array address */
	                       }
	    else {
		dptr[di++]=intval[li+1]*intval[li+2] ; 
		dptr[di++]=intval[li+3] ;   /* array address */
	                       }
	    COMPILE[ci++]=ccshow ; 
	    break ;
	    
	    /*INTP*/  
	case 112 : cplarray(5) ; goto L51 ;

	    /**** 113 was pollx ***/

	    /*PLEARN*/  
	case 114 : cplarray(3) ; goto L51 ;

	    /*delta*/   
	case 121 : cplarray(1) ; goto L51 ;

	    /*DELTA*/   
	case 122 : cplarray(-1) ; goto L51 ;

	    /*Vector*/  
	case 123 :
	    ddtflag=0 ;
	    start=1 ; ppp=getvar() ; if (errnum) derror(errnum) ;
	    if (ppp==NULL) derror(70) ;
	    jjj=ppp->valptr ;
	    if (ppp->symtype!=-1 || intval[jjj]!=1) derror(84); /* illegal */
	    kkk=VROW=intval[jjj+1] ; VBASE=intval[jjj+2] ;
	    if (warea[gi]=='^') {                       /* winner-take-all */
		gi++ ;                                            /* skip ^ */
		maxflag=TRUE ;
	                       }
	    else maxflag=FALSE ;
	    
	    if (warea[gi++]!='=') derror(29) ;                   /* skip = */
	    
/* @@@ ------------------------------------------------------------- */
	    if (warea[gi]=='[') {                     /* read mask, if any */
		gi++ ;                                            /* skip [ */
		PP=getvar() ; if (errnum) derror(errnum) ;
		if (PP==NULL || PP->symtype!=-1) derror(70) ; /* check mask */
		jjj=PP->valptr ;
		if (intval[jjj]!=1 || intval[jjj+1]!=VROW) derror(74) ;
		MASKBASE=intval[jjj+2] ;
		if (warea[gi++]!=']') derror(25) ;               /* skip ] */
	                       }
	    else MASKBASE=-1 ;   
	    
/* ----------------------------------------------------------------- */ 
	    start=0 ;     
	    ggg=gi ;                              /* remember text pointer */
	    
	    while (kkk>0) {
		
		if (MASKBASE<0 || relval[MASKBASE+kkk-1]==0) {         
		    gi=ggg ;       /* reset text pointer behind = or ] */
		    wptr=1 ; flevel=0 ;       /* reset expression stack etc. */
		    cplexp(kkk) ;
		    }
		else COMPILE[ci++]=cpush0 ;              /* masked, push 0 */
		dptr[di++]=VBASE+kkk-1 ;
		COMPILE[ci++]=cpop;
		if (di>maxparam || ci>maxcomp) derror(90) ;             
		kkk-- ;           
	                       }
	    
	    if (maxflag) {
		dptr[di++]=VBASE ; dptr[di++]=VROW ;            
		if (ci>maxcomp || di>maxparam-1) derror(90) ;
		COMPILE[ci++]=cMAX ;
	    }                     
	    break ;    
	    /* Vectr d/dt */ 
	case 124 :
	    ddtflag=1 ;
	    start=1 ; ppp = getvar() ; if (errnum) derror(errnum) ;
	    if (ppp==NULL) derror(70) ; 
	    jjj=ppp->valptr ;
	    if (ppp->symtype!=-1 || intval[jjj]!=1) 
		derror(84) ;/* illegal */     
	    kkk=VROW=intval[jjj+1] ; VBASE=intval[jjj+2] ;
	    if (VBASE>maxnsv) derror(84) ;       /* need STATE declaration */
	    if (warea[gi++]!='=') derror(29) ;                   /* skip = */ 
	    /* @@@ ------------------------------------------------------ */
	    if (warea[gi]=='[') {                     /* read mask, if any */
		gi++ ;                                            /* skip [ */
		PP=getvar() ; if (errnum) derror(errnum) ;
		if (PP==NULL || PP->symtype!=-1) derror(70) ; /* check mask */
		jjj=PP->valptr ;
		if (intval[jjj]!=1 || intval[jjj+1]!=VROW) derror(74) ;
		MASKBASE=intval[jjj+2] ;
		if (warea[gi++]!=']') derror(25) ;               /* skip ] */
	                       }
                          else MASKBASE=-1 ;  
	    
/* ----------------------------------------------------------------- */
	    start=0 ;   
	    ggg=gi ;                              /* remember text pointer */  
	    
	    while (kkk>0) {
		
		if (MASKBASE<0 || relval[MASKBASE+kkk-1]==0) {         
		    gi=ggg ;      /* reset text pointer behind = or ] */  
		    wptr=1 ; flevel=0 ;       /* reset expression stack etc. */
		    cplexp(kkk) ;            
		    }           
		else COMPILE[ci++]=cpush0 ;              /* masked, push 0 */
		dptr[di++]=VBASE+kkk-1 ;
		COMPILE[ci++]=cpopd ;
		if (di>maxparam || ci>maxcomp) derror(90) ;             
		kkk-- ;           
	    }      
	    break ;
	    
	    /*Vectr delta*/  
	case 125 :
	    ddtflag=0 ;
	    start=1 ; ppp = getvar() ; if (errnum) derror(errnum) ;
	    if (ppp==NULL) derror(70) ;
	    
	    jjj=ppp->valptr ;
	    if (ppp->symtype!=-1 || intval[jjj]!=1) derror(84); /* illegal */
	    kkk=VROW=intval[jjj+1] ; VBASE=intval[jjj+2] ;
	    
	    if (warea[gi++]!='=') derror(29) ;                   /* skip = */
	    
/* @@@ ------------------------------------------------------------- */
	    
	    if (warea[gi]=='[') {                     /* read mask, if any */
		gi++ ;                                            /* skip [ */
		PP=getvar() ; if (errnum) derror(errnum) ;
		if (PP==NULL || PP->symtype!=-1) derror(70) ; /* check mask */
		jjj=PP->valptr ;
		if (intval[jjj]!=1 || intval[jjj+1]!=VROW) derror(74) ;
		MASKBASE=intval[jjj+2] ;
		if (warea[gi++]!=']') derror(25) ;               /* skip ] */
	                       }
	    else MASKBASE=-1 ;
	    
/* ----------------------------------------------------------------- */ 
	    start=0 ;      
	    ggg=gi ;                              /* remember text pointer */
	    
	    while (kkk>0) {
		
		if (MASKBASE<0 || relval[MASKBASE+kkk-1]==0) {
		    gi=ggg ;      /* reset text pointer behind = or ] */
		    wptr=1 ; flevel=0 ;       /* reset expression stack etc. */
		    cplexp(kkk) ;
		    }           
		else COMPILE[ci++]=cpush0 ;              /* masked, push 0 */
		
		dptr[di++]=VBASE+kkk-1 ; 
		COMPILE[ci++]=CADD ;
		if (di>maxparam-1 || ci>maxcomp-1) derror(90) ;
		
		dptr[di++]=VBASE+kkk-1 ;
		COMPILE[ci++]=cpop;           
		kkk-- ;           
	    }        
	    break ;
	    	    
	    /*func2*/  
	case 126 :                    /* func2 y=Y(x1,x2; X1,X2 */
	    vardata(1) ;                                      /* get y */
	    if (warea[gi++]!='=') derror(29) ; 
	    arraydata2(1) ;                      /* get N1, N2, Ystart */
	    if (warea[gi++]!='(') derror(7) ;                       
	    vardata(0) ; if (warea[gi++]!=',') derror(25) ; /* get x1 */
	    vardata(0) ;                                     /* get x2 */
	    
	    if (warea[gi]!=';' && warea[gi]!=',') derror(25) ; gi++ ;             
	    arraydata2(0) ;                /* get N1,X1start */
	    if (warea[gi++]!=',') derror(25) ;  
	    arraydata2(0) ;                 ; /* get N2,X2start */
	    
	    if (dptr[di-4] != dptr[di-9]                       
		|| dptr[di-2] !=  dptr[di-8]) derror(84) ;
	    /* N1, N2 must match */
	    dptr[di-4]+=dptr[di-3] ;      /* X1end=X1start+N1 */
	    dptr[di-2]+=dptr[di-1] ;      /* X2end=X2start+N2 */
	    
	    COMPILE[ci++]=cfunc2  ; 
	    break ;
	    
/*******    fuzz case 127 :                        * fuzz i,member1,member2 = x  *
   
vardata(0) ;
if (warea[gi++]!=',') derror(25) ;
vardata(1) ;                     
if (warea[gi++]!=',') derror(25) ;
vardata(1) ;
if (warea[gi++]!='=') derror(29) ;
vardata(1) ;
break ;

 select * case 128 :                                select y = Y[i,...  

vardata(1) ;
arraydata2(0) ;
break ;    

                                ******/

/* AVG */  case 131 :
      ppp=getvar() ; if (errnum) derror(errnum) ;
       if (vartyp==0) derror(84) ;
       if (ppp==NULL) ppp=instal() ;
       if (ppp->symtype==1) sub=ppp->valptr ;
       else if (ppp->symtype==-1)
               sub=intval[ppp->valptr+intval[ppp->valptr]+1]+getsub(ppp) ;
       else derror(84) ;
       if (warea[gi++]!='=') derror(29) ;             /* check, skip = */
       cpldot(2) ;
       dptr[di++]=sub ; if (di>maxparam) derror(90) ;
       COMPILE[ci++]=cpop ;                            /* COMPILE  pop */
       break ;                                             /* CASE AVG */
         
	default : derror(219) ;
    }                                             /* end switch */
    
    if (ci>maxcomp) derror(90) ;            /* "break" goes here */
    
    L51 :  if (warea[gi]=='|') { gi+=2 ; goto L30 ; } /* next statement */
    if (warea[gi]!=lf) derror(33) ;
    if (gi<endtxt-1) goto L20 ;                     /* next line */

/* --------------------------------------------- COMPILATION IS DONE */

    L60 :  if (jcstart==0) COMPILE[ci]=cret ; /* no Jacobian code exists */
    else COMPILE[ci]=jret ;                 /* Jacobian code exists */

    if (cflag) xlength=0 ;          /* "label", recompile next time */
    else xlength=ci ;               /* keep compiled code next time */
       
    /*** killed - if (shiftflag) xlength=0 ; else xlength=ci ; ***/
       
    gi=gisav ;                      /* restore "drun" line location */

    INITE1=1 ;                                          /* INITE1>0 */
    return ;                                               /* done */

/* ------------------------------------------------------------------- */
/*    ppp->valptr = gkk is index of array dimension in intval table    */
/*    intval[gkk+1] (= addr1) is the array dimension                   */
/*    intval[gkk+2] (= addr2) is index of 1st array element            */
/* ------------------------------------------------------------------- */
  
    /* display, stash, etc. lists compile into address buffers */

    L90 :  if (dbegin!=0) derror(8) ;        /* only one display/type list */
    dbegin=gi ;                          /* marks the list in warea */

    L45 :  li-- ;                            /* all lists are counted here */

    ppp=getvar() ; if (errnum) derror(errnum) ;      /* get address */
    if (vartyp==0) derror(84) ;
    if (ppp==NULL) {
	if (lj!=98) derror(200) ;
	ppp=instal() ;
    }
    if (ppp->symtype==1) sub=ppp->valptr ;
    else if (ppp->symtype==-1)
	sub=intval[ppp->valptr+intval[ppp->valptr]+1]+getsub(ppp) ;
    else derror(84) ;

    if (li<0) derror(17) ;
    if (lj==94) {                                          /* stash */
	bufstash[20-li]=&relval[sub] ;
	bufstash[0]=&relval[mmm+5] ;
    }
    else if (lj==98) {                                   /* recover */
	bufrecvr[20-li]=&relval[sub] ;
	bufrecvr[0]=&relval[mmm+5] ;
    }
    else if (lj==91) bufdisp[5-li]=&relval[sub] ;           /* type */
       
    else bufdisp[12-li]=&relval[sub] ;                   /* display */  
/*   <***/

    if (warea[gi]==',') { gi++ ; goto L45 ; }

    if (lj==94) bufstash[21-li]=NULL ;                     /* stash */
    else if (lj==98) bufrecvr[21-li]=NULL  ;             /* recover */
    else if (lj==91) bufdisp[6-li]=NULL ;                   /* type */
       
    else bufdisp[13-li]=NULL ;                           /* display */  
/*   <***/

    goto L51 ;
    }                                                          /* end drun00(0) */


/*** labelled virtual-machine intructions **/

  xcpush0 :  relval[--gjj]=0 ;  goto *COMPILE[ci++] ;       /* push 0 */
  xcpushvar :  relval[--gjj]=relval[dptr[di++]] ; goto *COMPILE[ci++] ;  /* push */
  xcpop :  relval[dptr[di++]]=relval[gjj++] ;  goto *COMPILE[ci++] ;      /* pop */
  xcpopd : sderiv[dptr[di++]]=relval[gjj++] ; goto *COMPILE[ci++] ; /* popd */

  xcadd : relval[gjj+1]+=relval[gjj] ; gjj++ ;
           goto *COMPILE[ci++] ;                                 /* add/pop */
  xcsub : relval[gjj+1]-=relval[gjj] ; gjj++ ; 
           goto *COMPILE[ci++] ;                           /* subtract /pop */
  xcmult : relval[gjj+1]*=relval[gjj] ; gjj++ ; 
           goto *COMPILE[ci++] ;                            /* multiply/pop */
  xcdiv :  relval[gjj+1]/=relval[gjj] ; gjj++ ;  
           goto *COMPILE[ci++] ;                              /* divide/pop */

  xcpwr :                                                    /* x^y */
  if (relval[gjj]==2) relval[gjj+1]=relval[gjj+1]*relval[gjj+1] ;  
  else {
    relval[gjj+1]=exp(relval[gjj]*log(relval[gjj+1])) ;
    if (errno==EDOM) derror(9) ;
    }
  gjj++ ;goto *COMPILE[ci++] ; 
  
  xCADD : relval[gjj]+=relval[dptr[di++]] ; goto *COMPILE[ci++] ;   /* add */
  xCSUB :  relval[gjj]-=relval[dptr[di++]] ;  goto *COMPILE[ci++] ; /* subtract */
  xCMULT : relval[gjj]*=relval[dptr[di++]] ; goto *COMPILE[ci++] ; /* multiply */
  xCDIV : relval[gjj]/=relval[dptr[di++]] ;  goto *COMPILE[ci++] ;  /* divide */

  xCPWR :                                                    /* x^y */
  if (relval[dptr[di++]]==2) relval[gjj]=relval[gjj]*relval[gjj] ;
  else {
    relval[gjj]=exp(relval[dptr[di-1]]*log(relval[gjj])) ;
    if (errno==EDOM) derror(9) ;
    }
  goto *COMPILE[ci++] ; 

  xccsin :  relval[gjj]=sin(relval[gjj]) ;  goto *COMPILE[ci++] ;                  /* sin */
  xcccos :  relval[gjj]=cos(relval[gjj]) ;  goto *COMPILE[ci++] ;                  /* cos */
  xcctan :  relval[gjj]=tan(relval[gjj]) ;  goto *COMPILE[ci++] ;                  /* tan */
  xccexp :  relval[gjj]=exp(relval[gjj]) ;  goto *COMPILE[ci++] ;                 /* exp */
  xcctanh :  relval[gjj]=tanh(relval[gjj]) ;  goto *COMPILE[ci++] ;               /* tanh */
  xccsinh :  relval[gjj]=sinh(relval[gjj]) ;  goto *COMPILE[ci++] ;              /* sinh */
  xcccosh :  relval[gjj]=cosh(relval[gjj]) ; goto *COMPILE[ci++] ;                /* cosh */
  xccasin :  relval[gjj]=asin(relval[gjj]) ;  goto *COMPILE[ci++] ;               /* 
                asin */
  xccacos :  relval[gjj]=acos(relval[gjj]) ;  goto *COMPILE[ci++] ;               /* acos */
  xccatan :  relval[gjj]=atan(relval[gjj]) ;  goto *COMPILE[ci++] ;               /* atan */

  xccsinc :                                                  /* sinc */
        if (relval[gjj]==0) relval[gjj]=1 ;
        else relval[gjj]=sin(relval[gjj])/relval[gjj] ;
        goto *COMPILE[ci++] ;     

  xcrecip : relval[gjj]=1/relval[gjj] ;  goto *COMPILE[ci++] ;                  /* recip */

/*************
  xcran1 : relval[gjj]=2*d_lcran_()-1.0 ; goto *COMPILE[ci++] ;                   ran1 for Sun C
  xcran : relval[--gjj]=2*d_lcran_()-1.0 ; goto *COMPILE[ci++] ;                   ran 
                                                          note push!  
                                                 
  xcran1 :  relval[gjj]=ldexp((double)rand(),-14)-1.0 ; goto *COMPILE[ci++] ;     ran1 for WATCOM 
  xcran : relval[--gjj]=ldexp((double)rand(),-14)-1.0 ; goto *COMPILE[ci++] ;     ran 
                                       note push! ****/                  
                                                                                            

  xcran1 : relval[gjj]=2*drand48()-1.0 ;  goto *COMPILE[ci++] ;     /* ran1 for Linux gcc */
  xcran :  relval[--gjj]=2*drand48()-1.0 ;  goto *COMPILE[ci++] ;      /* ran  */

   /******************

  xcran1 :                                           ran1 for CYGWIN 
        double xxx ;    
        xxx=(double)rand() ; relval[gjj]=2.0*xxx/ 2147483647.0-1.0 ;
        goto *COMPILE[ci++] ; 
         
  xcran :                                            ran for CYGWIN  
        double xxx ;  
        xxx=(double)rand() ; relval[--gjj]=2.0*xxx/ 2147483647.0-1.0 ;   
       goto *COMPILE[ci++] ;                                                      note push!  

 ****/  
 
   xcgauss : relval[gjj]=sqrt(-2*log(drand48()))*cos(2*PI*drand48()) ;
	 /* &&& <***/

           goto *COMPILE[ci++] ;                                                 

  xccsqrt :                                                  /* sqrt */
   relval[gjj]=sqrt(relval[gjj]) ;
   if (errno==EDOM) derror(47) ; 
   goto *COMPILE[ci++] ; 
   
  xcln :                                                      /* ln */
   relval[gjj]=log(relval[gjj]) ;
   if (errno==EDOM) derror(9) ; 
   goto *COMPILE[ci++] ; 
      
  xcclog :                                                    /* log */
   relval[gjj]=log10(relval[gjj]) ;
   if (errno==EDOM) derror(9) ;
    goto *COMPILE[ci++] ; 

  xcfabs :  relval[gjj]=fabs(relval[gjj]) ;  goto *COMPILE[ci++] ;                /* abs */

  xclim :  relval[gjj]=0.5*(relval[gjj]+fabs(relval[gjj])) ;   goto *COMPILE[ci++] ;   /* lim */  /* <****/

  xcswtch : 
   if (relval[gjj]>0) relval[gjj]=1 ; else relval[gjj]= 0 ; 
   goto *COMPILE[ci++] ;                                             /* swtch */

  xctri :  relval[gjj]=1.0-fabs(relval[gjj]) ; goto *COMPILE[ci++] ;   /* tri */ /** <****/
  
  /**  if (relval[gjj]<0) relval[gjj]+=1 ;                     
   else relval[gjj]=1-relval[gjj] ;    **/
   
  xcrect :                                                   /* rect */
   if (fabs(relval[gjj])<1) relval[gjj]=1 ;                 
   else relval[gjj]=0 ;
   goto *COMPILE[ci++] ;
   
   
  xcsgn :                                                    /* sgn */
   if (relval[gjj]>0) relval[gjj]=1 ;
   else if (relval[gjj]<0) relval[gjj]=-1 ;
   else relval[gjj]=0 ;
   goto *COMPILE[ci++] ;
   
  xcsat :                                                    /* sat */ /* <****/
   relval[gjj]=0.5*(fabs(relval[gjj]+1.0)-fabs(relval[gjj]-1.0)) ;
   goto *COMPILE[ci++] ;
   
  /****  if (relval[gjj]>1) relval[gjj]=1 ;
   else if (relval[gjj]<-1) relval[gjj]=-1 ;  ***/
   
  xcSAT :                                                    /* SAT */ /* <***/
     relval[gjj]=0.5*(1.0+fabs(relval[gjj])-fabs(relval[gjj]-1)) ;
     goto *COMPILE[ci++] ;

   /***  if (relval[gjj]>1) relval[gjj]=1 ;
   else if (relval[gjj]<0) relval[gjj]=0 ;  ***/
   
  xcdeadz :                                                /* deadz */ /*  <***/
       relval[gjj]=relval[gjj]-0.5*(fabs(relval[gjj]+1.0)-fabs(relval[gjj]-1.0)) ;
       goto *COMPILE[ci++] ;

   /*** if (relval[gjj]>1) relval[gjj]-=1 ;
   else if (relval[gjj]<-1) relval[gjj]+=1 ;
   else relval[gjj]=0 ;  ***/
 
  xcdeadc :                                                /* deadc */ 

    if (relval[gjj]>1) relval[gjj]=1 ;
    else if (relval[gjj]<-1) relval[gjj]=-1 ;
    else relval[gjj]=0 ;
    goto *COMPILE[ci++] ;
   
  xcsigmoid : relval[gjj]=1/(exp(-relval[gjj])+1) ; goto *COMPILE[ci++] ;   /* sigmoid */

  xcsigsig :                                             /* SIGMOID */
   if (relval[gjj]>0) {
      relval[gjj]=relval[gjj]*relval[gjj] ;
      relval[gjj]=relval[gjj]/(relval[gjj]+1) ;
      }
   else relval[gjj]=0 ;
   goto *COMPILE[ci++] ;

  xccatanx :                                                /* atan2 */
      relval[gjj+1]=atan2(relval[gjj+1],relval[gjj]) ;
      gjj++ ; goto *COMPILE[ci++] ;
   

  xccomp :                                    /* comp(ctl,min,plus) */
   if (relval[gjj+2]<=0) relval[gjj+2]=relval[gjj+1] ;
   else relval[gjj+2]=relval[gjj] ;
   gjj+=2 ; goto *COMPILE[ci++] ;
   

  xcterm :  if (ICOM>0) { if (relval[gjj++]>0) errnum=303 ; }             /* term <*******/
  goto *COMPILE[ci++] ;
  
  xcif :  if (relval[gjj++]<=0) longjmp(bjump,1) ;   goto *COMPILE[ci++] ;                                      /* if <******/
  xcret :  longjmp(bjump,1) ;                /* derivative return */
  xjret :  longjmp(jjump,1) ;              /* Jacobian-code return */


  xcsample :   /* SAMPLE nn ; dptr[di] has nn, dpt[di+1] is counter */

   if (ICOM>0) {                        /* ICOM>0, communication point */
      if (INITE1>0) dptr[di+1]=dptr[di] ;          /* initialize count */
      if (--dptr[di+1]==0) { dptr[di+1]=dptr[di] ; di+=2 ;  goto *COMPILE[ci++] ;}
      }
   longjmp(bjump,1) ;
   

  xcstep :  if (IDERIV<=0) longjmp(bjump,1) ;  goto *COMPILE[ci++] ;            /* step */

  xcout :  if (ICOM<=0) longjmp(bjump,1) ; goto *COMPILE[ci++] ;                 /* OUT */
   

  xcstore :                                            /* store X=x */

/* ****>     dptr[di] has X dimension;
             +1 has X starting address (address of X1);
             +2 is counter, +3 is pointer; +4 has x-address      <**** */

   if (ICOM>0) {                        /* ICOM>0, communication point */
      if (INITE1>0) {                                    /* initialize */
         dptr[di+2]=dptr[di] ; dptr[di+3]=dptr[di+1] ;    /* ctr & ptr */
         }
      if (dptr[di+2]-- >0) relval[dptr[di+3]++]=relval[dptr[di+4]] ;
      }
   di+=5 ; goto *COMPILE[ci++] ;
   

  xcget :                                               /* get x=X */

   if (ICOM>0) {                        /* ICOM>0, communication point */
      if (INITE1>0) {                                    /* initialize */
         dptr[di+3]=dptr[di+1] ; dptr[di+4]=dptr[di+2] ;  /* ctr & ptr */
         }
      if (dptr[di+3]-- >0) relval[dptr[di]]=relval[dptr[di+4]++] ;
      }
   di+=5 ; goto *COMPILE[ci++] ;

  xcfunc :                      /* y = F(x ; changed 4/99; no longer used */

/* ****> on entry, dptr[di] has y-address;
                   +1 has F dimension/2 = N ; 
                   +2 has F starting address (Xstart, address of x1);
                   +3 has Ystart (F start address + dimension/2)           <**** */

  /* double x,y ;  int  I, K ; */        /* *COMPILEr precomputes Ystart */
   
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
   di+=6 ; goto *COMPILE[ci++] ;
   
  xcfunc1 : 			/* y=cfunc1(x,Farray), vectorizable, added 7/05 */
					/* ARRAY x[N]+Y[N]=F */

/******>  on entry, dptr[di] has F dimernsion/2= N
	                       dptr[di+1] has  F starting address (Xstart, address of x1)
			       dptr[di+2] has Ystart=Xstart+dimension/2               <**********/

/*	double x ; int I,K ;  */
	
    I=dptr[di+1] ;								/* i=Xstart */
    x=relval[gjj] ;
    if (x>relval[I]) {                                                         /* x > Xstart */       
          do { if (x<=relval[++I]) break ; }
                           while (I<dptr[di+2]) ;        /* while I < Xstart + N */
          K=I+dptr[di] ;          				/* K=corresponding Y */                 
          if (I<dptr[di+2])                               /* interpolate if X<Ystart */

                relval[gjj]=relval[K]+(x-relval[I])
                        *(relval[K]-relval[K-1])/(relval[I]-relval[I-1]) ;
         
         else relval[gjj]=relval[K-1] ;	                         /* upper end */
         }
   else relval[gjj]=relval[dptr[di+2]] ;     /* y=Ystart at lower end */
  di+=3 ; goto *COMPILE[ci++] ;
   
/* ------------------------------------------------------------------------ */

  xcfuzz :                                           /** FUZZ lo,hi,i=x,X 
                                                                               
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
          goto *COMPILE[ci++] ;                                             **************/

  xcfunc2 :                                     /* y = Y(x1,x2;X1,X2  */
                        
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
                   (di ends at +10)                        <*****/
                                                                               

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
 
   + HiMEMBER1*(LoMEMBER2*relval[Ystart+K1*N2 + (K2-1)]                
                   + HiMEMBER2*relval[Ystart+K1*N2 +K2]) ;                                                     
 di+=10 ;
 goto *COMPILE[ci++] ;
   
  xcxfunc2 : 		/* y = cfunc2(x,y,Farray,Xarray,Yarray) , added 7/05 */

/******>  on entry,  dptr[di] has dimension N1 of F (i.e. dimension of X1)  
												from arraydata2(1)                 
                   +1 has dimension N2 of F (i.e. dimension of X2) 
                   +2 has Fstart (address of F[1,1])                                                
                   +3 has N1, then X1start+N1 = X1end            from arraydata2(0)
                   +4 has X1start (address of X1[1])
                   +5 has N2, then X2start+N2 = X2end           from arraydata2(0)
                   +6 has X2start (address of X2[1])
               			(di ends at +7)                      <********/                                                                 
 
  Fstart=dptr[di+2] ; X1end=dptr[di+3] ; X2end =dptr[di+5] ; N2=dptr[di+1];
  x1=relval[gjj+1] ; x2=relval[gjj] ;

  I1=dptr[di+4] ;                                        /* I1 = X1start */  
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

  I2=dptr[di+6] ;                                        /* I2 = X2start */  
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

  K1=I1-dptr[di+4] ; K2=I2-dptr[di+6] ;
  
 /*** relval[dptr[di]]=  LoMEMBER1*LoMEMBER2*relval[Ystart +(K1-1)*N2 + (K2-1)]
                       + HiMEMBER1*LoMEMBER2*relval[Ystart +K1*N2     + (K2-1)]
                       + LoMEMBER1*HiMEMBER2*relval[Ystart +K1-1)*N2  + K2]
                       + HiMEMBER1*HiMEMBER2*relval[Ystart +K1*N2     + K2] ; ***/
 
  relval[gjj+1]= LoMEMBER1*(LoMEMBER2*relval[Fstart+(K1-1)*N2 + (K2-1)]
                            + HiMEMBER2*relval[Fstart+(K1-1)*N2 + K2])
 
                 + HiMEMBER1*(LoMEMBER2*relval[Fstart+K1*N2     + (K2-1)]                
                      + HiMEMBER2*relval[Fstart+K1*N2 + K2]) ;     
   gjj++  ;     											 /* output now on stack */                                        
   di+=7 ;
   goto *COMPILE[ci++] ;
  
  xcdelay :                                      /* delay y=X,x,tau */

/* ****>     dptr[di] has y-address; +1 has X dimension>=1000;
             +2 has X starting address (address of X1);
             +3 is counter, +4 is pointer; +5 has x-address;
             +6 has tau-address                                  <**** */

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
goto *COMPILE[ci++] ;
   

   xctdela :                                    /* tdelay y=X,x,tau */

/* ****>     dptr[di] has y-address; +1 has X dimension>=1000;
             +2 has X starting address (address of X1);
             +3 is counter, +4 is pointer; +5 has x-address;
             +6 has tau-address                                  <**** */

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
   goto *COMPILE[ci++] ;

  xctrkhld :                                     /* trkhld y,y0=p,x */

/* ****>     dptr[di] has y-address; +1 has y0 address;
             +2 has p address); +3 has x-address;                <**** */

   if (relval[dptr[di+2]]>0) {                                /* track */
      relval[dptr[di]]=relval[dptr[di+1]] ;                    /* y=y0 */
      relval[dptr[di+1]]=relval[dptr[di+3]] ;                  /* y0=x */
      }
   di+=4 ;
   goto *COMPILE[ci++] ;

  xccount1 : goto *COMPILE[ci++] ;

  xcround :                                                /* round */

   /**** int  rj ;  ***/         
                              
   if (relval[gjj]>=maxint || relval[gjj]<=-maxint) derror(20) ;
   if (relval[gjj]>0) relval[gjj]=rj=relval[gjj]+0.5 ;
   else if (relval[gjj]<0)  relval[gjj]=rj=relval[gjj]-0.5 ;
   else relval[gjj]=0 ;
   goto *COMPILE[ci++] ;
/* ------------------------------------------------------------------- */

  xcyinit :                           /* start and mark VECTOR loop */

   NROW1=dptr[di++] ;  YBASE=dptr[di++] ; COUNT=NROW1-1 ;
   cmark=ci ; dmark=di ;                       /* keep for loop return */
   goto *COMPILE[ci++] ;

  xcainit :                           /* start and mark MATRIX loop */

   NROW1=dptr[di++] ;  NCOL1=dptr[di++] ; COUNT=NROW1*NCOL1-1 ;
   YBASE=dptr[di++] ;
   cmark=ci ; dmark=di ;                       /* keep for loop return */
   goto *COMPILE[ci++] ;

  xcpusharr :  relval[--gjj]=relval[dptr[di++]+COUNT] ; goto *COMPILE[ci++] ;  /* array */

  xcaik :                                               /* push A*x */

  /** register int  count, Aindex, xindex ; **/

   count=dptr[di++] ;                                          /* ncol */
   Aindex=dptr[di++]+(COUNT+1)*count ;        /* ABASE+COUNT*ncol+ncol */
   xindex=dptr[di++]+count ;                             /* XBASE+ncol */

   relval[--gjj]=0 ;                                     /*** changed ***/
   while (--count>=0) relval[gjj]+=relval[--Aindex]*relval[--xindex] ;
   goto *COMPILE[ci++] ;

  xcAIK :                                               /* push A*x */

  /** register int  count, ROWEND, XEND ; **/

   count=dptr[di++] ;                                        /* ncol-1 */
   ROWEND=dptr[di++] ;                              /* ABASE+ii*ncol-1 */
   XEND=dptr[di++] ;                                   /* XBASE+ncol-1 */

   relval[--gjj]=0 ;
   while (count-- >=0) relval[gjj]+=relval[ROWEND--]*relval[XEND--] ;
   goto *COMPILE[ci++] ; 

  xcaki :                                              /* push A%*x */

   /** register int  count, Aindex, xindex ;  **/

   count=dptr[di++] ;                      /* nrow of transpose = NCOL */
   Aindex=dptr[di++]+NROW1*count+COUNT ;      /* ABASE+NROW*NCOL+COUNT */
   xindex=dptr[di++]+count ;                             /* XBASE+NCOL */

   relval[--gjj]=0 ;
   while (--count>=0) {
       Aindex-=NROW1 ; relval[gjj]+=relval[Aindex]*relval[--xindex] ; 
      }
   goto *COMPILE[ci++] ;

  xcAKI :                                              /* push A%*x */

  /** register int  count, VROW, COLEND, XEND ; **/

   count=dptr[di++] ;                  /* nrow of transpose-1 = XROW-1 */
   VROW=dptr[di++] ;                                   /* VROW = XBASE */
   COLEND=dptr[di++] ;                    /* ABASE+XBASE*(XROW-1)+ii-1 */
   XEND=dptr[di++] ;                                   /* XBASE+ncol-1 */

   relval[--gjj]=0 ;
   while (count-- >=0) { 
       relval[gjj]+=relval[COLEND]*relval[XEND--] ; COLEND-=VROW ;
      }
   goto *COMPILE[ci++] ;

  xcpatrn :                              /* push pattern-matrix row */

    /** int  i,irow ; **/

     if ((irow=relval[mmm+12]-1)<0) derror(124) ;  /* index 0 is row 1 */
     i= (int)irow%dptr[di++] ;               /* irow%(no. of patterns) */

     relval[--gjj]=relval[dptr[di++]+i*NROW1+COUNT] ;
     goto *COMPILE[ci++] ; 

  xcPATRN :                             /* push pattern-matrix row */

    /** int VROW, irow, i ; **/

     if ((irow=relval[mmm+12]-1)<0) derror(124) ;  /* index 0 is row 1 */    

     i= (int)irow%dptr[di++] ;               /* irow%(no. of patterns) */
     VROW=dptr[di++] ;                             /* vector dimension */    
     relval[--gjj]=relval[dptr[di++]+i*VROW] ;  /* dptr has ABASE+ii-1 */ 
     goto *COMPILE[ci++] ;

  xcmaskq :                                        /* mask an array */

   if (relval[dptr[di++]+COUNT]==0) {
      COUNT-- ; ci=cmark ; di=dmark ;       /* conditional loop return */
      }
   goto *COMPILE[ci++] ;

  xcpushv2 :                       /* push and multiply vector pair */

 /**  int  xbase ; **/

   xbase=dptr[di++] ;
   relval[--gjj]=relval[xbase+COUNT/NCOL1]*relval[dptr[di++]+COUNT%NCOL1] ;
   goto *COMPILE[ci++] ;

  xcmin :                                               /* min(x,y) */

  /** int  xbase ;  **/
  
   xbase=dptr[di++] ;
   if (relval[xbase+COUNT/NCOL1]<relval[dptr[di]+COUNT%NCOL1])
                  { di++ ; relval[--gjj]=relval[xbase+COUNT/NCOL1] ; }
   else relval[--gjj]=relval[dptr[di++]+COUNT%NCOL1] ;
   
   goto *COMPILE[ci++] ;

  xcyend :                                 /* finish sum, test loop */

   relval[YBASE+COUNT]=relval[gjj++] ;          /* a y-element is done */
   if (--COUNT<0) goto *COMPILE[ci++] ; 
 ;                                     /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
   goto *COMPILE[ci++] ; 

  xcyendli :                   /* finish sum, set limits, test loop */

    if (relval[gjj]<relval[gjj+2]) relval[YBASE+COUNT]=relval[gjj] ;
    else if (relval[gjj+1]>relval[gjj+2]) relval[YBASE+COUNT]=relval[gjj+1] ;
    else relval[YBASE+COUNT]=relval[gjj+2] ;
    gjj+=3 ;                                       /* clean data stack */
    if (--COUNT<0) goto *COMPILE[ci++] ; 
 ;                                    /* quit */
    ci=cmark ; di=dmark ;
goto *COMPILE[ci++] ;

  xcupdat :                         /* finish update sum, test loop */

   relval[YBASE+COUNT]+=relval[gjj++] ;       /* a y-increment is done */
   if (--COUNT<0)  goto *COMPILE[ci++] ; 
                                   /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
   goto *COMPILE[ci++] ;

  xcupdatli :           /* finish update sum, set limits, test loop */

  /** register int  i ; **/

   i=YBASE+COUNT ;
   relval[i]+=relval[gjj+2] ;                 /* a y-increment is done */
   if (relval[i]>relval[gjj]) relval[i]=relval[gjj] ;
   else if (relval[i]<relval[gjj+1]) relval[i]=relval[gjj+1] ;
   gjj+=3 ;                                             /* clean stack */
   if (--COUNT<0) goto *COMPILE[ci++] ; 
 ;                                     /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
  goto *COMPILE[ci++] ;

  xcdend :                                 /* finish sum, test loop */

   sderiv[YBASE+COUNT]=relval[gjj++] ;           /* derivative is done */
   if (--COUNT<0) goto *COMPILE[ci++] ;                                /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
 goto *COMPILE[ci++] ;  

  xcdendli :        /* finish derivative sum, set limits, test loop */

  /** register int  i ; **/

   i=YBASE+COUNT ;
   sderiv[i]=relval[gjj+2] ;                     /* derivative is done */
   if (IDERIV>0) {               /* test only once per DT step */
      if (relval[i]>relval[gjj]) relval[i]=relval[gjj] ;
      else if (relval[i]<relval[gjj+1]) relval[i]=relval[gjj+1] ;
      }
   gjj+=3 ;                                        /* clean data stack */
   if (--COUNT<0) goto *COMPILE[ci++] ;                 /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
  goto *COMPILE[ci++] ; 

  xcdotend :                           /* finish DOT sum, test loop */

   relval[gjj+1]+=relval[gjj]*relval[YBASE+COUNT] ; gjj++ ;                 /**  <*******/
   if (--COUNT<0)  goto *COMPILE[ci++] ;                                 /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
   goto *COMPILE[ci++] ;

  xcsumend :                       /* finish DOT a = x*1, test loop */

   relval[gjj]+=relval[YBASE+COUNT] ;
   if (--COUNT<0) goto *COMPILE[ci++] ;                                      /* quit */
   ci=cmark ; di=dmark ;                                /* loop return */
   goto *COMPILE[ci++] ;


  xcmax :                                         /* winner-take-all */

  /** register int  I, Imax ;  double  MAX ;**/

   Imax=I=YBASE+NROW1-1 ; MAX=relval[I] ; relval[I]=0 ;

   while (--I>=YBASE) {
      if (MAX<relval[I]) { MAX=relval[I] ; Imax=I ; }
      relval[I]=0 ;
      }
   relval[Imax]=MAX ;
   goto *COMPILE[ci++] ;


  xcMAX :                                         /* winner-take-all */

  /** register int  I, Imax, VBASE ;  double  MAX ;  **/
   
   VBASE=dptr[di++] ;
   Imax=I=VBASE+dptr[di++]-1 ; MAX=relval[I] ; relval[I]=0 ;

   while (--I>=VBASE) {
      if (MAX<relval[I]) { MAX=relval[I] ; Imax=I ; }
      relval[I]=0 ;
      }
   relval[Imax]=MAX ;
   goto *COMPILE[ci++] ;
/* ---------------------------------------------------------------------

************>           CLEARN y=W(x)lrate,crit               <**********

                simple competitive learning for crit<0
                pseudo-ART for crit>0
                FSCL (modified "conscience") for crit=0
                Oja's algorithm for lrate<0, crit=0

     Template matrix W has NROW1 rows, XCOL1 columns.  XBASE, YBASE,
     WBASE are indices of x[1], y[1], W[1,1] in the relval[] data array.

     DESIRE must declare an auxiliary array, say h[n], immediately after
     y[n] (as in ARRAY ...,y[n], h[n], ...) for FSCL and pseudo-ART.
     h[n] is initialized to 0.

     NROW1 and YBASE are global variables whose values are *COMPILEd
     before cclearn() is called.  XCOL1, WBASE, XBASE, and the relval[]
     indices of the learning rate lrate, the vigilance flag/parameter
     crit, and the fast-learn flag fast are *COMPILEd into the data-
     index array dptr[]; fast learn is on by following crit with "#".

     ----------------------------------------------------------------- */

     xcclearn :          

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
        if (CRIT==0) sum*=relval[YBASE+NROW1+i] ; /* "conscience", for FSCL only */
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
     
goto *COMPILE[ci++] ;

  xcbarf :                                       /* INTP operation */

  /**   register int  i, k, rowbase, WBASE, XCOL1, XBASE ;
     double  lrate, thresh, sum ;   **/

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
goto *COMPILE[ci++] ;

  xcconv :                                       /* shift operation */

  /** register int XBASE, indexx ; **/

   XBASE=dptr[di++] ;
   indexx=dptr[di++]+COUNT ;
   if (indexx<NROW1 && indexx>=0) relval[--gjj]=relval[XBASE+indexx] ;
   else relval[--gjj]=0 ;
  goto *COMPILE[ci++] ; 


  xcplearn :                                 /* PLEARN y=W*x;lrate */

/* ---------------------------------------------------------------------
               - learns principal-components transformation

     The weight matrix W has NROW1 rows, XCOL1 columns.  XBASE, YBASE,
     WBASE are indices of x[1], y[1], W[1,1] in the relval[] data array.

     NROW1 and YBASE (dimension and base of the vector y) are global
     variables whose values are determined by cyinit() before cplearn() is
     called.  The dimension XCOL1 of the vector x, the bases WBASE, XBASE,
     and the relval[] index of the learning rate lrate are *COMPILEd into
     the data-index array dptr[].

     ----------------------------------------------------------------- */

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
     goto *COMPILE[ci++] ;   

xcshowstrt : showstrt() ;  goto *COMPILE[ci++] ;  

xccshow : cshow() ; goto *COMPILE[ci++] ; 
     
 /*------------------------------ end of label code ------------------------------------*/

    }                 /* end of drun00()   */
