/* EXPR.C     Expression-interpreter Routines DESIRE - UNIX            */
/*            Registered Copyright 1993 G.A. Korn              9/13/93 */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/**            fixed sinc() ; set up for GNU                    6/5/97 */

/* ------------------------------------------------------------------- */

#include <math.h>                      /** for GNU **/
#include <stdlib.h>
#include <time.h>

#include "declare.h"
#include "global.h"

extern  void  cvalexp() ;  extern  void  derror() ;
extern  ptr  getvar() ;

double evalexp() ;                              /* forward declaration */


double ROUND(arg)                   /* returns rounded double argument */

      double arg ; {
      int rj ;

      if (arg>=maxint || arg<= -maxint) derror(20) ;
      if (arg>0) { rj=arg+0.5 ; return rj ; }
      else if (arg<0) { rj=0.5-arg ; return -rj ; }
      else return 0 ;
      }                                                       /* ROUND */


double hotode() {                              /* octal or hex to REAL */
                                                    /* USED BY evalexp */
   double val ;

      val=0 ;
      if (warea[++gi]=='&') goto L10 ;                          /* hex */

      while (warea[gi]>='0' && warea[gi]<'8') {               /* octal */
         val=8*val+warea[gi]-'0' ;
         if (val>65535.0) derror(57) ;
         gi++ ;
         }
      goto L12 ;

L10 : gi++ ;                                            /* hex, skip $ */
      if (warea[gi]>='0' && warea[gi]<='9') val=16*val+warea[gi]-'0' ;
      else if (warea[gi]>='A' && warea[gi]<='F') val=16*val+warea[gi]-55 ;
      else if (warea[gi]>='a' && warea[gi]<='f') val=16*val+warea[gi]-87 ;
      else goto L12 ;
      if (val>65535.0) derror(57) ;
      goto L10 ;

L12 : if (val>32767) return val-65536.0 ; /* signed, NOT up to maxint! */
      else return val  ;
      }                                                      /* hotode */


int getsub(q)      /* compute index for array, with subscripts NOT    */
        ptr q ; {     /* reversed ; on entry, gi points to [, on exit */
                   /* to char. after ] .  USED BY evalexp, cvalexp,   */
                   /* execute, compiler                               */

      int  val, lj, maxlj ;  double  xval ;

      val=0 ; gi++ ;                                     /* skip '['  */
      lj=q->valptr ;                 /* lj points to the number ...   */
      maxlj=lj+intval[lj] ;         /*   ...  intval[lj] of dimensions*/

L10 : if (++lj>maxlj) derror(69) ;     /* lj points to next dimension */
      xval=evalexp() ; if (errnum) derror(errnum) ;
      if (xval<=0 || xval>intval[lj]) derror(2) ;   /* index overflow */
      val=val*intval[lj]+(int) xval-1 ;       /* [1,1,...] is index 0 */
                                                  /* note TRUNC(xval) */
      if (warea[gi]==',') { gi++ ; goto L10 ; }  /* another dimension */

      if (lj<maxlj || warea[gi++]!=']') derror(53) ;      /* skip ']' */
      return val ;
      }                                                     /* getsub */


double getoprnd() {       /* evaluate operand ; on entry, gi points to */
                           /* first character, on exit to char. after */
                                          /* USED BY evalexp, cvalexp */
         int  lj, lcount ;  ptr  p, pp ;
         double  arg, sign, arg1 ;

      if (warea[gi]<'0' || warea[gi]>'9') {
         if (warea[gi]!='.') goto L30 ;              /* not a literal */
         arg=0 ; goto L10 ;                               /* fraction */
         }
      arg=0 ;                          /* get integer part of literal */
      do arg=arg*10+warea[gi++]-'0' ;
      while (warea[gi]>='0' && warea[gi]<='9') ;
      if (warea[gi]!='.') goto L20 ;

L10 : sign=1 ;                                        /* get fraction */
L11 : gi++ ;
      if (warea[gi]<'0' || warea[gi]>'9') goto L20 ;
      sign*=0.1 ;
      arg+=sign*(warea[gi]-'0') ;
      goto L11 ;                                              /* loop */

L20 : if (warea[gi]=='E' || warea[gi]=='e') {             /* E-format */
        lcount=0 ;
        if (warea[gi+1]=='+') sign=10 ;
        else if (warea[gi+1]=='-') sign=0.1 ;
        else derror(54) ;

        gi+=2 ;
        while (warea[gi]>='0' && warea[gi]<='9')
                         lcount=lcount*10+warea[gi++]-'0' ;
        for (lj=1 ; lj<=lcount ; lj++) arg*=sign ;
        }                                             /* if E-format */
      return arg ;
                                   /* operand is a symbolic variable */

L30 : p=getvar() ; if (errnum) return 0 ;       /* for cvalexp etc.! */
      if (p==NULL) {
         if (vartyp<0) derror(19) ;                 /* undimensioned */
         else derror(200) ;                    /* undefined variable */
         }
                                   /* variable is previously defined */

      if (p->symtype==1)  return relval[p->valptr] ;
      else if (p->symtype== -1)
         return relval[intval[intval[p->valptr]+p->valptr+1]+getsub(p)] ;
      else if (p->symtype==2) return intval[p->valptr] ;
      else if (p->symtype== -2)
         return intval[intval[p->valptr]+p->valptr+getsub(p)+1] ;

      else if (p->symtype==0) {                  /* it's a function */
         gi++ ;                                            /* skip ( */
         if (p->valptr>0) {                 /* user-defined function */
            p->SWITCH=FALSE ;                   /* prevent recursion */
            pp=p->next ;                /* pp points at 1st argument */

            while (pp!=NULL && warea[gi]!=')') {
               if (pp->symtype==1) relval[pp->valptr]=evalexp() ;
               else if (pp->symtype==2) {
                  arg=evalexp() ; if (errnum) goto L33 ;
                  if (arg>maxint || -arg>maxint) {
                     errnum=20 ; goto L33 ;
                     }
                  intval[pp->valptr]=arg ;              /* truncated */
                  }
               else errnum=31 ;

L33 :          if (errnum) { p->SWITCH=TRUE ; return 0 ; }
               if (warea[gi]==',') gi++ ;
               pp->SWITCH=TRUE ;           /* turn local variable ON */
               pp=pp->next ;
               }                                            /* while */

            if (pp!=NULL || warea[gi]!=')') derror(56) ;
            lcount=gi ;            /* save return - no stack needed! */
            gi=p->valptr ;              /* call the function routine */

            if (tracer==2 || tracer==3 || tracer>=6) {
               printf("%c%s",'\33',"[7m") ;    /* ANSI reverse video */
               printf("G: EXECUTING DEFINED FUNCTION %s",p->name) ;
               printf("%c%s",'\33',"[0m") ;    /* ANSI restore video */
               }
            arg1=evalexp() ;         /* execute the function routine */
            if (errnum) { p->SWITCH=TRUE ; return 0 ; }

            p->SWITCH=TRUE ;                 /* undo recursion check */
            gi=lcount ;                          /* return gi to ')' */
            pp=p->next ;                 /* turn local variables OFF */
            while (pp!=NULL) {
               pp->SWITCH=FALSE ;
               pp=pp->next ;
               }
            }                                      /* if p->valptr>0 */

         else {                    /* p->valptr<=0, library function */
            flevel++ ;
            if (warea[gi]==')' && p->valptr== -11) ;        /* ran() */
            else if (p->valptr> -14 || p->valptr< -18) arg=evalexp() ;
            else cvalexp(&arg,&sign) ; if (errnum) return 0 ;

            switch (p->valptr) {

        /*ln*/  case -1  : if (arg<=0) derror(9) ; arg1=log(arg) ; break ;
       /*tim*/  case -2  : arg1=(double)clock()/1.0E+06 ; break ;
       /*sin*/  case -3  : arg1=sin(arg) ; break ;
       /*cos*/  case -4  : arg1=cos(arg) ; break ;
       /*exp*/  case -5  : arg1=exp(arg) ; break ;
       /*abs*/  case -6  : arg1=fabs(arg) ; break ;
      /*sqrt*/  case -7  : if (arg<0) derror(47) ; arg1=sqrt(arg) ; break ;

      /*trnc*/  case -8  : if (arg>maxint || arg< -maxint) derror(20) ;
                           arg1=lcount=arg ; break ;

       /*lim*/  case -9  : if (arg>0) arg1=arg ; else arg1=0 ; break ;

      /*sgn*/  case -10  : if (arg<0) arg1= -1 ; else if (arg>0) arg1=1 ;
                           else arg1=0 ; break ;

      /*ran*/  case -11  : arg1=2*drand48()-1.0 ;

/**** 2*d_lcran_()-1.0 ;    ****/   

                /*** arg1=(double)rand()*ranscale-1.0 ; **** GNU ***/

                            break ;

     /*atan*/  case -12  : arg1=atan(arg) ; break ;
    /*round*/  case -13  : arg1=ROUND(arg) ; break ;
       /*Re*/  case -14  : arg1=arg ; break ;
       /*Im*/  case -15  : arg1=sign ; break ;
     /*Cabs*/  case -16  : arg1=sqrt(arg*arg+sign*sign) ; break ;

      /*Arg*/  case -17  : arg1=atan2(sign,arg) ; break ;
   /*Conjug*/  case -18  :
     /*expj*/  case -19  :  errnum=31 ; return 0 ;

      /*log*/  case -20  : if (arg<=0) derror(9) ; arg1=log10(arg) ; break ;
      /*tan*/  case -21  : arg1=tan(arg) ; break ;
     /*sinh*/  case -22  : arg1=sinh(arg) ; break ;
     /*cosh*/  case -23  : arg1=cosh(arg) ; break ;
     /*acos*/  case -24  : arg1=acos(arg) ; break ;
     /*asin*/  case -25  : arg1=asin(arg) ; break ;
     /*tanh*/  case -26  : arg1=tanh(arg) ; break ;

     /*atan2*/ case  -27 : if (warea[gi++]!=',') derror(44) ;
                           sign=evalexp() ; if (errnum) derror(errnum) ;
                           arg1=atan2(arg,sign) ;
                           break ;

      /*swtch*/ case -28 : if (arg>0) arg1=1 ; else arg1=0 ; break ;

      /*SAT*/   case -29 : if (arg<0) arg1=0 ; else if (arg>1)
                                     arg1=1 ; else arg1=arg ; break ;

                                           /* -30 is for NEUNET */

      /*sat*/   case -31 : if (arg< -1) arg1= -1 ; else if (arg>1)
                                     arg1=1 ; else arg1=arg ; break ;

     /*deadz*/  case -32 : if (arg< -1) arg1=arg+1 ; else if (arg>1)
                                   arg1=arg-1 ; else arg1=0 ; break ;

     /*deadc*/  case -33 : if (arg< -1) arg1= -1 ; else if (arg>1)
                                       arg1=1 ; else arg1=0 ; break ;

      /*rect*/  case -34 : if (arg<1 && arg> -1) arg1=1 ;
                           else arg1=0 ; break ;

      /*comp*/  case -35 : if (warea[gi++]!=',') derror(44);
                           arg1=evalexp() ; if (errnum) derror(errnum) ;
                           if (warea[gi++]!=',') derror(44) ;
                           sign=evalexp() ; if (errnum) derror(errnum) ;
                           if (arg>0) arg1=sign ; break ;

       /*tri*/  case -36 : if (arg<0) arg1=arg+1 ;
                           else arg1=1-arg ; break ;

   /*sigmoid*/  case -37 : arg1=1/(exp(-arg)+1) ; break ;

   /*SIGMOID*/  case -38 : if (arg>0) arg1=arg*arg/(arg*arg+1) ;
                           else arg1=0.0 ; break ;

      /*sinc*/  case -39 : if (arg==0) arg1=1 ;
                           else arg1=sin(arg)/arg ; break ;

                  default : strcpy(symbol,p->name) ; derror(200) ;
                                               /* undefined function */
               }                               /* switch (p->valptr) */

            if (warea[gi]!=')') derror(7) ;             /* missing ) */
            flevel-- ;
            }                               /* else library function */

         gi++ ;                                           /* skip  ) */
         return arg1 ;
         }                               /* if ( symtype=0, function */
      else { errnum=31 ; return 0 ; }
      }                                                   /* getoprnd*/


double evalexp() {                     /* evaluate a REAL expression */

         int  prcnt, level, opflg ;  double  accum ;

      if (warea[gi]=='&') return hotode() ;                   /* hex */

      prcnt=0 ; level=flevel ; opflg=FALSE ;
L15 : warea[++wptr]=blank ;                      /* initialize stack */

      if (warea[gi]=='-') { accum=0 ; goto L50 ; }   /* unary oprtr? */
      if (warea[gi]=='+') goto L60 ; goto L10 ;

                      /* push the new operator and operand on stacks */

L50 : warea[++wptr]=warea[gi] ;
      relval[--gjj]=accum ; if (gjj<=gir) derror(40) ;

L60 : gi++ ;                                    /* get next operator */

L10 : if (warea[gi]=='(') { prcnt++ ; gi++ ; goto L15 ; }

      accum=getoprnd() ; if (errnum) return 0 ;       /* get operand */

L25 : if (warea[gi]!='+' && warea[gi]!='-') goto L30 ;
      if (warea[wptr]==blank) goto L50 ; /* push low-precednce oprtr */
L40 : do {                           /* compute until stack is empty */

         switch (warea[wptr]) {

 case '+' : accum+=relval[gjj++] ; break ;
 case '-' : accum=relval[gjj++]-accum ; break ;
 case '*' : accum*=relval[gjj++] ; break ;
 case '/' : if (accum==0) derror(5) ; accum=relval[gjj++]/accum ; break ;

 default  : if (accum==2) { accum=relval[gjj]*relval[gjj] ; gjj++ ; }
            else {
               if (relval[gjj]<=0) derror(9) ;
               accum=exp(accum*log(relval[gjj++])) ;
               }
            break ;
            }                                              /* switch */

         } while (warea[--wptr]!=blank) ;            /* ends do loop */

      if (opflg) goto L70 ; goto L50 ;

L30 : if (warea[gi]=='*' || warea[gi]=='/') {
         while (warea[wptr]!=blank) {     /* until oprtr stack empty */
            if (warea[wptr]=='*') accum*=relval[gjj++] ;
            else if (warea[wptr]=='/') {
               if (accum==0) derror(5) ; accum=relval[gjj++]/accum ;
               }
            else if (warea[wptr]=='^') {
               if (accum==2) { accum=relval[gjj]*relval[gjj] ; gjj++ ; }
               else {
                  if (relval[gjj]<=0) derror(9) ;
                  accum=exp(accum*log(relval[gjj++])) ;
                  }
               }
            else goto L50 ;
            wptr-- ;
            }                                               /* while */
         goto L50 ;
         }                                                     /* if */
      if (warea[gi]=='^') goto L50 ;
      if (warea[wptr]!=blank) { opflg=TRUE ; goto L40 ; }

L70 : wptr-- ;                   /* pop operator from operator stack */
      if (warea[gi]==')') {
         if (level==flevel && prcnt!=0) {
            prcnt-- ;
            gi++ ;
            opflg=FALSE ;
            goto L25 ;
            }
         }
      if (prcnt!=0) derror(7) ; return accum ;
      }                                                  /* evalexpr */
