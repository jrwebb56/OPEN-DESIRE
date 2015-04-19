/* COMPLX.C  Complex-expression Interpreter Routines for DESIRE/X   */
/*             Registered Copyright 1991 G.A. Korn          1/25/91 */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/* ---------------------------------------------------------------- */

#include "declare.h"
#include "global.h"

extern void  derror() ;      extern  ptr  getvar() ;
extern  int  getsub() ;
extern  double getoprnd() ;  extern double evalexp() ;


void cvalexp(A,B)                               /* complex expression */

       double  *A,*B ; {                 /* VAR parameters in PASCAL! */

       int  lcount,prcnt,level,gisav,gsav1,wsav1,fsav1,opflg ;
       double  a,b,arg,sign,val,val1 ;  ptr  p,pp ;

       prcnt=0 ; level=flevel ; opflg=FALSE ;

L15 :  warea[++wptr]=blank ;                      /* initialize stack */

                                          /* check for unary operator */

       if (warea[gi]=='-') { a=0 ; b=0 ; goto L50 ; }
       if (warea[gi]=='+') goto L60 ; goto L10 ;

                        /* push the new operator and operand on stack */

L50 :  warea[++wptr]=warea[gi] ;

       if (gjj<=gir+2) derror(40) ;
       relval[--gjj]=a ; relval[--gjj]=b ;

L60 :  gi++ ;                                    /* get next operator */

L10 :  if (warea[gi]=='(') { prcnt++ ; gi++ ; goto L15 ; }

/* ------------------------------------------------------------------ */
/*      the following puts real and imaginary parts into a and b      */
/* ------------------------------------------------------------------ */

       if (warea[gi]!='[') goto L110 ; gi++ ;               /* skip ] */
                                                /* try for [a,b] form */
       a=evalexp() ; if (errnum>0) derror(errnum) ;
       if (warea[gi++]!=',') derror(75) ;                   /* skip , */
       b=evalexp() ; if (errnum) derror(errnum) ;
       if (warea[gi++]!=']') derror(75) ;                   /* skip ] */
       goto L25 ;

L110 : gisav=gi ;
       gsav1=gjj ; wsav1=wptr ; fsav1=flevel ;         /* save stacks */
       a=getoprnd() ;                                /* REAL operand? */
       if (errnum==0) { b=0 ; goto L25 ; }                     /* yes */
       if (errnum!=31) derror(errnum) ;

       errnum=0 ;                /* it is a symbolic COMPLEX variable */
       gjj=gsav1 ; wptr=wsav1 ;                     /* restore stacks */
       flevel=fsav1 ;                           /* and function level */
       gi=gisav ; p=getvar() ; if (errnum) derror(errnum) ;
       if (p==NULL) {
          if (vartyp<0) derror(19) ;               /* undefined array */
          derror(200) ;                         /* undefined variable */
          }

       if (p->symtype==3) {
          a=relval[p->valptr] ; b=relval[p->valptr+1] ;
          }
       else if (p->symtype== -3) {
          gisav=intval[intval[p->valptr]+p->valptr+1]+2*getsub(p) ;
          a=relval[gisav] ; b=relval[gisav+1] ;
          }
       else if (p->symtype==0) {
          gi++ ;                                  /* function, skip ( */

          if (p->valptr>0) {                 /* user-defined function */
             p->SWITCH=FALSE ;                   /* prevent recursion */
             pp=p->next ;                /* pp points at 1st argument */

             while (pp!=NULL && warea[gi]!=')') {

                if (pp->symtype==1) {
                   relval[pp->valptr]=evalexp() ;
                   if (errnum) derror(errnum) ;
                   }
                else if (pp->symtype==3)
                    cvalexp(&relval[pp->valptr],&relval[pp->valptr+1]) ;

                if (warea[gi]==',') gi++ ;
                pp->SWITCH=TRUE ;           /* turn local variable ON */
                pp=pp->next ;
                }                                            /* while */

             if (pp!=NULL || warea[gi]!=')') derror(56) ;
             gi++ ;
             lcount=gi ;             /* save return, no stack needed! */
             gi=p->valptr ;              /* call the function routine */

             if (tracer==2 || tracer==3 || tracer>=6) {
                printf("%c%s",'\33',"[7m") ;    /* ANSI reverse video */
                printf("G: EXECUTING DEFINED FUNCTION %s",p->name) ;
                printf("%c%s",'\33',"[0m\n") ;  /* ANSI restore video */
                }
             cvalexp(&a,&b) ;
             gi=lcount ;                                    /* return */
             p->SWITCH=TRUE ;                 /* undo recursion check */

             pp=p->next ;                 /* turn local variables OFF */
             while (pp!=NULL) {
                pp->SWITCH=FALSE ;
                pp=pp->next ;
                }
             }                                    /* if (p->valptr>0) */

          else {                        /* p->valptr<=0, recursion or */
             flevel++ ;                           /* library function */
             cvalexp(&arg,&sign) ;

             switch(p->valptr) {

    /*exp*/ case  -5  : a=exp(arg)*cos(sign) ; b=exp(arg)*sin(sign) ;
                        break ;

 /*Conjug*/ case -18  : a=arg ; b= -sign ; break ;
   /*expj*/ case -19  : a=cos(arg) ; b=sin(arg) ; break ;

              default : strcpy(symbol,p->name) ; derror(200) ;
              }                                             /* switch */

             if (warea[gi++]!=')') derror(7) ;              /* skip ) */
             flevel-- ;
             }                               /* else library function */
          }                                         /* if (symtype=0) */
       else { strcpy(symbol,p->name) ; derror(84) ; }

/* ------------------------------------------------------------------ */

L25 :  if (warea[gi]!='+' && warea[gi]!='-') goto L30 ;
       if (warea[wptr]==blank) goto L50 ; /* push lower-prcdnce oprtr */

L40 :  do {                           /* compute until stack is empty */

          switch (warea[wptr]) {

         case '+' :   a=relval[gjj+1]+a ; b=relval[gjj]+b ; break ;
         case '-' :   a=relval[gjj+1]-a ; b=relval[gjj]-b ; break ;

         case '*' :   val1=a ;
                      a=relval[gjj+1]*a-relval[gjj]*b ;
                      b=relval[gjj+1]*b+relval[gjj]*val1 ; break ;

         case '/' :   val=a*a+b*b ; val1=a ;
                      if (val==0) derror(5) ;

                      a=(relval[gjj+1]*a+relval[gjj]*b)/val ;
                      b=(relval[gjj]*val1-relval[gjj+1]*b)/val ; break ;

          default :   derror(75) ;
             }                                              /* switch */

          gjj+=2 ;
          } while(warea[--wptr]!=blank) ;                       /* do */

       if (opflg) goto L70 ; goto L50 ;

L30 :  if (warea[gi]=='*' || warea[gi]=='/') {
          while (warea[wptr]!=blank) {  /* until operator stack empty */
             if (warea[wptr]=='*') {
                val1=a ;
                a=relval[gjj+1]*a-relval[gjj]*b ;
                b=relval[gjj+1]*b+relval[gjj]*val1 ;
                }
             else if (warea[wptr]=='/') {
                val=a*a+b*b ; val1=a ;
                if (val==0 ) derror(5) ;
                a=(relval[gjj+1]*a+relval[gjj]*b)/val ;
                b=(relval[gjj]*val1-relval[gjj+1]*b)/val ;
                }
             else goto L50 ;
             gjj+=2 ; wptr-- ;
             }                                               /* while */
          goto L50 ;
          }                                                     /* if */
       if (warea[gi]=='^') derror(75) ;
       if (warea[wptr]!=blank) { opflg=TRUE ; goto L40 ; }

L70 :  wptr-- ;                   /* pop operator from operator stack */
       if (warea[gi]==')') {
          if (level==flevel && prcnt!=0) {
             prcnt-- ;
             gi++ ;
             opflg=FALSE ;
             goto L25 ;
             }
          }
       if (prcnt!=0) derror(7) ;
       *A=a ; *B=b ;
       }                                                   /* cvalexp */

