/* VECDOT.C      Interpreter matrix operations I                       */
/*             Registered Copyright 1998  G.A. Korn            4/17/93 */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/* new dot                                                     9/14/98 */
/* ------------------------------------------------------------------- */

#include "declare.h"
#include "global.h"

extern void derror() ;    extern void cvalexp() ;   extern void matprt() ;

extern ptr getvar() ;     extern ptr instal() ;     extern int getsub() ;
extern double evalexp() ; extern double getoprnd() ;



void vector() {

      int  i,k,dim,nrow,ncol,start0,start1,start2,itemp,first,minus ;
      double  val ;  ptr  qy ;

      start=1 ;                                     /* makes vartyp=-1 */
      p=getvar() ; if (errnum) derror(errnum) ;
      if (p==NULL || p->symtype!=-1) derror(70) ; /* undef. or un-REAL */
      i=p->valptr ;
      if (intval[i]!=1) derror(74) ;                      /* no vector */
      nrow=intval[i+1] ; start0=intval[i+2];

      if (warea[gi++]!='=') derror(29) ;                  /* no = sign */

      if (warea[gi]=='0') {                                /* vector=0 */
         for (k=start0 ; k<start0+nrow ; k++) relval[k]=0 ;
         gi++ ;                                              /* skip 0 */
         goto L5 ;                                            /* done! */
         }

      first=TRUE ;                                  /* first-pass flag */
      if (warea[gi]=='-') { minus=TRUE ; gi++ ; }
      else minus=FALSE ;               /* deal with minus sign, if any */

L10 : i=gi ; start=0 ;
      val=getoprnd() ;                    /* try for scalar multiplier */
      if (errnum) goto L15 ;                             /* not scalar */
      if (minus) val=-val ;
      if (warea[gi++]!='*') derror(75) ;

      start=1 ;                     /* prepare to multiply scalar ...  */
      qy=getvar() ; if (errnum) derror(errnum) ;   /*  ... by a vector */
      if (qy==NULL || qy->symtype!=-1) derror(70) ;  /* undef, notREAL */
      if (qy==p) derror(85) ;                     /* illegal recursion */
      i=qy->valptr ;
      if (intval[i]!=1 || intval[i+1]!=nrow) derror(74) ;
      start1=intval[i+2] ;

      if (first) for (k=0 ; k<nrow ; k++)
                               relval[start0+k]=val*relval[start1+k] ;
      else for (k=0 ; k<nrow ; k++)
                              relval[start0+k]+=val*relval[start1+k] ;
      goto L20 ;

L15 : if (errnum!=73) derror(errnum) ;
      errnum=0 ; start=1 ;
      gi=i ;                    /* no scalar, try for vector or matrix */

      qy=getvar() ; if (errnum) derror(errnum) ;
      if (qy==NULL || qy->symtype!=-1) derror(70) ;
      if (qy==p) derror(85) ;                     /* illegal recursion */
      i=qy->valptr ; dim=intval[i] ;
      if (dim>2 || intval[i+1]!=nrow) derror(74) ;
      if (dim==1) {                                /* vector-only term */
         start1=intval[i+2] ;
         if (first) {
            if (minus) for (k=0 ; k<nrow ; k++)
                     relval[start0+k]= -relval[start1+k] ;
            else for ( k=0 ; k<nrow ; k++)
                     relval[start0+k]=relval[start1+k] ;
            }                                            /* if ( first */
         else if (minus) for (k=0 ; k<nrow ; k++)         /* not first */
                     relval[start0+k]-=relval[start1+k] ;
         else for (k=0 ; k<nrow ; k++)
                     relval[start0+k]+=relval[start1+k] ;
         }                                               /* if ( dim=1 */

      else if (dim==2) {                          /* matrix multiplier */
         ncol=intval[i+2] ; start1=intval[i+3] ;
         if (warea[gi++]!='*') derror(75) ;                  /* skip * */

         qy=getvar() ; if (errnum) derror(errnum) ;    /* next operand */
         if (qy==NULL || qy->symtype!=-1) derror(70) ;
         if (qy==p) derror(85) ;                  /* illegal recursion */
         i=qy->valptr ;
         if (intval[i]!=1 || intval[i+1]!=ncol) derror(74) ;
         start2=intval[i+2] ;

         if (minus) for (k=0 ; k<nrow ; k++) {
            itemp=start1+k*ncol ;                   /* base of kth row */
            if (first) val=0 ; else val=relval[start0+k] ;
            for (i=0 ; i<ncol ; i++)
                     val-=relval[itemp+i]*relval[start2+i] ;
            relval[start0+k]=val ;
            }

         else for (k=0 ; k<nrow ; k++) {
            itemp=start1+k*ncol ;
            if (first) val=0 ; else val=relval[start0+k] ;
            for (i=0 ; i<ncol ; i++)
                      val+=relval[itemp+i]*relval[start2+i] ;
            relval[start0+k]=val ;
            }
         }                                 /* ELSE  matrix multiplier */

L20 : if (warea[gi]=='+') minus=FALSE ;
      else if (warea[gi]=='-') minus=TRUE ;
      else goto L5 ;
      first=FALSE ;
      gi++ ; goto L10 ;

L5 :  start=0 ;                                               /* done */
      if (tracer>=4 && !errnum) {
         printf("%c%s",'\33',"[7m") ;           /* ANSI reverse video */
         matprt() ;
         printf("%c%s",'\33',"[0m") ;           /* ANSI restore video */
         }
      }                                                     /* vector */



double vaik(arow,acol,abase,xxbase,k)                         /* A*x */
int arow,acol,abase,xxbase,k ; {
      int j ;  double val ;   
      val=0.0 ; for (j=0;j<acol;j++)
               val+=relval[abase+k*acol+j]*relval[xxbase+j] ;
               return val ;
      }

      
double vaki(arow,acol,abase,xxbase,k)                        /* A%*x */
int arow,acol,abase,xxbase,k ; {
      int j ; double val ;
      
      val=0.0 ; for (j=0;j<arow;j++) 
               val+=relval[abase+j*acol+k]*relval[xxbase+j] ;
               return val ; 
      }


void dot() {                               /* DOT, starts out like LET */
                                                
      int  i, k, NROW, NCOL, XBASE,AROW, ACOL, ABASE, XXBASE, sub ;
      double  val ;  ptr  px, p, q, r ;

      px=getvar() ; if (errnum) derror(errnum) ; /* address the result */
      if (vartyp==0) derror(84) ;
      if (px==NULL) px=instal() ;
      else if (px->symtype!=1 || px->symtype!=-1) derror(84) ;
      if (vartyp<0) sub=getsub(px) ;

      if (warea[gi++]!='=') derror(29) ;                     /* skip = */
      
      start=1 ; val=0 ;                          /* arrays only; sum=0 */
      
L10 : p=getvar() ; if (errnum) derror(errnum) ;    /* get first vector */
      if (p==NULL) derror(70) ;
      i=p->valptr ; if (intval[i]!=1 || p->symtype!=-1) derror(84) ;
      NROW=intval[i+1] ;                            /* number of terms */
      XBASE=intval[i+2] ;                /* index base of first vector */   

      if (warea[gi++]!='*') derror(60) ;                    /* check * */

      if (warea[gi]=='1') {                           /* a simple sum! */
         for (k=0; k<NROW ; k++) val+=relval[XBASE+k] ;                       
         gi++ ; goto L20 ;
         }
         
      q=getvar() ; if (errnum) derror(errnum) ;        /* x or A next? */
      if (q==NULL || q->symtype!=-1) derror(70) ;
      i=q->valptr ;

      if (intval[i]==1) {                  /* multiplier is a vector x */
         if (intval[i+1]!=NROW) derror(74) ;        /* not conformable */
         for (k=0; k<NROW ; k++) 
                       val+=relval[XBASE+k]*relval[intval[i+2]+k] ;        
         }
             
      else {                            /* multiplier is matrix*vector */
         if (intval[i]!=2) derror(84) ;         /* not a matrix, error */
         AROW=intval[i+1] ;  ACOL=intval[i+2] ; ABASE=intval[i+3] ;                   

         if (warea[gi]=='*') {
            gi++ ;                                 /* A*x term, skip * */
            if (AROW!=NROW) derror(74) ;            /* must match rows */

            r=getvar() ; if (errnum) derror(errnum) ;    /* get vector */
            if (r==NULL || r->symtype!=-1) derror(70) ;
            i=r->valptr ;
            if (intval[i]!=1 || intval[i+1]!=ACOL) derror(74) ; 
            XXBASE=intval[i+2] ;             
            
            for (k=0; k<NROW ; k++)                         
               val+=relval[XBASE+k]*vaik(AROW,ACOL,ABASE,XXBASE,k) ;                                                               
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
                               
           for (k=0; k<NROW ; k++) 
                      val+=relval[XBASE+k]*vaki(AROW,ACOL,ABASE,XXBASE,k) ; 
           }                      
        }

L20 : if (warea[gi]=='+') { gi++ ; goto L10 ; }
            
      if (px->symtype>0) relval[px->valptr]=val ;
      else relval[intval[intval[px->valptr]+px->valptr+1]+sub]=val ;
      if (tracer>=4) {
         printf("%c%s",'\33',"[7m") ;            /* ANSI reverse video */
         printf("A: %s = $g",px->name,val) ;
         printf("%c%s",'\33',"[0m") ;            /* ANSI restore video */
         }
      start=0 ;
      }                                                   



