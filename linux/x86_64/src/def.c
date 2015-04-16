/* DEF.C      Declarations of FUNCTION, PROCEDURE, MACRO,                */
/*             INTEGER, COMPLEX, dimension, ARRAY, and STATE                 */

/*             Registered Copyright 2012 G.A. Korn      original file  5/28/94 */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/
/*             added Error (40)check under L30                 9/5/98  */

/* new STATE/equivalence declarations       2/14/12    */
/* ------------------------------------------------------------------- */

#include "declare.h"
#include "global.h"
#include <string.h>


extern void derror() ;  extern void seekx() ;
extern ptr getvar() ;   extern ptr instal() ;   extern double evalexp() ;


void def(int i)         /* for i=0,1,-1 defines/links FUNCTION, PROCEDURE, */
{                            /*  or MACRO name and args */
    
    int  gkk, dim, count ;  ptr  p, pp, q ;
    
    if (flag1) derror(88) ;                               /* nested! */
    p=getvar() ; if (errnum) derror(errnum) ;
    if (!runf || vartyp!=0) derror(37) ;
    if (p!=NULL) derror(55) ;                           /* duplicate */
    p=instal() ;  gir-- ;                         /* un-REAL install */
    q=p ;                                       /* saves p for later */
    
    count=dim=0 ;              /* count semicolons, value-parameters */
    if (warea[gi+1]==';') { count=1 ; gi++ ; }           /* only VAR */
    else if (warea[gi+1]==')') {
	p->next=NULL ; gi+=2 ; goto L20 ;             /* no arguments */
    }
    
    L10 : if (count==0) dim++ ;                        /* skip (';' or ',' */
    gi++ ; gkk=gi ; pp=getvar() ;/* get argument; UNIX dislikes ++gi */
    if (vartyp<=0) derror(196) ;                        /* no ] or ) */
    if (errnum) {
        if (errnum==73) {                     /* is argument an array? */
	    errnum=0 ;
	    gi=gkk ; start=1 ; pp=getvar() ; start=0 ;
	    if (errnum) derror(errnum) ;
	    if (pp==NULL || pp->symtype!=-1) derror(196) ;
	}
        else derror(errnum) ;
    }                                               /* if (errnum) */
    
    if (pp==NULL) {                               /* REAL, define it */
	pp=instal() ;
	if (i<0 || count>0) { errnum=0 ; gir-- ; }   /* VAR parameter */
    }
    else if (pp->symtype==1) derror(71) ;           /* REAL, defined */
    else if (pp->symtype>3 || pp->symtype<-4) derror(196) ;  /* type */
    
    pp->SWITCH=FALSE ;                     /* turn local variable OFF */
    pp->next=NULL ; p->next=pp ; p=pp ;             /* link arguments */
    
    if (warea[gi]==',') goto L10 ;               /* another argument */
    if (warea[gi]==';') { count++ ; goto L10 ; }
    if (warea[gi++]!=')') derror(36) ;        /* all linked; skip ')' */
    
    L20 : if (count>1 || count>0 && i<=0) derror(196) ;
    if (i==0) {                                 /* FUNCTION statement */
	q->symtype=0 ;                                     /* function */
	if (warea[gi++]!='=') derror(46) ;                 /* skip '=' */
	q->valptr=gi ;                              /* record location */
	while (warea[gi]!=lf && warea[gi]!='|') gi++ ;
	if (warea[gi-1]=='=') derror(46) ;         /* no function code */
    }
    else {                            /* PROCEDURE or MACRO statement */
	if (warea[gi]!=lf && warea[gi]!='|') derror(33) ;
	q->valptr=gi ;
	if (i>0) q->symtype=8+dim ;                  /* i>0, PROCEDURE */
	else q->symtype=-59 ;                            /* i=1, MACRO */
	
	seekx() ;                  /* search for "end" and check pairs */
	if (errnum || warea[gi]!=vretrn) derror(58) ;
	if (warea[++gi]=='|') { if ( warea[gi+1]!=vrem) derror(62) ; }
    }
}                                                           /* def*/


void declare(int i) {      /* COMPLEX, ARRAY, INTEGER,  ...
                                            .. or STATE for i = -1,0,1,3          

   - this implements COMPLEX z1,z2,..., 
        ARRAY x[n,m,...], ARRAY x1[n1]+x2[n2]+... = x, 
           ARRAY W[n,m]=v,  INTEGER x,y,..., INTEGER x[n,m,...],                            
             STATE x,y,..., STATE x[n], STATE x1[n1]+x2[n2]+... =x, 
                                     and STATE W[n,m]=v                         **/     
                                                    
    int  count, ival, li, ivalsum, adrsav, sflag=FALSE ;
    int  j=106 ;                                                          /* complex j */
    double  val, expval ;  ptr  pp, qq ;

L8 :                                                                  /* install symbol */
    pp=getvar() ; if (errnum) derror(errnum) ;    
    if (pp!=NULL) derror(28) ;            /* multiple declaration */
    pp=(struct symnode*)malloc(sizeof(struct symnode)) ; 
    pp->SWITCH=TRUE ;
    pp->link=hashtab[hashval] ;
    strcpy(pp->name,symbol) ;
    hashtab[hashval]=pp ;
    
    if (vartyp<0) goto L10 ;                                           /* array */ 
    if (i==0) derror(25) ;                                   /* ARRAY, no [ */
    /*-----------------------------------------------------------------------*/
                                                                            /* it is scalar ! */
    
    if (i==3) {                                    /* scalar STATE variable */     		         
        pp->symtype=1 ;
	    if (nn1++>maxnsv) derror(100) ;    /* too many state vars */
	    pp->valptr=nn1 ; relval[nn1]=0 ;
        }
   else if (i<0) {          /* scalar COMPLEX number , define j */
         if (strcmp(pp->name,"j")==0) derror(28) ;   /* multiple declaration */
         pp->symtype=3 ;
         pp->valptr=gir ; relval[gir]=0 ;
         gir+=2 ; if (gir>=gjj-2) derror(40) ;          /* allows for j */
         relval[gir-1]=0 ;
         
         strcpy(symbol,"j") ;                /* note: depends on varsize */
	     if (hashtab[j]!=NULL) { if (hashtab[j]->symtype!=3) derror(28) ; }                                             
	     else {
              pp=(struct symnode*)malloc(sizeof(struct symnode)) ;
              hashtab[j]=pp ;
              pp->SWITCH=TRUE ;
              pp->link=NULL ;
              strcpy(pp->name,symbol) ;
              pp->symtype=3 ;
              pp->valptr=gir ;if (gir+1>=gjj) derror(40) ; /* out of memory */
              relval[gir++]=0 ; relval[gir++]=1 ;
	          }            
         }                                                                    /* end if COMPLEX */
    else{                                                                   /* scalar INTEGER */
	   pp->symtype=2 ;
	   pp->valptr=gii ; intval[gii]=0 ;
	   if (++gii>maxinvar) derror(41) ;              /* too many integers */
       }                                          
    goto L30 ;
    
    L10 :                                                                          /* it is an array! */
       if (i<0) derror(202) ;                               /* no COMPLEX arrays */
/* ----------------------------------------------------------------------------------- */
/* read array declaration, make table entries in intval[], relval[]  */
                                                                
/*  intval[pp->valptr]   number of dimensions (accumulated count)  */
/*                    ... followed by the individual dimensions                   */
/*                    ... followed by index to data in intval or relval           */
/*  gii indexes this table in intval[]; gir indexes data in relval[]    */
/* ------------------------------------------------------------------------------------ */

    pp->valptr=gii ;                         /* counts number of dimensions */
    if (++gii>=maxinvar) derror(41) ;        /* too many integers??  <**/
    count=0 ;                       
    val=1 ;
                                                                      /* gi indexes program text */
L20 :                           	       /* return here for each new dimension */
    gi++ ;  count++ ;                    /* skip '[' or ',' then read dimension */     
    expval=evalexp() ; if (errnum) derror(errnum) ;      /* dimension */
    if (count>maxdim || expval<1 || expval>=maxint) derror(80) ;
                        /* too many dimensions, or dimension out of range */
    if ((i==3)&&(count>2)) derror(69) ;         /* STATE dimension>2  */                         
                        
    intval[gii++]=(int)expval ;        /* next table item is a dimension */
    if (gii>=maxinvar) derror(41) ;               /* dimension is too large */
    val*=expval ;                 /* accumulate number of array elements */
    pp->symtype=-1 ;                  /* this will be revoked below  if i==1 */
    if (warea[gi]==',') goto L20 ;                 /*read another dimension */
    if (warea[gi++]!=']') derror(25) ;              /* array has been parsed */
 
/* ----------------------------------- */
    
    intval[pp->valptr]=count ;          /* number of dimensions */
    if (val>=maxint) derror(40) ;                       /* out of memory */
    ival=(int) val ;                           /* number of array elements */
    
                                                            /* what kind of array is it? */
                                
    if (i==1) {                                          /* INTEGER array, zero it */
	   pp->symtype=-2 ;
	   if (gii+ival-1>maxinvar) derror(41) ;
	   for (li=gii ; li<gii+ival ; li++) intval[li]=0 ;                         /* zero */
	   gii=li   ;       /* incremented for next integer; one redundant 0 */
	   goto L30 ; 
       }
                              /* not INTEGER, either ordinary or STATE array */                                                                           
     
/*--------------------------------------------------------------------------------------*/   
    if (i!=3) goto L31 ;                                           /* not a STATE array */
    
        /* STATE array! nn1 is current number of state variables, ... */
                                        /* ... NOT pre-incremented like gii and gir! */        
    intval[gii++]=nn1+1 ;                   /* nn1+1 points to 1st element */
    if (nn1+ival>maxnsv ) derror(100) ;         /* too many state vars */   
                 
	 if (!sflag) {                                                 /* no + yet, if any */
        adrsav=nn1+1 ;             /* save for STATE subvectors */
        ivalsum=ival ;            /* sum of subvector dimensions */
	    }
    else ivalsum+=ival ;           /* add subvector dimensions */
	
	if (nn1+ival>=gjj) derror(40) ;       /* zero out entire array */
    for (li=nn1+1 ; li<nn1+1+ival ; li++) relval[li]=0 ;
	nn1=li ;   
	nn1++ ;   /* points to next STATE item; ONE REDUNDANT 0 */
    if (gii>=maxinvar) derror(41) ;                 /* out of memory */
    
    goto L32 ;            /* check for equivalence specification */
/* --------------------------------------------------------------------------- */ 
                                                                          /* ordinary array */
 L31:                             
	 intval[gii++]=gir ;                   /* gir points to 1st element */
	 if (!sflag) {                                                  /* no + yet, if any */
        adrsav=gir ;                                  /* save for subvectors */
        ivalsum=ival ;            /* sum of subvector dimensions */
	    }
    else ivalsum+=ival ;           /* add subvector dimensions */
	
	if (gir+ival-1>=gjj) derror(40) ;      /* zero out entire array */
    for (li=gir ; li<gir+ival ; li++) relval[li]=0 ;
	gir=li ;   /* points to next data item; ONE REDUNDANT 0 */

/* ---------------------------------------------------------------------------- */ 
         /* for both STATE and ordinary arrays: equivalence? */  
L32:                            
    if (warea[gi]=='+') {                                          /* subvectors */
        if ((count!=1)||(i==3)) derror(84) ;  /* + needs ordinary vector! */
        sflag=TRUE ;                /* subvectors, but no = sign yet */
        gi++ ; goto L8 ;                                  /* skip + , next array */
	     }
    if (warea[gi]=='=') {            /* equivalence, if any, is done */
         if (count>2) derror(84) ; /* admit only vectors and matrices */
         if (count==1) { if (!sflag) derror(84) ; }    /* subvectors */
         else if (sflag) derror(29) ;            /* matrix equivalence */
	    
         gi++ ;                                                                        /* skip = */
         sflag=FALSE ;
         qq=getvar() ; if (errnum) derror(errnum) ;
         if (vartyp<=0) derror(14) ;     /* illegal or missing vrble */
         if (qq!=NULL) derror(28) ;             /* name already used */
         
                                         /* now install the equivalent vector */
                                      
         qq=(struct symnode*)malloc(sizeof(struct symnode)) ;
         qq->SWITCH=TRUE ;
         qq->link=hashtab[hashval] ;
         strcpy(qq->name,symbol) ;
         hashtab[hashval]=qq ;
         qq->symtype=-1 ;
         qq->valptr=gii ;               /* gii points to table in intval[] */
	    
         intval[gii++]=1 ;   /* equiv. vector  has one dimension) */
                                           /* get equivalent-vector dimension */
         if (count==1) intval[gii++]=ivalsum ;       /* subvectors */ 					                             
         else intval[gii++]=ival ;                 /* matrix equivalence */ 
          
         intval[gii++]=adrsav ;                         /* array base index */
	     }
	 else if (sflag) derror(29) ;                          /* missing = sign */    
     if (gii>=maxinvar) derror(41) ;                       /* too large ?? */
 /*-------------------------------------------------------------------------------*/  
 
 L30 : if (warea[gi]==',') { gi++ ; goto L8 ; }          /* more to do */                                             
     }                                                                           /* end declare */
