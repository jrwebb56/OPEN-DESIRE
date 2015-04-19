/* DEF.C      Declarations of FUNCTION, PROCEDURE, MACRO,              */
/*             INTEGER, COMPLEX, dimension, ARRAY, and STATE           */
/*             Registered Copyright 1998 G.A. Korn             5/28/94 */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/*             added Error (40) check under L30                9/5/98  */

/* ------------------------------------------------------------------- */

#include "declare.h"
#include "global.h"

#include <string.h>


extern void derror() ;  extern void seekx() ;

extern ptr getvar() ;   extern ptr instal() ;   extern double evalexp() ;


void def(i)         /* for i=0,1,-1 defines/links FUNCTION, PROCEDURE, */
      int  i ; {                            /*  or MACRO name and args */

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


void declare(i)     /* declares COMPLEX, ARRAY or "dimension", INTEGER, */
   int i ; {                               /* or STATE for i = -1,0,1,3 */

      int  count, ival, li, ivalsum, adrsav, sflag=FALSE ;
      int  j=106 ;                                         /* complex j */
      double  val, expval ;  ptr  pp, qq ;

L8 :  pp=getvar() ; if (errnum) derror(errnum) ;
      if (pp!=NULL) derror(28) ;
      pp=(struct symnode*)malloc(sizeof(struct symnode)) ; /* install */
      pp->SWITCH=TRUE ;
      pp->link=hashtab[hashval] ;
      strcpy(pp->name,symbol) ;
      hashtab[hashval]=pp ;

      if (vartyp<0) goto L10 ;                     /* array declaration */
      if (i==0) derror(25) ;                               /* REAL, no [ */
      if (i==3) {                      /* declared scalar STATE variable */
         pp->symtype=1 ;
         if (nn1++>maxnsv) derror(100) ;
         pp->valptr=nn1 ; relval[nn1]=0 ;
         }
      else if (i>0) {                                        /* INTEGER */
         pp->symtype=2 ;
         pp->valptr=gii ; intval[gii]=0 ;
         if (++gii>maxinvar) derror(41) ;
         }
      else {                         /* COMPLEX - define complex j, too */
         if (strcmp(pp->name,"j")==0) derror(28) ;      /* note varsize */
         pp->symtype=3 ;
         pp->valptr=gir ; relval[gir]=0 ;
         gir+=2 ; if (gir>=gjj-2) derror(40) ;          /* allows for j */
         relval[gir-1]=0 ;
         }
      goto L30 ;

/* -------------------------------------------------------------------- */
/*  Read array declaration, make table entries in intval[]              */
/*                                                                      */
/*  intval[pp->valptr]    number of dimensions (accumulated count)      */
/*                    ... followed by the individual dimensions         */
/*                    ... followed by index to data in intval or relval */
/*                                                                      */
/*  gii indexes the table in intval[]; gir indexes data in relval[]     */
/* -------------------------------------------------------------------- */

L10 : pp->valptr=gii ;
      if (++gii>=maxinvar) derror(41) ;
      count=0 ;                          /* counts number of dimensions */
      val=1 ;

L20 : gi++ ; count++ ;             /* read array dimensions, one by one */
      expval=evalexp() ; if (errnum) derror(errnum) ;      /* dimension */
      if (count>maxdim || expval<1 || expval>=maxint) derror(80) ;

      intval[gii++]=(int)expval ;     /* next table item is a dimension */
      if (gii>=maxinvar) derror(41) ;

              /* accumulate number of (possibly COMPLEX) array elements */

      val*=expval ; 
      if (warea[gi]==',') goto L20 ;               /* do all dimensions */
      if (warea[gi++]!=']') derror(25) ;    /* an array has been parsed */

      if (i<0) val*=2 ;                     /* COMPLEX *//***** 5/28/94 */
      
/* -------------------------------------------------------------------- */

      intval[pp->valptr]=count ;                /* number of dimensions */
      if (val>=maxint) derror(40) ;
      ival=(int) val ;                      /* number of array elements */

      if (i==1) {                             /* INTEGER array, zero it */
         pp->symtype=-2 ;
         if (gii+ival-1>maxinvar) derror(41) ;
         for (li=gii ; li<gii+ival ; li++) intval[li]=0 ;       /* zero */
         gii=li   ;    /* incremented for next integer; one redundant 0 */
         goto L30 ;
         }
/* --------------------------------- */

      if (i<0) pp->symtype=-3 ; else pp->symtype=-1 ;   /* not INTEGER */

      if (i==3) {                       /* STATE, must have dimension 1 */
         if (count>1) derror(69) ;
         intval[gii++]=nn1+1 ;
         if (nn1+ival>maxnsv ) derror(100) ;       /* zero out array */
            for (li=nn1+1 ; li<=nn1+ival ; li++) relval[li]=0 ;

         nn1=li ;   /* nn1 is current number of state variables, ... */
         }              /* ... NOT pre-incremented like gii and gir! */

      else {                         /* ARRAY, dimension, or COMPLEX */
         intval[gii++]=gir ;            /* gir points to 1st element */
         if (!sflag) {                           /* no + yet, if any */
            adrsav=gir ;                      /* save for subvectors */
            ivalsum=ival ;            /* sum of subvector dimensions */
            }
         else ivalsum+=ival ;            /* add subvector dimensions */

         if (gir+ival-1>=gjj) derror(40) ;         /* zero out array */
            for (li=gir ; li<gir+ival ; li++) relval[li]=0 ;
            gir=li ;    /* points to next data item; ONE REDUNDANT 0 */

/* ----------------------------------------------------------------- */

         if (warea[gi]=='+') {                         /* subvectors */
            if (pp->symtype!=-1 || count!=1) derror(84) ;
            sflag=TRUE ;
            gi++ ; goto L8 ;                  /* skip + , next array */
            }
         if (warea[gi]=='=') {       /* subvectors, if any, are done */
            if (pp->symtype!=-1 || count>2) derror(84) ;
            if (count==1) { if (!sflag) derror(84) ; } /* subvectors */
            else if (sflag) derror(29) ;            /* "equivalence" */

            gi++ ;                                         /* skip = */
            sflag=FALSE ;
            qq=getvar() ; if (errnum) derror(errnum) ;
            if (vartyp<=0) derror(14) ;
            if (qq!=NULL) derror(28) ;          /* name already used */

                                                          /* install */
            qq=(struct symnode*)malloc(sizeof(struct symnode)) ;
            qq->SWITCH=TRUE ;
            qq->link=hashtab[hashval] ;
            strcpy(qq->name,symbol) ;
            hashtab[hashval]=qq ;
            qq->symtype=-1 ;
            qq->valptr=gii ;      /* gii points to table in intval[] */

            intval[gii++]=1 ;   /* equiv. vector  has one dimension) */
            if (count==1) intval[gii++]=ivalsum ; /* subvectors, overall
                                                    vector dimension */
            else intval[gii++]=ival ;    /* "equivalence", dimension */ 
            intval[gii++]=adrsav ;               /* array base index */
            }
         else if (sflag) derror(29) ;
         }                           /* ARRAY, dimension, or COMPLEX */

    if (gii>=maxinvar) derror(41) ;

/* ----------------------------------------------------------------- */

L30 : if (warea[gi]==',') { gi++ ; goto L8 ; }
      if (i<0) {                                 /* define complex j */
         strcpy(symbol,"j") ;            /* note: depends on varsize */
         if (hashtab[j]!=NULL) { if (hashtab[j]->symtype!=3) derror(28) ; }
         else {
            pp=(struct symnode*)malloc(sizeof(struct symnode)) ;
            hashtab[j]=pp ;
            pp->SWITCH=TRUE ;
            pp->link=NULL ;
            strcpy(pp->name,symbol) ;
            pp->symtype=3 ;
            pp->valptr=gir ;if (gir+1>=gjj) derror(40) ;
            relval[gir++]=0 ; relval[gir++]=1 ;
            }                                                /* else */
         }                                                     /* if */
      }                                                   /* declare */
