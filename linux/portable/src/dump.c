
/* DUMP.C.     Array Writer; and Data Dump for Debugging               */
/*             Registered Copyright 1992  G.A. Korn            2/17/92 */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/**            added UNIX keyboard-hit routine in 2 places     9/20/02 */
/* ------------------------------------------------------------------- */

#include "declare.h"
#include "global.h"

extern void derror() ;  extern void  gitchn() ;  extern ptr getvar() ;


void  matprt() {                    /* prints an array with name labels */

   int  lq,lj,lk,lh,dim,offset,index,ind1,count1 ;

   lj=p->valptr ;
   dim=intval[lj] ;                    /* dim has number of dimensions */
   offset=intval[lj+1] ;                     /* 1st (column) dimension */

                  /* display array, dimensions, get total no. elememts */

   fprintf(outptr,"\n%s%c%d",p->name,'[',offset) ;    /* 1st dimension */
   for (lk=2 ; lk<=dim ; lk++) {
      fprintf(outptr,"%c%d",',',intval[lj+lk]) ;       /* dimension(s) */
      offset*=intval[lj+lk] ;                 /* total no. of elements */
      }
   index=1 ;                            /* index counts items per line */
   ind1=5 ;                                          /* items per line */

   lh=1 ;                             /* lh counts last "column" (row) */
   count1=intval[lj+dim] ;            /* items per last "column" (row) */

   if (p->symtype==-1) {
      fprintf(outptr,"]  : REAL ARRAY (in ROW major sequence)\n") ;
      lj=intval[lj+dim+1] ;            /* points to first REAL element */
      }
   else if (p->symtype==-3) {
      fprintf(outptr,"]  : COMPLEX ARRAY (pairs in ROW major sequence)\n") ;
      lj=intval[lj+dim+1] ;            /* points to first REAL element */
      offset*=2 ; count1*=2 ;
      }
   else {
      fprintf(outptr,"]  : INTEGER ARRAY  (in ROW major sequence)\n") ;
         lj+=dim+1 ; ind1=8 ;
      }

   for (lk=lj ;lk<=lj+offset-1 ; lk++) {            /* write the array */

   if (spare1) {                                 /* look at ctl-c flag */
       spare1=0 ;
       if (lq=getchar()!=lf) derror(301) ;  /* lf continues, else stop */
       }   
       
      if (p->symtype==-2) fprintf(outptr,"%6d%s",intval[lk],"   ") ;
      else fprintf(outptr,"%12.6e%s",relval[lk],"   ") ;

      if (ferror(outptr)) { clearerr(outptr) ; derror(106) ; }

      index++ ; lh++  ;
      if (index>ind1) { index=1 ; putc('\n',outptr) ; }
      if (lh>count1) { index=1 ; lh=1 ; fprintf(outptr,"\n\n") ; }
      }                                                        /* for  */
   putc('\n',outptr) ;
   }                                                        /* matprnt */


void  MATPRT() {                /* prints an array without name labels */

   int  lq,lj,lk,dim,offset ;

   spare5=0 ;                                         /* reset ## flag */
   lj=p->valptr ;
   dim=intval[lj] ;                    /* dim has number of dimensions */
   offset=intval[lj+1] ;                     /* 1st (column) dimension */

                  /* display array, dimensions, get total no. elememts */

   for (lk=2 ; lk<=dim ; lk++) offset*=intval[lj+lk] ;

   if (p->symtype==-1) lj=intval[lj+dim+1] ;    /*  first REAL element */
   else if (p->symtype==-3) {
      lj=intval[lj+dim+1] ;            /* points to first REAL element */
         offset*=2 ;
      }
   else lj+=dim+1 ;

   for (lk=lj ;lk<=lj+offset-1 ; lk++) {            /* write the array */
         if (p->symtype==-2) fprintf(outptr,"%d%c",intval[lk],'\n') ;
         else fprintf(outptr,"%g%c",relval[lk],'\n') ;

         if (spare1) {                           /* look at ctl-c flag */
           spare1=0 ;
           if (lj=getchar()!=lf) derror(301) ;  /* lf continues, else stop */
           }   
         
         if (ferror(outptr)) { clearerr(outptr) ; derror(106) ; }
         }                                                      /* for  */
   }                                                          /* MATPRT */


void  dump() {                                       /* DUMP statement */
     int  li,index,flag ;

     if (warea[gi]=='#') {
        gi++ ;                                               /* skip # */
        gitchn() ;
        if (chanlno==0) derror(50) ;
        if ((outptr=fd[chanlno])==NULL) derror(64) ;
        }
     else outptr=stdout ;                 /* write to console terminal */

     putc('\n',outptr) ;
     index=2 ; flag=FALSE ;

     for (li=0 ; li<=maxhash ; li++) {
        p=hashtab[li] ;
        while (p!=NULL) {
           if (p->symtype!=1 && p->symtype!=2) goto L10 ;
           if (index==2) { putc('\n',outptr) ; index=0 ; }
           if (p->symtype==1) fprintf(outptr,"  %-8s%s%-12.6e%c",p->name,
                                   " REAL  = ",relval[p->valptr],tab) ;
           else fprintf(outptr,"  %-8s%s%-8d%c",p->name,
                                 " INTEGER  = ",intval[p->valptr],tab);
           index++ ; flag=FALSE ;

L10 :      if (p->symtype!=3) goto L20 ;
           if (!flag) fprintf(outptr,"\n\n") ;
           fprintf(outptr,"  %-8s%s%.6g",p->name,
                            " COMPLEX VARIABLE = ",relval[p->valptr]) ;
           fprintf(outptr,"%s%.6g%s"," + j* (",relval[p->valptr+1],")\n") ;

           index=2 ; flag=TRUE ;

L20 :      if (ferror(outptr)) { clearerr(outptr) ; derror (106) ; }

           p=p->link ;
           }                                                  /* while */
        }                                                       /* for */
     putc('\n',outptr) ;

/* ------------------------------------------------------------------- */
                                                    /* arrays are next */
     index=1 ;
     for (li=0 ; li<= maxhash ; li++) {
        p=hashtab[li] ;
        while (p!=NULL) {
           if (p->symtype<0 && p->symtype>-4) matprt() ;
           p=p->link ;
           }                                                  /* while */
        }                                                       /* for */
     putc('\n',outptr) ;
     }                                                         /* dump */

/*  if (errnum=302) THEN errnum=301     $$$$$$$            no GO after */

