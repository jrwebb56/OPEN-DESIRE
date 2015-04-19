/* READ.C      read and input                                          */
/*             Registered Copyright 1992  G.A. Korn            2/18/92 */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/* ------------------------------------------------------------------- */

#include "declare.h"
#include "global.h"

extern void cvalexp() ; extern void gitchn() ;   extern void derror() ;

extern ptr getvar() ;   extern ptr instal() ;   extern double evalexp() ;
extern int getsub() ;


void getinput(i)               /*  INPUT,READ if i=0,1; matrix if flag */
       int  i ; {

       int  k, li, sub, lj, lk, savgi, num, dim, jj, kk, flag ;
       unsigned char  ch ;  double  val1, val2 ;  ptr  p ;
       FILE  *INPTR ;


       if (!runf) derror(22) ;
       flag=FALSE ;                                      /* matrix flag*/
       if (i>0) goto L10 ;

/*                                    note: k<0 indicates file of REAL */
/* ------------------------------------------------------------------- */
                                          /*  only INPUT and MAT INPUT */
       if (warea[gi]=='#') {                     /* get channel number */
          k=0 ; gi++ ;                                       /* skip # */
          gitchn() ;
          if ((INPTR=fd[chanlno])==NULL) derror(64) ;      /* not open */
          if (warea[gi++]!=',') derror(44) ;               /* no comma */
          }
       else INPTR=stdin ;                /* read from console terminal */

 /*    for INPUT, gi reads user text; li writes queries; lj writes
                        input to bottom of warea; lk marks items there */
/* ------------------------------------------------------------------- */

       lk=1 ; /* points to bottom of warea for first go-around (not 0) */

L10 :  savgi=li=gi ;  /* save pointer to INPUT variable name for error */

/* ------------------------------------------------------------------- */

L20 :  if (flag) goto L25 ;    /* we already established it is a array */

       kk=gi ;                                      /* is it an array? */
       p=getvar() ;
       if (errnum==73) {                                        /* yes */
          errnum=0 ; flag=TRUE ; gi=kk ; goto L25 ;
          }
       if (errnum) derror(errnum) ;

       if (vartyp==0) derror(84) ;             /* no functions allowed */
       if (p==NULL) p=instal() ;                /* new REAL, instal it */
       else {
          if (p->symtype>3 || p->symtype< -3) derror(84) ;  /* illegal */
          if (p->symtype<0) sub=getsub(p) ;             /* subscripted */
          }                         /* NOTE: sub is needed explicitly! */
       goto L30 ;

L25 :  start=1 ;                             /* fixes getvar for array */
       p=getvar() ; start=0 ; if (errnum) derror(errnum) ;
       if (p==NULL) derror(19) ;
       if (p->symtype< -3) derror(84) ;                     /* illegal */

       jj=p->valptr ; dim=intval[jj] ; num=1 ;
       for (kk=1 ; kk<=dim ; kk++ ) num*=intval[jj+kk] ;   /* get size */
       sub=0 ;                            

L30 :  if (i>0) goto L500 ;                       /* READ and MAT READ */

/* ------------------------------------------------------------------- */
                                          /*  only INPUT and MAT INPUT */

       if (warea[lk]==',' || warea[lk]==';') { lk++ ; goto L400 ; }

L100 : lj=4 ; lk=lj ;        /* reset pointers ahead of stack ptr wptr */

       if (INPTR==stdin) {
          do {                   /* prompt with name if terminal input */
             putchar(warea[li]) ;
             if (warea[li]=='[') { while (warea[li]!=']' && warea[li]!=lf)
                       putchar(warea[++li]) ; }         /* subscripted */
             li++ ;
             } while (warea[li]!=',' && warea[li]!=';' &&
                                   warea[li]!=lf && warea[li]!='|') ;

          if (flag) printf("[]") ;                  /* indicates array */
          printf(" ? ") ;
          }                                         /* if INPTR==stdin */

/*  $$$$$$$     else if (k<0) {   for file of REAL  $$$$$$$$$$$$       */

       do {                 /* copy input string(s) to bottom of warea */

          if ((kk=getc(INPTR))==EOF) goto L200 ;
          ch=kk ;                               /* getc needs INTEGER! */
          if (INPTR==stdin && ch=='!') derror(197) ;        /* escape! */
          if (ch!=blank) warea[lj++]=ch ;
          } while (ch!=lf) ;

        warea[lj]=lf ; /****************************************?*?***/

L400 : lj=gi ; gi=lk ;          /* gi must now work at bottom of warea */
       if (p->symtype!=3 && p->symtype!= -3) val1=evalexp() ;
       else cvalexp(val1,val2) ; if (errnum) goto L405 ;
       if ((p->symtype==2 || p->symtype== -2) &&
          (val1>maxint || val1< -maxint)) { errnum=20 ; goto L405 ; }

       if (warea[gi]!=lf && warea[gi]!=',' && warea[gi]!=';') derror(33) ;

L405 : lk=gi ; gi=lj ;                             /* recover pointers */
       if (errnum) {
          if (INPTR!=stdin || warea[lk]==',' || warea[lk]==';')
                                                         derror(errnum) ;

          bye=FALSE ;   /* disables derror return to longjmp to let .. */
          derror(errnum) ; bye=TRUE ;       /*  ... user correct error */
          gjj=maxrlvar ; wptr=1 ;                 /* clean eval. stack */
          flevel=0 ;
          li=savgi ;
          goto L100 ;
          }
/* ------------------------------------------------------------------- */
                                                /*  for all four cases */
L700 : if (p->symtype==1) relval[p->valptr]=val1 ;
       else if (p->symtype== -1)
          relval[intval[intval[p->valptr]+p->valptr+1]+sub]=val1 ;
       else if (p->symtype==2) {
          if (val1>maxint || val1< -maxint) derror(20) ;
          intval[p->valptr]=val1 ;                        /* truncated */
          }
          else if (p->symtype==3) {
                relval[q->valptr]=val1 ; relval[q->valptr+1]=val2 ;
                }
       else if (p->symtype== -2) {
          if (val1>maxint || val1< -maxint) derror(20) ;
          intval[intval[p->valptr]+p->valptr+1+sub]=val1 ;/* truncated */
          }
       else if (p->symtype== -3) {
          kk=intval[intval[p->valptr]+p->valptr+1]+2*sub ;
          relval[kk]=val1 ; relval[kk+1]=val2 ;
          }
       else derror(84) ;

       if (tracer>3) {
          printf("%c%s",'\33',"[7m") ;                /* reverse video */
          if (abs(p->symtype)==3)
            printf("A: %s%c%g%s%g%s",p->name,'=',val1,"+j*(",val2,")\n") ;
          else printf("A: %s%c%g\n",p->name,'=',val1) ;
          printf("%c%s",'\33',"[0m") ;                /* restore video */
          }
/* ------------------------------------------------------------------- */
                                             /* MAT READ and MAT INPUT */
       if (flag) {
          li=savgi ;                                    /* query again */
          if (++sub<num) goto L30 ;              /* next array element */
          }
/* ------------------------------------------------------------------- */
                                                    /*  all four cases */

   if (warea[gi]==',' || warea[gi]==';') { gi++ ; goto L10 ; }
   return ;                                  /* rest of list or return */

/* ------------------------------------------------------------------- */
                                                         /*  only READ */

L500 : li=datptr ;                                /* li looks for DATA */
       if (warea[li]!=',' && warea[li]!=';') {

L550 :    if (warea[li]==lf) {            /* search for DATA statement */
             li+=4 ;
             if (li>=endtxt) derror(51) ;
             }
          else if (warea[li]!='|') { li++ ; goto L550 ; }
          else li++ ;
          if (warea[li]!=vdata) goto L550 ;
          }
                                        /* skip ',',';', or DATA token */

       savgi=gi ; li++ ; gi=li ;        /* evalexp or cvalexp needs gi */
       if (p->symtype!=3 && p->symtype!= -3) val1=evalexp() ;
       else cvalexp(val1,val2) ;  if (errnum) derror(errnum) ;

       if (warea[gi]!=lf && warea[gi]!='|' && warea[gi]!=','
                                     && warea[gi]!=';') derror(33) ;

       datptr=gi ; gi=savgi ; goto L700 ;          /* recover pointers */

/* ------------------------------------------------------------------- */
                                                                /* EOF */
L200 : if (ferror(INPTR)) {clearerr(INPTR) ; derror(18) ; }   /* error */
       if (INPTR!=stdin) {                              /* end of file */

 printf("Warning: END OF FILE encountered on Channel %d\n",chanlno) ;
           if (fclose(INPTR)==EOF) {
              clearerr(INPTR) ;
              INPTR=stdin ; fd[chanlno]=NULL ;
              }
           eofflg=TRUE ;
           while (warea[gi]!=lf && warea[gi]!='|') gi++ ;
           }                                               /* getinput */
       }
