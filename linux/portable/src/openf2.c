/* OPENF.C    file operations for DESIRE                               */
/*            Registered Copyright 1999  G.A. Korn   UNIX              */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/*   fixed writefile, old00, newload for ./, ../, ....     7/9/95      */
/*   added append                                          5/12/99     */
/*   changed newload() (reload)                           10/24/99     */
/*   changed no_name.prc to .lst                           7/16/01     */
/*   killed error 125                                       8/8/01     */
/*   added NEW, as for Windows                              4/2/02     */

/* ------------------------------------------------------------------- */
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>

#include "declare.h"
#include "global.h"

extern void prepr1() ;   extern void derror() ;  extern void unpack() ;
extern int find00() ;    extern void QuitGraph() ;

/* ------------------------------------------------------------------- */

void readname(ext)                /* read file name and, if necessary, */
                                 /* default extension into array fname */
      filext  ext ; {           /* CALLED BY writefile, unsave, old00  */
      int li, idot ;

      if (warea[gi++]!='\'') derror(34) ;
      idot=0 ; li=0 ;

L10 : if (warea[gi]=='.') fname[li++]=warea[gi++] ; /* for './xx, etc. */ 
      if (warea[gi]=='.') fname[li++]=warea[gi++] ;
      if (warea[gi]=='/') { fname[li++]=warea[gi++] ; goto L10 ; }

      do {
         if (warea[gi]=='.') idot=1 ;
         if (li>maxnam-1) derror(45) ;
         fname[li++]=warea[gi++] ;
         } while(warea[gi]!='\'') ;

      if (idot==0) {                      /* no extension was supplied */
         if (li>maxnam-4) derror(45) ;
         idot=li ;                                /* default extension */
         for (li=idot ; li<=idot+3 ; li++)
            fname[li]=ext[li-idot] ;  /* ext. over 3 chars. is clipped */
         }

      fname[li]=0 ;                          /* terminate fname string */
      gi++ ;                                                 /* skip ' */
      }                                                    /* readname */



void writefile() {                /* write user-area into file 'fname' */
      int lj ;

      if (endtxt<=linsiz+2) derror(99) ;

      if (warea[gi]!= '\'') {
         strcpy(fname,pname) ;

         lj=0 ;     /* fix extension; first take care of .\, ..\, etc. */
L9:      if (fname[lj]=='.') lj++ ; if (fname[lj]=='.') lj++ ;
         if (fname[lj]=='/') { lj++ ; goto L9 ; }

         do lj++ ; while (fname[lj]!='.') ;
         fname[lj+1]='p' ; fname[lj+2]='r' ; fname[lj+3]='c' ;
         fname[lj+4]=0 ;
         }

      else readname(".prc") ;

      if ((fdn=fopen(fname,"w"))==NULL) derror(11) ;      /* open file */

      lj=linsiz+2 ;
      do {
         if (putc(warea[lj++],fdn)==EOF) { clearerr(fdn) ; derror(106) ; }
         } while (lj!=endtxt) ;

      if (fclose(fdn)!=0) { clearerr(fdn) ; derror(103) ; }
      system("clear") ;     
      printf("-- saved in %s\n",fname) ;
      fprintf(noteptr,"-- saved in %s\n",fname) ;       /* journal file */
      }                                                    /* writefile */


void  old00(i)                          /* "old" and "chain" statements */
      int  i ; {
      int  li, lj, len, nn ;

      if (i>0) {                                               /* "old" */
         runf=FALSE ;
         if (warea[gi]!= '\'') {    /* restore old program as .prc file */
            strcpy(fname,pname) ;

            lj=0 ;   /* fix extension; first take care of .\, ..\, etc. */
L9:         if (fname[lj]=='.') lj++ ; if (fname[lj]=='.') lj++ ;
            if (fname[lj]=='/') { lj++ ; goto L9 ; }
            do lj++ ; while (fname[lj]!='.') ;
            fname[lj+1]=pname[lj+1]='p' ; 
            fname[lj+2]=pname[lj+2]='r' ;
            fname[lj+3]=pname[lj+3]='c' ;
            }
         else readname(".prc") ;                         /* set new PIC */
         system("clear") ;                              /* clear screen */
         goto L10 ;
         }

      if (spare3) QuitGraph() ;                              /* "chain" */
      readname(".prc") ;
      lnumber=0 ;
      if (warea[gi]==',') {                   /* decode the line number */
         gi++ ;
         while (warea[gi]>='0' && warea[gi]<='9')
                              lnumber=10*lnumber+warea[gi++]-'0' ;
         if (lnumber<1 || lnumber>=maxlin) derror(198) ;
         }
      while (stktop!=NULL) {         /* clean control stack for "chain" */
         tptr=stktop ;
         stktop=stktop->dslink ;
         free(tptr) ;
         }
      gjj=maxrlvar ; wptr=1 ; flevel=0 ;     /* clean evaluation stacks */
      datptr=linsiz+1 ;                            /* restore data ptr. */

L10 : if (warea[gi]==lf) goto L12 ;
      if (warea[gi]!='|') derror(33) ;
      if (warea[gi+1]!=vrem && warea[gi+1]!=chr11 &&
                            warea[gi+1]!=vendls) derror(81) ;

L12 : if ((fdn=fopen(fname,"r"))==NULL) derror(11) ;    /* connect file */

      len=linsiz+1 ;       /* write to memory, starting at len=linsiz+2 */
      nn=0 ;                                    /* this will be dynlin! */
L19 : while ((lj=getc(fdn))!=EOF) {                    /* read the file */
         if (++len>maxtxt) derror(39) ;               /* file too large */
         if ((warea[len]=lj)==lf) goto L19 ;               /* next line */
         if (warea[len]==vdynmc && warea[len-4]==lf)
                         nn=BASE*warea[len-3]+warea[len-2] ;
         }
      if (ferror(fdn)) { clearerr(fdn) ; derror(18) ; }    /* I/O error */
      if (fclose(fdn)!=0) {clearerr(fdn) ; derror(103) ; } ;   /* close */

      strcpy(pname,fname) ; endtxt=len+1 ; dynlin=nn ;       /* set PIC */

      if (i>0) {                                               /* "old" */
         prepr1() ; relval[mmm+13]=1.0 ;           /* reset data, scale */
                                              /*  note dispp=1 in prepr1*/
         size=1 ; tracer=0 ; flag2=FALSE ;
         printf("%s",pname) ; WRITELN ; WRITELN ;
         fprintf(noteptr,"-- old %s ---------\n",pname) ;
         }
      else {                                                 /* "chain" */
         xlength=0 ; nn1=0 ;                     /* clear compiled area */
         fprintf(noteptr,"-- chain to %s --------\n",pname) ;
         if (lnumber==0) gi=linsiz+1 ;
         else {
            gi=find00()-1 ;
            if (no!=lnumber) {lineno=0 ; derror(68) ; }
            }
         WRITELN ;
         }                                                      /* else */
      if (editflg>0) { editflg=0 ; SPARE3=1 ; }          /* kill editor */ 
      }                                                        /* old00 */


void new00(i)   /* "new" and "NEW" statements: clear data, program,  etc.
                                         i = 1 is for editors using NEW */
   int i ; {

   unsigned char  ch ;  int NEWflag=0, k ;

   if (runf) derror(199) ;

   if (i==0) {
      printf("\nERASE PROGRAM %s FROM MEMORY?  ",pname) ;
      for (k=1; k<1000; k++) ;                            /* delay loop */
      if (getchar()!='y') { WRITELN ; return ; }
      }
      
   system("clear") ;                            /* <*******/     /* clear screen */
   
   NEWflag=0 ;
   if (warea[gi]==lf || warea[gi]=='|') {
      NEWflag=1 ;
      strcpy(pname,"no_name.lst") ;
      }
   else { readname(".lst") ; strcpy(pname,fname) ;}    /* get new name */
   if (i==0) printf("PIC = %s\n\n",pname) ;
   relval[mmm+13]=1.0 ;                                 /* new program */       
   size=1 ; tracer=0 ; flag2=FALSE ;
   nn2=200 ; ncol=0 ;           
   prepr1() ; endtxt=linsiz+2 ; dynlin=0 ;  
   if (i==0 || !NEWflag) fprintf(noteptr, "-- new %s --------\n",pname) ; 
   }                                                          /* new00 */


void newload() {                /* load new ASCII source program file */
    
   int  lj ;

   system("clear") ;                                  /* clear screen */
   runf=FALSE ;
 
   if (warea[gi]!= '\'') strcpy(fname,pname) ; /* restore old program */
   else {                                              /* set new PIC */
      readname(".lst") ;
      strcpy(pname,fname) ;
      }

   prepr1() ; relval[mmm+13]=1.0 ;               /* reset data, scale */
   size=1 ; tracer=0 ; flag2=FALSE ;
   endtxt=linsiz+2 ; dynlin=0 ;         /* this will be a new program */

   if (editflg>0) { editflg=0 ; SPARE3=1 ; }      /* kill editor at > */

   nn2=200 ; ncol=10 ; append=TRUE ;                   /* for precomp */
   if ((inptr=fopen(fname,"r"))==NULL) {
      ncol=0 ; inptr=stdin ; append=FALSE ;
      derror(11) ;
      }

   printf("%s",pname) ; WRITELN ; WRITELN ;
   fprintf(noteptr,"-- reload %s ---------\n",pname) ;
   }


void gitchn() {                             /* get the channel number */
   int minus ;

   chanlno=0 ;
   if (warea[gi]=='-') { minus=TRUE ; gi++ ; } else minus=FALSE ;
   while (warea[gi]>='0' && warea[gi]<='9')
                      chanlno=10*chanlno+warea[gi++]-'0' ;
   if (chanlno<0 || chanlno>maxchan) derror(50) ;
   if (chanlno==0 && warea[gi-1]=='0') return ;
   if (minus) chanlno= -chanlno ;
   }                                                         /* gitchn */


void openf() {          /* - connect file or device for input or output */
   int  minus,len, stat ;  unsigned char  token ;

   readname(".dat") ;
   if (warea[gi++]!=ESC) derror(49) ;                       /* need ESC */
   token=warea[gi++] ;                  /* get, skip input/output token */
   gitchn() ;                                     /* get channel number */
   if (chanlno<0) { minus=1 ; chanlno= -chanlno ; }      /* binary file */
   else minus=0 ;

   if (fd[chanlno]!=NULL) derror(63) ;    /* file/device already in use */

   if (token==vinput) {                                 /****************/
         if (!minus) fd[chanlno]=fopen(fname,"r") ;
         else fd[chanlno]=fopen(fname,"rb") ;
         if (fd[chanlno]==NULL) derror(11) ;
         }
   else if (token==voutput) {
         if (chanlno>=2)                         /* not a database file */
              {if ((fd[chanlno]=fopen(fname,"wt"))==NULL) derror(11) ;}
         else if ((fd[chanlno]=fopen(fname,"at"))==NULL) derror(11) ;            
         }
   else derror(49) ;
   }                                                           /* openf */


void closef() {                     /* disconnect file(s) or device(s) */

L10 : gitchn() ;                                    /* get channel no. */

      if (fd[chanlno]==NULL) goto L15 ;               /* not connected */
      if (fd[chanlno]==inptr) inptr=stdin ;
      else if (fd[chanlno]==outptr) outptr=stdout ;

      if (fclose(fd[chanlno])!=0) derror(103) ;
      fd[chanlno]=NULL ;

L15 : if (warea[gi]==',') { gi++ ; goto L10 ; }    /* another channel? */
      }                                                      /* closef */


void lod00() {                 /* load; does NOT change pname directly */

  if (runf) derror(199) ;
  if (warea[gi]!=lf && warea[gi]!='|')  {             /* ordinary load */
     readname(".src") ; 
     if (editflg>1) {  editflg=0 ; SPARE3=1 ; }    /* kill editor at > */
     }
  else strcpy(fname,"SYSPIC.lst") ;                           /* eload */
  
  ncol=10 ; append=TRUE ; eofflg=FALSE  ;       /* for precomp */
  if ((inptr=fopen(fname,"r"))==NULL) {
     ncol=0 ; inptr=stdin ; append=FALSE ;
     derror(11) ;
     }
 }                                                           /* lod00 */
