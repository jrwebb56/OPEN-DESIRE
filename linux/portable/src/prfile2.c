/* PRFILE.C   File Operations for OPEN DESIRE      LINUX               */
/*            Registered Copyright 2002 G.A. Korn                      */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/*   fixed "keep" for ./, ../, ....     7/9/95                         */
/*   added keep+                        5/21/99                        */
/*   changed help for LINUX             8/6/99                         */
/*   changed editing                    7/16/01 , 4/2/02               */
/*   ed WITHOUT line numbers, edit WITH 8/24/02                        */

/**     changed note(0) to make NN = old NN/MM for use with MM 9/18/02 */

/* ------------------------------------------------------------------- */

#include <string.h>

#include "declare.h"
#include "global.h"

extern void prepr1() ;  extern void derror() ;  extern void readname() ;
extern void dlist() ;   extern void print() ;   extern quitX() ;
extern QuitGraph() ;

/* ------------------------------------------------------------------- */

void pic0() {                                 /* display or change PIC */

   if (warea[gi]!=lf && warea[gi]!='|') {
      readname(".lst") ; strcpy(pname,fname) ;
      }
   printf("PIC = %s\n",pname) ;                         /* display PIC */
   fprintf(noteptr,"-- PIC = %s ------\n",pname) ;
      }                                                        /* pic0 */


void prfile(i)     /* list, list+, keep, keep+, edit, bye, help, or sh */
  int i ; {

  int lj ;  unsigned char slen ;

  if (i>=0) {                              /* list, list+, keep, keep+ */

    if (warea[gi]!='\'') {                             /* no file spec */
       if (i<2) {                                     /* list or list+ */
          system("clear") ;                        /* clear screen ... */
          fdn=stdout ; dlist(i) ;           /* ... and list to console */
          return ;
          }
                   
       else if (i==2) {                                        /* keep */
          strcpy(fname,pname) ;
          lj=0 ;    /* fix extension; first take care of .\, ..\, etc. */
L9:       if (fname[lj]=='.') lj++ ; if (fname[lj]=='.') lj++ ;
          if (fname[lj]=='/') { lj++ ; goto L9 ; }

          do lj++ ; while (fname[lj]!='.') ;          /* new extension */
          fname[lj+1]='l' ; fname[lj+2]='s' ; fname[lj+3]='t' ; 
          fname[lj+4]=0 ;
          goto L15 ; 
          }

       else if (i==4) {                                       /* keep+ */
          strcpy(fname,pname) ;
          lj=0 ;     /* fix extension; first take care of .\, ..\, etc. */
L99:      if (fname[lj]=='.') lj++ ; if (fname[lj]=='.') lj++ ;
          if (fname[lj]=='\\') { lj++ ; goto L99 ; }

          do lj++ ; while (fname[lj]!='.') ;          /* new extension */
          fname[lj+1]='s' ; fname[lj+2]='r' ; fname[lj+3]='c' ; 
          fname[lj+4]=0 ;
          goto L15 ; 
          }
          
       else { fdn=noteptr ; dlist(i) ; return ; }  /* list to notebook */
       }

     if (i=0 || i==4) readname(".src") ; else readname(".lst") ;  /* file spec */

L15: if ((fdn=fopen(fname,"w"))==NULL) derror(11) ;         /* connect */
     dlist(i) ;
  
 /**    fprintf(fdn,"\n/PIC \'%s\'\n",fname) ;       needed for reload only 
     fprintf(fdn,"/--\n") ;         for OPEN WINDOWS version!         **/
        
     if (fclose(fdn)==EOF) {                             /* disconnect */
        clearerr(fdn) ; derror(95) ;
        }
     system("clear") ;
     printf("-- keep in %s\n",fname) ; 
     fprintf(noteptr,"-- keep in %s\n",fname) ;       /* journal entry */
     return ;
     }                                                /* list or list+ */

/* ------------------------------------------------------------------- */

  if (i== -1 || i== -4) {                                /* sh or help */

     if (i== -1) slen=0 ;                                        /* sh */
     else {                                                    /* help */
        dlabl[0]='m' ; dlabl[1]='o' ; dlabl[2]='r'; 
        dlabl[3]='e' ; dlabl[4]=blank ; dlabl[5]='<' ;       /* more < */
        dlabl[6]=blank ; dlabl[7]= '.'; dlabl[8]='/' ;
        dlabl[9]='h' ; dlabl[10]='e' ; dlabl[11]='l' ;
        dlabl[12]='p' ; dlabl[13]='/' ;
        slen=14 ;
        }
    
     if (warea[gi]==lf) {            /* was display help subdirectory */
        if (spare3) QuitGraph() ;
        printf("\n\n===>  type \"help FILENAME \"\n\n") ;
        return ;
        }   

     else do  {                          
        dlabl[slen++]=warea[gi++] ;
        if (slen>50) derror(12) ;
        } while (warea[gi]!=lf) ;
        
     dlabl[slen]=0 ;                               /* terminate string */
     WRITELN ;

     if (i== -1) printf("%c%s",'\33',"[7m\n") ;       /* reverse video */
     else system("clear") ;                                    /* help */
     if (system(dlabl)<0) derror(121) ;                   /* EXEC call */
     if (i== -1) printf("%c%s",'\33',"[0m\n") ;       /* restore video */
     return ;
     }                                                   /* sh or help */

    /* ed, edit, or bye: save program unless edit causes prcomp errors */

  if (maxstk<=0 || i== -3) {

     fdn=fopen("SYSPIC.lst","w") ;                 /* write SYSPIC.lst */
     if (fdn==NULL) derror(11) ;

     if (warea[gi]==lf || warea[gi]=='|') {       /** edit whole file **/
        fprintf(fdn,"/NEW\n") ;                 /**** /NEW  *****/   
        fprintf(fdn,"/PIC \'%s\'\n",pname) ;     /** edit all **/
        }         
     else if (i== -3 || i== -5) derror(33) ;  /* special dialogs 
                             require line numbers, do NOT work with ed */
     if (i== -5) {
        fprintf(fdn,"/\n") ;    /* ed, switch to auto-line number mode */
        dlist(4) ;                   /*  and save WITHOUT line numbers */ 
        fprintf(fdn,"/\n") ;     /* then switch back to commands */
        }
     else if (i== -2 || i == -3) dlist(1) ; /* edit or bye, save WITH line numbers */
    
     fprintf(fdn,"\n") ;           /** for return from editor **/
    
     if (ferror(fdn)) { clearerr(fdn) ; derror(106) ; }
     if (fclose(fdn)==EOF) {clearerr(fdn) ; derror(95) ; }
     }                                                           /* if */

  WRITELN ;

  if (i== -2)                                    /* ed: call gedit & */
     system("sh ./editcfg1") ;  
 
  else if (i== -5)                             /* edit: call gedit & */
    system("sh ./editcfg2") ;       

  else if (i==-3) {                                       /* i=-3, bye */

     system("clear") ;      
     fprintf(noteptr,"/--\n") ;
     printf(" *********************************************\n") ;
     printf(" ***  your program is saved in SYSPIC.lst  ***\n") ;
     printf(" *********************************************\n") ;

     quitX() ;      
     WRITELN ; exit(0) ;                           /* closes all files */
     }
  }                                                          /* prfile */


 void not0() { /* NOTE statement; list, print, or save in journal file */

   int lj, idot ;

   if (warea[gi]=='#') {                                       /* list */
      gi++ ; putc('\n',noteptr) ; prfile(3) ;
      fprintf(noteptr,"STOP\n") ; return ;
      }
   else if (warea[gi]!=lf && warea[gi]!='|') {
      fprintf(noteptr,"-- \n") ; print(1) ; return ;
      }
                      /* save TMAX, NN, and stash list in an .RCV file */

   if (!disk || !sbegin || !xlength) derror(92) ;          /* no stash */

   strcpy(fname,pname) ;                            /* get current PIC */
   idot=0 ;                          /* find character after extension */
   if (fname[idot]=='.') idot++ ;  if (fname[idot]=='.') idot++ ;
   while(fname[idot]!='.') idot++ ;
   fname[idot+1]='R' ; fname[idot+2]='C' ; fname[idot+3]='V' ; /* add .RCV */
   fname[idot+4]=0 ;

   if ((fdn=fopen(fname,"w"))==NULL) derror(11) ; /* connect .RCV file */

                 /* entries in .RCV file to deal with .TIM file recover*/

   fname[idot+1]='T' ; fname[idot+2]='I' ; fname[idot+3]='M' ;

   fprintf(fdn,"connect \'%s\' as input %d\n",fname,1-maxchan) ;
   fprintf(fdn,"t=%g | TMAX=%g\n",t0,relval[mmm+4]) ;
   fprintf(fdn,"NN=%g\n",relval[mmm+1]/relval[mmm+15]) ;  /*  <*****/
   fprintf(fdn,"drun\n") ; fprintf(fdn,"DYNAMIC\n") ;

   fprintf(fdn,"recover ") ; lj=sbegin ;           /* write stash list */
   while(warea[lj]!=lf && warea[lj]!='|') putc(warea[lj++],fdn) ;

   putc('\n',fdn) ; fprintf(fdn,"/-- \n") ;      /* termination needed */

   if (ferror(fdn)) { clearerr(fdn) ; derror(106) ; }
   if (fclose(fdn)==EOF) { clearerr(fdn) ; derror(95) ; }

   fprintf(noteptr,"STOP | ------------------------------\n") ;

         /* increment PIC version number to preserve current .TIM file */

   if (pname[idot-1]<'0' || pname[idot-1]>'9') pname[idot-1]='1' ;
   else pname[idot-1]++ ;

   if (pname[idot-1]==':') {
      if (pname[idot-2]<'0' || pname[idot-2]>'9') pname[idot-2]='1' ;
      else pname[idot-2]++ ;

      if (pname[idot-2]==':') {
         if (pname[idot-3]<'0' || pname[idot-3]>'9') pname[idot-3]='1' ;
         else pname[idot-3]++ ;
         if (pname[idot-3]==':') derror(97) ;
         }
      }
   printf("PIC = %s\n",pname) ;                     /* display new PIC */
   fprintf(noteptr,"-- PIC = %s  ---------\n",pname) ;
   }                                                           /* not0 */
