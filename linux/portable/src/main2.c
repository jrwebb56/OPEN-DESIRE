/*    DESIRE/X - Direct Executing SImulation in REal time  - LINUX     */

/*    REGISTERED COPYRIGHT 2002 by Granino A. Korn                     */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/*  MAIN.C - main DESIRE program, global variables            11/4/93  */

/*         - added delta, DELTA                                4/97    */
/** - added keep+, Vector, Mat d/dt, Delta,    
                         add 40 to compiler stack              6/99    */
/*  - changed initializers for LINUX                           7/25/99 */

/*** new signal handling                                       8/3/01  */
/*   added NEW                                                 4/2/02  */
/*   added Vectr d/dt, Vectr delta, SUBMODEL                   9/13/02 */

/***         9/13/02  added Vectr d/dt, Vectr delta, SUBMODEL          */
/***         9/18/02  added MM to slow display rate                    */

/***         9/20/02  INITE1, ICOM, IDERIV are now global integer flags
                        and zCOMINT, ztnext are global double          */
                              
/*         - error return to prompt                                    */
                    
/*         - arrays have one extra element to match PASCAL version     */
/* ------------------------------------------------------------------- */
#include <setjmp.h>
#include <signal.h>

#include "declare.h"
/* ------------------------------------------------------------------ */
                                                  /* STRING CONSTANTS */

STRING sysp1="NN$CHECKN$DT$TMAX$t$ERRMAX$DTMAX$DTMIN$ERMAX$ERMIN$Tevent$\
iRow$scale$PI$MM$" ;

STRING func1="ln(tim(sin(cos(exp(abs(sqrt(trnc(lim(sgn(ran(atan(round(\
Re(Im(Cabs(Arg(Conjug(expj(log(tan(sinh(cosh(acos(asin(tanh(atan2(\
swtch(SAT(dxXr(sat(deadz(deadc(rect(comp(tri(sigmoid(SIGMOID(sinc(recip(" ;

                                                     /* keyword array */

STRING w="else$connect$if$while$repeat$for$until$next$end while$\
end$proceed$STOP$drunr$PROCEDURE$erase$list$run$data$go to$input$\
save$..$then$FUNCTION$old$load$clear$reset$chain$FFT$to$dump$\
call$time$read$disconnect$delete$drun$list+$new$irule$size$\
help$display$STATE$eof$go$note$dimension$edit$auto$ed$bye$\
trace$write$plot$INTEGER$VECTOR$DOT$MATRIX$sh$DYNAMIC$restore$\
PIC$JACOBIAN$label$MACRO$COMPLEX$invoke$..$--$term$d/dt$or$\
dispt$CLEARN$tdelay$store$SAMPLE$get$type$delay$trkhld$stash$OUT$func$\
dispxy$recover$MAT d/dt$step$as input$as output$and$exit$DISPXY$count$\
SEED$ARRAY$UPDATE$LEARN$SHOW$INTP$keep+$PLEARN$reload$keep$erun$rld$NEW$\
..$delta$DELTA$Vector$Mat d/dt$Delta$func2$vv$Vectr d/dt$Vectr delta$SUBMODEL$"  ;

/* ------------------------------------------------------------------- */

                                              /* VARIABLE DECLARATIONS */

/* NOTE: these variable names are the same as in older DESIRE programs */

unsigned char cc, cf, bdummy ;      /* cc and cf are spares under UNIX */

unsigned int   lineno, lnumber, no ;                   /* line numbers */
unsigned int   mparea=10, nn2=200, ncol=0 ;  /* auto-incr., start,
                                                               current */
int   errnum=0, endtxt=linsiz+2, size=1, cstart=1, dispp=1 ;
int   tracer=0, disk=1, append=0, start=0, dynlin=0, nrow=0 ;
int   bye=1 , flag2=0 ;
int   nn1=0 ;                                    /* no. of state vars. */
int   INITE1, ICOM, IDERIV ;
int   gi, gii, gjj, gir ;
int   flevel, chanlno ;
int   sbegin, rbegin, hashval, vartyp, xlength ;
int   dmode, dbegin, wptr, datptr, rrr, maxstk ;
int   flag, savindx, INdex, eofflg, notefl ;
int   dflag, sflag, runf, old, flag1, spare1, spare2, spare3 ;
int   stashflag ;
int   spare4, spare5, editflg ;
int   SPARE1, SPARE2, SPARE3, SPARE4 ;

/*** int raddr, rrr, para, iseed, bdummy1, stog0, dim, drunr, eflag ; ***/

int   intval[maxinvar+1] ;
/* ------------------------------------------------------------------- */

double  accum, scale, t0, hdt, zCOMINT, ztnext ;

double  relval[maxrlvar+41] ;    /* REAL variables, and compiler stack */
double  storesv[maxnsv+1] ;                    /* stores inital values */
double  sderiv[maxnsv+1] ;                        /* state derivatives */
double  dpsv[maxnsv+1] ;          /* temporary storage for integration */

ptr  p, q, hashtab[maxhash+1] ;      /* global pointers and hash table */
dsptr  tptr, stktop=NULL ;

double  *bufdisp[15],*bufstash[21], *bufrecvr[21] ;

      /*****> *bufdisp[11] in some versions; fix global.h in that case */
/* ------------------------------------------------------------------- */

void  (*COMPILE[maxcomp+1])() ;          /* compiled function pointers */
int   dptr[maxparam+1] ;                /* compiled parameter pointers */
int   ci, di, jcstart, jdstart ;             /* compiler-array indices */
/* ------------------------------------------------------------------- */

FILE *inptr ;                                  /****>   for load, edit */
FILE *outptr ;                                     /****>    for write */
FILE *noteptr ;                                    /* for journal file */
FILE *fdn ;                      /****>    for save, list, list+, type */
FILE *fd[maxchan+1] ;                              /* for write, input */
/* ------------------------------------------------------------------- */
                                                     /* STRING BUFFERS */
unsigned char  symbol[varsize+1] ;
unsigned char  pname[maxnam+1] ;
unsigned char  fname[maxnam+1] ;
unsigned char  warea[maxtxt+1] ;                     /* user text area */
unsigned char  dlabl[maxcmd+1] ;                   /* OS shell command */

/* ------------------------------------------------------------------- */

jmp_buf djump ;                           /* type declared in setjmp.h */

extern  void derror()  ;      extern  void clsall() ;
extern  void initialize() ;   extern  void prepr1() ;
extern  void precomp();       extern  void execute() ;
extern  void scroll() ;
/* ------------------------------------------------------------------- */

void keyboard_intr(int sig_num) {

   sigset_t mask_set ;
   sigset_t old_set ;   

   signal(SIGINT,keyboard_intr) ;             /* reset signal handling */
                                 
   sigfillset(&mask_set) ;        /* mask signals while inside handler */
   sigprocmask(SIG_SETMASK,&mask_set,&old_set) ;
   
   spare1=1 ;                                            /* ctl-c flag */

   sigprocmask(SIG_SETMASK,&old_set,NULL) ; /* restore old signal mask */
   
   }

void float_err(int sig_num) {
   signal(SIGFPE,float_err);
   derror(3) ;
   }

void bus_intr(int sig_num) { 
   signal(SIGBUS,bus_intr) ;
   derror(11) ; 
   }

void seg_intr(int sig_num) { 
   signal(SIGSEGV,seg_intr) ;
   derror(113) ; 
   }

void abt_intr(int sig_num) { 
   signal(SIGABRT,abt_intr) ;
   derror(113) ; 
   }

/* ------------------------------------------------------------------ */

main(argc)                            /* checks command-line argument */
   int  argc ; {

   initialize() ;   /* symbol table, flags, display, get command line */

   if (argc>1) {                            /* load last user program */

         ncol=10 ; nn2=200 ; append=TRUE ; eofflg=FALSE ;
         if ((inptr=fopen("SYSPIC.lst","r"))==NULL) {
            WRITELN ; exit(0) ;
            }
         }

   if (setjmp(djump)) goto prompt ;    /* error etc. return to prompt */

   signal(SIGFPE,float_err);
   signal(SIGINT,keyboard_intr);
   signal(SIGBUS,bus_intr) ;
   signal(SIGSEGV,seg_intr) ;
   signal(SIGABRT,abt_intr) ;
 
 prompt:

   precomp() ;                               /* main interpreter loop */
   execute() ;
   goto prompt ;
   }
