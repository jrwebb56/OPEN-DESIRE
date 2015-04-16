/*    DESIRE - Direct Executing SImulation in REal time           */
/*           UNIX all-C version 
                                       */
/*    REGISTERED COPYRIGHT 2012extern unsigned int by Granino A. Korn                */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/
/*  GLOBAL.H - external variables for all modules  -        7/25/92    */
/*                                                                     */
/*           - arrays have one extra element to match PASCAL version   */
/***        uses marxrlvar+41                               2/14/99    */
/* 64-bit version with 3 hardware REGISTER VARIABLES - not portable  1/12/05 */
/*** NEW VERSION FOR SUSE 10, Fedora 8    11/18/06 ***/
/*   made bdummy into int for display Y   12/13/11 ***/
/* ------------------------------------------------------------------- */

register int ci asm("r13") ; 
register int di asm("r14") ;
register int gjj asm ("r15") ;

extern unsigned char cc,cf, b ; /* display colors, "thick" flag */

extern STRING sysp1 ;                        /* system-parameter names */
extern STRING func1 ;                        /* library-function names */
extern STRING w ;                            /* keyword table *//* <***/

extern unsigned int  lineno, lnumber, no ;           /* line numbers */
extern unsigned int  nn2, ncol, mparea ;   /* auto-line-no start,
					      increment, saved value */
extern  int bdummy ;                       /* for display Q,R,Y */

extern int   nn1, errnum ;         /* no. of state vars., error number */
extern int   gi ;            /* global index for user program in warea */
extern int   wptr ;        /* index or operator-stack pointer in warea */

extern int   gii,  gir ;        /* index markers in intval, relval */
extern int   cstart, dbegin, sbegin, rbegin, endtxt ; /* warea markers */
extern int   dynlin, datptr ;          /* DYNAMIC marker, data pointer */
extern int   INdex ;            /* warea index saved for stepwise runs */

extern int   flevel ;                                /* function level */
extern int   chanlno, eofflg, noteflg ;              /* I/O parameters */
extern int   hashval, vartyp;                      /* for symbol table */

extern int   runf, flag2 ;       /* program-is-running, step-run flags */
extern int   maxstk ;           /* flag indicating errors at edit/exit */
extern int   flag1 ;  /* flag to prevent new definitions in procedures */
extern int   old ;       /* flag indicates TRUE/FALSE in if statements */
extern int   xlength ;                      /* compilation-exists flag */
extern int   tracer, size ;                 /* trace and irule numbers */
extern int   start ;       /*  flag modifies getvar() to accept arrays */

extern int   disk, sflag, append ;   
/* disk, load flags;
   sflag also marks first occurrence of + in subvector declaration */

extern int   bye ;               /*flag enables error escape for input */
extern int   dflag, dmode, dispp, savindx ;           /* display flags */
extern int   nrow ;                            /* "go" (continue) flag */
extern int   spare1 ;                            /* flag for ^C signal */
extern int   spare2 ;                                      /*spare *****/
extern int   spare3 ;                     /* XWindows graphics ON flag */
extern int   flag ;                                  /* SHOW mode flag */
extern int   stashflag ;                          /* SYSPIC.DAT exists */
extern int   spare4 ;      /* killed 6/10/04, was "ed flag for SOLARIS */
extern int   spare5 ;           /* changes matrix print format in file */
extern int   editflg ;       /* no longer  controls erun after editing */

extern int   IDERIV,ICOM,INITE1,CHECKN ;
/**** multiple definition?   <*****/
extern int   SPARE1, SPARE2, SPARE3, SPARE4 ;       /** SPARE3 killed **/ 
/* SPARE1 causes run in prcomp; SPARE4 marks "bye" in excut2 for prcomp */

/*** extern int   raddr, rrr, para, iseed, bdummy1, stog0, dim, drunr,
     eflag ; ***/
extern int   intval[maxinvar+1] ;                   /* stores integers */

/* -------------------------------------------------------------------- */

extern double  hdt ;                               /* for integration */
extern double  accum, scale, t0, zCOMINT, ztnext ;

extern double  relval[maxrlvar+41] ;   /* REAL variables *//*** <*****/
extern double  storesv[maxnsv+1] ;            /* stores inital values */
extern double  sderiv[maxnsv+1] ;                /* state derivatives */
extern double  dpsv[maxnsv+1] ;  /* temporary storage for integration */

extern ptr  p, q, hashtab[maxhash+1] ;  /* global pointers, hash table */
extern dsptr  tptr, stktop ;          /* global control-stack pointers */

extern double  *bufdisp[15], *bufstash[21], *bufrecvr[21] ;

/* -------------------------------------------------------------------- */

extern int  dptr[maxparam+1] ;               /* compiler data pointers */
extern int  jcstart,jdstart ;           /* for compiler arrays */

/* -------------------------------------------------------------------- */

extern FILE *inptr ;                                  /* file pointers */
extern FILE *outptr ;
extern FILE *noteptr ;                             /* for journal file */
extern FILE *fdn ;                              /* for save,list,list+ */
extern FILE *fd[maxchan+1] ;                       /* for write, input */

/* -------------------------------------------------------------------- */
/* STRING BUFFERS */

extern unsigned char  symbol[varsize+1] ;              /* identifier */
extern unsigned char  pname[maxnam+1] ;      /* file or device names */
extern unsigned char  fname[maxnam+1] ;
extern unsigned char  dlabl[maxcmd+1] ;   /* OS shell command string */
extern unsigned char  warea[maxtxt+1] ;  
/* user text area, also used
   for command line and for interpreter and compiler operator stacks */
