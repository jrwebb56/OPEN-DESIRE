/*    DESIRE - Direct Executing SImulation in REal time                */
/*           and DESIRE for Linux                                      */

/*    REGISTERED COPYRIGHT 2002 by Granino A. Korn                     */

/* DECLARE.H  - Includes and Declarations for all Modules     4/22/93  */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/***  increased program size   3/17/97, 4/19/97 ,8/4/98, 2/14/99, 9/14/02    <***/

/**   introduced Vectr d/dt, Vectr delta, SUBMODEL            9/13/02 **/

/* ------------------------------------------------------------------- */

#include <stdio.h>
/*                 include <stdlib.h>    */
#include <math.h>

/* ------------------------------------------------------------------- */

/*  CONSTANT DEFINITIONS  */

#define     maxint 2147483647

#define     BASE 256          /* for two-char line-number coding */
#define     maxlin 65535      /* largest (unsigned) line number */
#define     linsiz 236        /* terminal buffer size */
#define     maxchan 10        /* max number of channels - 1 */
#define     maxext 4          /* max. file extension size incl. '.' */
#define     maxnam 50         /* path name size - incl. extension etc. */
#define     maxdim 10         /* max. number of dimensions in an array */
#define     varsize 8         /* max. identifier length, also in DEF */
#define     maxcmd 55         /* max. length of command string */
#define     maxhash 499       /* length of hash table */
#define     maxtxt 100000     /* max. user text area in bytes $$$$$ */
#define     maxcomp 500000    /* max. no. of compiled functions $$$$$ */
#define     maxparam 500000   /* max. no. of compiled data addresses $$$$$ */
#define     maxinvar 4000     /* max. number of INTEGER variables $$$$$ */
#define     maxrlvar 150000   /* max. number of REAL variables  $$$$$  <****/
#define     maxnsv 20000      /* max. number of state variables $$$$$ <**ok**/
#define     maxsysp 25        /* max. number of simulation parameters */
#define     mmm maxnsv+4      /* a base in relval array */
#define     EQUMAX 400        /* max. number of GEAR etc. state vars. $$$$$ <**ok**/

/* ------------------------------------------------------------------- */

#define  EOS '\0'
#define  tab  '\t'
#define lf  '\n'
#define ranscale  (2/maxint)

#define  chr11  11  /* = velse */

#define  vopen  12
#define  vif    13
#define  vwhil  14
#define  vrept  15
#define  vfor   16
#define  vuntil 17
#define  vnext  18
#define  vendwh 19
#define  vretrn 20
#define  vendls 21
#define  vstop  22

#define  vproc  24
#define  vdata  28

 ;
#define  blank   ' '   /* 32 */

#define  vthen   '!'   /* 33 */
#define  vto     ')'   /* 41 */
#define  veof    '8'   /* 56 */
#define  vat     'G'   /* 71 */
#define  vdynmc  'H'   /* 72 */
#define  vlabel  'L'   /* 76 */
#define  vmacro  'M'   /* 77 */
#define  vlet    'P'   /* 80 */
#define  vrem    'Q'   /* 81 */
#define  vddt    'S'   /* 83 */
#define  vor     'T'   /* 84 */
#define  vstep   'd'   /* 100 */
#define  vinput  'e'   /* 101 */
#define  voutput 'f'   /* 102 */
#define  vand    'g'   /* 103 */

#define  vMAT    vand+21   /* 124 */
#define  vDelta  vand+22   /* 125 */
#define  vVecdt  vand+25   /* 128 */
#define  vVecdel vand+26   /* 129 */
#define  vSUBMOD vand+27   /* 130 */

#define  ESC     '~'   /* 126, replaces escape character */
                                 /* same as vfunc2 !!! */
#define TRUE     1
#define FALSE    0

/* ------------------------------------------------------------------- */

#define WRITELN putchar('\n')

/* ------------------------------------------------------------------- */

                                                  /* TYPE DECLARATIONS */
typedef char  *STRING ;
typedef unsigned char  symbl[varsize+1] ;
typedef unsigned char  filext[maxext+1] ;


                             /* linked-list structure for symbol table */
typedef struct symnode* ptr ;
typedef struct symnode {
   symbl   name ;
   int     symtype ;
   int     valptr ;
   ptr     link ;
   ptr     next ;
   int     SWITCH ;
   } SYMNODE ;

                                 /* structure for loop/PROCEDURE stack */
typedef struct dstak* dsptr ;
typedef struct dstak {
   unsigned char    head ;                            /* type of entry */
   int     radd ;                                    /* return address */
   ptr     cvar ;              /* points to loop control variable etc. */
   double  fval ;                              /* FOR loop final value */
   float   step ;                                     /* FOR loop step */
   dsptr   dslink ;                                      /* stack link */
   int     tskptr ;                             /* for interrupts etc. */
   } DSTAK ;

