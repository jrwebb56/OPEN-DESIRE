/* MATRIX.C    Interpreter matrix routines II    - OK for UNIX         */
/*             Registered Copyright 1991  G.A. Korn            12/9/91 */

/***>  This source code is copyrighted and distributed under the GNU General
    Public License found in the file GPL.txt in this software package.    <***/

/*                                                                     */
/* ------------------------------------------------------------------- */
/*  NOTE:   relval[start0 + (ROW-1)*nrow + (COL-1)] is B[ROW,COL] .    */
/*  matrices are stored in ROW-major order; start0 indexes B[1,1].     */
/* ------------------------------------------------------------------- */

#include "declare.h"
#include "global.h"

extern void derror() ;     extern void matprt() ;

extern ptr getvar() ;      extern ptr instal() ;     extern int getsub() ;
extern double getoprnd() ; extern double evalexp() ;

int  NROW, NROW0, start0, start1, start11, irefrow ;         /* global */

void pivot(int irefrow)

/* search irefrow COLUMN of A for the first non-zero element below
   diagonal; if found, switch ROWS to place it on diagonal.  If no
   non-zero element exists, matrix is singular (error 123). Apply
   same switch to B */
{
    int inewrow, temp1, temp2, k ;  double dummy ;
    
    errnum=123 ;  /* assume A is singular; find a diag. elmnt. 0!=0 */
    for (inewrow=irefrow+1 ; inewrow<NROW ; inewrow++) {
	if (relval[start11+inewrow*NROW+irefrow]!=0) {
	    temp1=start0+inewrow*NROW ;           /* switch rows of A */
	    temp2=start0+irefrow*NROW ;
	    for (k=0 ; k<NROW ; k++) {
                dummy=relval[temp1+k]  ;          /* dummy row = row 1 */
                relval[temp1+k]=relval[temp2+k] ;     /* row 1 = row 2 */
                relval[temp2+k]=dummy ;           /* row 2 = dummy row */
	    }
	    temp1+=start11-start0 ;             /* switch rowwes of B */
	    temp2+=start11-start0 ;
	    for (k=0 ; k<NROW ; k++) {
                dummy=relval[temp1+k]  ;          /* dummy row = row 1 */
                relval[temp1+k]=relval[temp2+k] ;     /* row 1 = row 2 */
                relval[temp2+k]=dummy ;           /* row 2 = dummy row */
	    }
	    errnum=0 ; break ;                  /* maybe not singular */
	}                                                   /* if */
    }                                                     /* for */
}                                                      /* pivot */


void invert() {                                  /* MATRIX  B = $In(A) */

    int  temp1, temp2, k, j ;  double  mult, mult1 ;

    start11=gir+1 ; if (start11+NROW0 >= gjj) derror(40) ;
    for (k=0 ; k<NROW0 ; k++) {
	relval[start11+k]=relval[start1+k] ;      /* use a copy of A */
	relval[start0+k]=0 ;                   /* and start with B=I */
    }
    for (k=0 ; k<NROW ; k++) relval[start0+k*(NROW+1)]=1 ;
    irefrow=0 ;                          /* Make A upper triangular */
    while (irefrow<NROW) {
	if (relval[start11+irefrow*(NROW+1)]==0) {
	    pivot(irefrow) ; if (errnum) derror(errnum) ;   /* diag=0 */
	}
	mult=relval[start11+irefrow*(NROW+1)] ;         /* new diag. */
	if (mult==0) derror(123) ;
	mult=1/mult ;
	/* multiply a row of each matrix by mult */

	temp1=start0+irefrow*NROW ;
	for (k=temp1 ; k<temp1+NROW ; k++) relval[k]*=mult ;
	temp1+=start11-start0 ;
	for (k=temp1 ; k<temp1+NROW ; k++) relval[k]*=mult ;

	/* make irefrow element of this row 0 */

	for (k=0 ; k<NROW ; k++) if (k!=irefrow) {
	    mult1=relval[start11+k*NROW+irefrow] ;
	    if (mult1!=0) {
                mult=relval[start11+irefrow*(NROW+1)] ;
                if (mult==0 ) derror(123) ;
                mult= -mult1/mult ;

		/* multiply refrow of the matrix by mult and add to kth row */

                temp1=start0+k*NROW ;  temp2=start0+irefrow*NROW ;
                for (j=0 ; j<NROW ; j++)
		    relval[temp1+j]+=relval[temp2+j]*mult ;
                temp1+=start11-start0 ;  temp2+=start11-start0 ;
                for (j=0 ; j<NROW ; j++)
		    relval[temp1+j]+=relval[temp2+j]*mult ;
	    }                                                /* if */
	}                                                  /* for */
	irefrow++ ;
    }                                                   /* while */
}                                                     /* invert */

/* ------------------------------------------------------------------- */

void  matrix() {                                   /* MATRIX statement */

    int  i, j, k,  first, minus ; double  val ;  ptr  qy ;

    start=1 ;                                    /* makes vartyp=-1 */
    p=getvar() ; if (errnum) derror(errnum) ;
    if (p==NULL || p->symtype!=-1) derror(70) ; /* undef or un-REAL */
    i=p->valptr ;         /* must be at least 2 by 2, square matrix */
    if (intval[i]!=2) derror(74) ;        /* must have 2 dimensions */
    NROW=intval[i+1] ; NROW0=intval[i+2] ; start0=intval[i+3] ;

    if (warea[gi++]!='=') derror(29) ;                 /* no = sign */
    if (warea[gi]!='0' && warea[gi]!='1' ||
	warea[gi+1]!=lf && warea[gi+1]!='|') goto L8 ;

    if (NROW0!=NROW) derror(74) ;         /* 0 or 1 must be square! */
    NROW0=NROW*NROW ;                  /* number of matrix elements */
    for (k=start0 ; k<start0+NROW0 ; k++) relval[k]=0 ;      /* A=0 */
    if (warea[gi++]=='1') for (k=0 ; k<NROW ; k++)           /* A=1 */
	relval[start0+k*(NROW+1)]=1 ;
    goto L5 ;                               /* skip 0 or 1 and quit */

    L8 :   first=TRUE ;                                 /* first-pass flag */
    if (warea[gi]=='$') goto L100 ;         /* transpose or inverse */
    if (warea[gi]=='-') { minus=TRUE ; gi++ ; } else minus=FALSE ;
    start=0 ; i=gi ;       /* try for scalar multiplier, mark place */
    val=getoprnd() ;
    if (errnum) {
	if (errnum!=73) derror(errnum) ;               /* true error */
	goto L10 ;                                   /* not a scalar */
    }

    if (minus) val= -val ;                                /* scalar */
    if (NROW0!=NROW) derror(74) ;            /* must be square now! */
    NROW0=NROW*NROW ;                  /* number of matrix elements */
    if (warea[gi++]!='*') derror(75) ;

    start=1 ;                                    /* prepare for ... */
    qy=getvar() ; if (errnum) derror(errnum) ;  /* ... next operand */
    if (qy==NULL || qy->symtype!= -1) derror(70) ;
    if (qy==p) derror(85) ;                    /* illegal recursion */
    i=qy->valptr ;
    if (intval[i]!=2 || intval[i+1]!=NROW ||intval[i+2]!=NROW) derror(74) ;
    start1=intval[i+3] ;

    for (k=0 ; k<NROW0 ; k++) relval[start0+k]=val*relval[start1+k] ;
    goto L15 ;

    L10 :  errnum=0 ; gi=i ;               /* not a scalar, try for matrix */
    wptr=1 ; gjj=maxrlvar ; flevel=0 ;              /* reset stacks */

    L12 :  start=1 ;                                     /* try for matrix */
    qy=getvar() ; if (errnum) derror(errnum) ;
    if (qy==NULL || qy->symtype!= -1) derror(70) ;
    if (qy==p) derror(85) ;                    /* illegal recursion */
    i=qy->valptr ;
    if (intval[i]!=2) derror(74) ;        /* must have 2 dimensions */
    start1=intval[i+3] ;                                   /* ABASE */

    if (warea[gi]=='%') {                              /* transpose */
	if(!first || intval[i+1]!=NROW0 || intval[i+2]!=NROW) derror(74) ;
	for (i=0 ; i<NROW ; i++) for (k=0 ; k<NROW0 ; k++)
	    relval[start0+k+i*NROW0]=relval[start1+i+k*NROW] ;
	gi++ ; goto L5 ;                                   /* skip % */
    }
    else if (intval[i+2]!=NROW) {                     /* not square */
	if (!first || intval[i+1]!=NROW || intval[i+2]!=NROW0) derror(74) ;
	NROW0*=NROW ;                  /* number of matrix elements */
	for (k=0 ; k<NROW0 ; k++) relval[start0+k]=relval[start1+k] ;
	goto L5 ;
    }
    /* must be square */
    if (intval[i+1]!=NROW || intval[i+2]!=NROW) derror(74) ;
    NROW0=NROW*NROW ;          /* square, number of matrix elements */

    if (first) {
	if (!minus) for (k=0 ; k<NROW0 ; k++)
	    relval[start0+k]=relval[start1+k] ;
	else for (k=0 ; k<NROW0 ; k++)
	    relval[start0+k]=-relval[start1+k] ;
    }
    else {
	if (gir+NROW0>=gjj) derror(40) ;
	for (k=0 ; k<NROW0 ; k++) relval[gir+k]=relval[start0+k] ;
	for (i=0 ; i<NROW ; i++) for (k=0 ; k<NROW ; k++) {
	    val=0 ;
	    for (j=0 ; j<NROW ; j++)
		val+=relval[gir+i*NROW+j]*relval[start1+j*NROW+k] ;
	    relval[start0+i*NROW+k]=val ;
	}
    }

    L15 :  if (warea[gi]!='*') goto L5 ; gi++ ;                  /* skip * */
    first=FALSE ;
    goto L12 ;

    L100 : if (NROW0!=NROW) derror(74) ;                 /* must be square */
    NROW0=NROW*NROW ;                  /* number of matrix elements */
    if (warea[gi+3]!='(') derror(75) ;
    if (warea[gi+1]=='T' && warea[gi+2]=='r') first=FALSE ;
    else if (warea[gi+1]!='I' || warea[gi+2]!='n') derror(75) ;
    /* first=TRUE for inversion */
    gi+=4 ;                  /* transpose or inverse, skip beyond ( */
    qy=getvar() ; if (errnum) derror(errnum) ;
    if (qy==NULL || qy->symtype!=-1) derror(70) ;
    if (qy==p) derror(85) ;
    i=qy->valptr ;
    if (intval[i]!=2 || intval[i+1]!=NROW || intval[i+2]!=NROW) derror(74) ;
    start1=intval[i+3] ;

    if (first) { invert() ; if (errnum) derror(errnum) ; }
    else for (i=0 ; i<NROW ; i++) for (k=0 ; k<NROW ; k++)
	relval[start0+k+i*NROW]=relval[start1+i+k*NROW] ;/* transpose */
    if (warea[gi++]!=')') derror(33) ;                    /* skip ) */

    L5 :   start=0 ;                                               /* done */
    if (tracer>=4) {
	printf("%c%s",'\33',"[7m") ;           /* ANSI reverse video */
	matprt() ;
	printf("%c%s",'\33',"[0m") ;           /* ANSI restore video */
    }
}                                                     /* matrix */

