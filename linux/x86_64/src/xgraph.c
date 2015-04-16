/*       XWindows Graphics for DESIRE/LINUX                            */

/*       Registered Copyright 2012  Granino A. Korn                    */

/***>  This source code is copyrighted and distributed under the GNU General
Public License found in the file GPL.txt in this software package.    <***/

/*                    FIXED WINDOW                              9/1/94 */

/*   LINUX version                    7/99                             */
/*** temporarily killed Error Handler  7/99                          ***/
/*   killed spare2 - no polling       2/02                             */     
/****  changed X, Y for P4            1/10/03   */
/*     added display Y, very big dots  12/13/11 */
/* ------------------------------------------------------------------- */
#include <string.h>

#include  <stdio.h>
#include  <math.h>
#include  <X11/Xlib.h>       /* note order */
#include  <X11/Xutil.h>

#include "declare.h"
#include "global.h"

extern void derror() ;  extern void prfile() ;  extern void cvalexp() ;
extern double evalexp() ;

#define  ixctr   335 
#define  ixscl1  303
#define  itctr    31 
#define  itscal  607
#define  iyctr   277 
#define  iyscl1 -266

#define  TMAX    relval[mmm+4]
#define  scale   relval[mmm+13]

#define  BORDER_WIDTH 7 

int             localflag ;                  /* flag for display label */
int             iy ;                                  /* SHOW ordinate */
int             CoordColor, CurveColor, SpareColor ;
double          xscal,yscal,tscal ;

Display         *theDisplay ;                         /* which display */
Screen          *theScreen ;            /* which screen on the display */
int             theScreenNo ;         /* what screen no on the display */
int             theDepth ;                   /* number of color planes */
unsigned long   FOREGROUND ;      /* foreground color (normally black) */
unsigned long   BACKGROUND ;      /* background color (normally white) */
Colormap        theColormap ;              /* default system color map */

Window          aParent, GraphWindow ;         /* may have more windows */
GC              GraphGC ;                                   /* and GCs */

int    X ;                       /* GraphWindow origin */ /*** <****/
int    Y ;

unsigned int    WIDTH=656 ;                                /* and size */
unsigned int    HEIGHT=600 ;

/* partial coordinate net, clip rectangle */

XRectangle      GraphArea[1]={31,9,608,533} ;
XSegment        clines[7]={{31,9,639,9},{31,144,639,144},{31,410,639,410},
			   {31,544,639,544},
			   {183,9,183,544},{487,9,487,544},{639,9,639,544}} ;

XEvent          GraphEvent ;
long int        GraphEmask = ButtonPressMask | KeyPressMask | ExposureMask ;

/*** ComposeStatus  ComposeStatus ;
     KeySym          Ksym ;
     int             Klength=8 ;
     char            KeyBuffer[9] ;   ***/

/* ------------------------------------------------------------------- */

/* convert color names to numbers */
#define  maxPixels  17
unsigned long           thePixels[maxPixels];
char                    *theColorNames[maxPixels] = {

    "White",                /* --  0  */
    "Yellow",               /* --  1  */
    "Green",                /* --  2  */
    "Cyan",                 /* --  3  */
    "Red",                  /* --  4  */
    "Violet",               /* --  5  */
    "Lime green",           /* --  6  */
    "Firebrick",            /* --  7  */
    "Light blue",           /* --  8  */
    "Magenta",              /* --  9  */
    "OrangeRed",            /* -- 10  */
    "Blue",                 /* -- 11  */
    "Brown",                /* -- 12  */
    "Turquoise",            /* -- 13  */
    "Grey",                 /* -- 14  */
    "White",                /* -- 15  */

    "Black"                 /* -- 16/switch */              
} ;

/* Functions for initial positioning of graph window */

int xpos ( Screen *s, unsigned int width )
{ 
  return WidthOfScreen(s)-width-2;
}

int ypos ( Screen *s, unsigned int height )
{
  return HeightOfScreen(s)-height-2;
}

/*  initDefaultColors attempts to set up a local color table to
**  access colors by number.  On a monochrome system, all colors
**  except the background turn into FOREGROUND color.
*/

void initDefaultColors() {

    XColor  theRGBColor, theHardwareColor;
    int     theStatus, i ;
        

    if (theDepth>1) for (i=0; i<maxPixels; i++) {         /* color */
	theStatus = XLookupColor( theDisplay,
				  theColormap,
				  theColorNames[ i ],
				  &theRGBColor,
				  &theHardwareColor ) ;
	if (theStatus != 0 ) {
	    theStatus = XAllocColor( theDisplay,
				     theColormap,
				     &theHardwareColor ) ;

	    if (theStatus!=0) thePixels[i]=theHardwareColor.pixel ;
	    else thePixels[i]=FOREGROUND ;
	}
    }
    /* monochrome */
    else for (i=0; i<maxPixels; i++) {
	if (strcmp("white",theColorNames[i])==0) thePixels[i]=BACKGROUND ;
	else thePixels[i]=FOREGROUND ;
    }
}

/* ------------------------------------------------------------------- */

void QuitGraph() {                               /* unmap graph window */

    dflag=0 ; dispp=1 ; spare3=0 ;
    XUnmapWindow(theDisplay,GraphWindow) ;
    XFreeGC(theDisplay,GraphGC) ;
    XDestroyWindow(theDisplay,GraphWindow); 
    XFlush(theDisplay) ;
}


void quitX() {                                       /* dismiss X server */

    if (spare3) {
	QuitGraph() ;
    }
    XCloseDisplay(theDisplay);
}

void FatalErrorHandler() {
    fprintf(stderr,"FATAL XWINDOWS ERROR\n\n") ; prfile(-3) ;
}

void ErrorHandler() { FatalErrorHandler() ; }


Window  openWindow( int x, int y, 
		    unsigned int width, unsigned int height, 
		    int flag, 
		    Window aParent, 
		    GC* aGC )
/* returns aWindow */
{

    Window                aWindow ;
    XSetWindowAttributes  theWindowAttributes ;
    XSizeHints            theSizeHints ; 
    unsigned long         theWindowMask ;
    XWMHints              theWMHints ;
    XGCValues             theGCValues ;
   
    theWindowAttributes.border_pixel      = 3 ;
    theWindowAttributes.background_pixel  = BACKGROUND ;
    theWindowAttributes.override_redirect = True ;
    theWindowMask = CWBackPixel | CWBorderPixel | CWOverrideRedirect ;
     
    aWindow = XCreateWindow( theDisplay,                /* open window */
                             aParent,
                             x, y, width, height,
                             BORDER_WIDTH,
                             theDepth,
                             InputOutput,
                             CopyFromParent,
                             theWindowMask,
                             &theWindowAttributes ) ;
     
    theWMHints.initial_state = NormalState;
    theWMHints.flags         = StateHint;

    XSetWMHints( theDisplay, aWindow, &theWMHints );

    theSizeHints.flags  = USPosition | USSize;         /* what we want */
    theSizeHints.x      = x;
    theSizeHints.y      = y;
    theSizeHints.width  = width;
    theSizeHints.height = height;

    XSetNormalHints( theDisplay, aWindow, &theSizeHints );

    /* set up a GC for the window */

    *aGC=XCreateGC(theDisplay,aWindow,(unsigned long) 0, &theGCValues) ;
    if (aGC==NULL) {
	XDestroyWindow(theDisplay,aWindow) ;
	fprintf(stderr,"ERROR: bad Graphics Context") ;
    }
    else {
	XSetForeground(theDisplay,*aGC,FOREGROUND) ;
	XSetBackground(theDisplay,*aGC,BACKGROUND) ;
	return(aWindow) ;                           /* return window ID */
    }
}                                                    /* openWindow */


void initX() {              /* connects X server; called by initialize */
    
    if ((theDisplay=XOpenDisplay(NULL))==NULL) 
	fprintf( stderr,"ERROR: cannot connect X Server %s\n", 
		 XDisplayName(NULL) ) ;

/***
    XSetErrorHandler(ErrorHandler) ;
    XSetIOErrorHandler(FatalErrorHandler) ;   ***/

    theScreen     = DefaultScreenOfDisplay(theDisplay) ;
    theScreenNo   = DefaultScreen(theDisplay) ;
    theDepth      = DefaultDepth(theDisplay,theScreenNo) ;  /* mono if 1 */
    FOREGROUND    = WhitePixel(theDisplay,theScreenNo) ;
    BACKGROUND    = BlackPixel(theDisplay,theScreenNo) ;
    theColormap   = DefaultColormap(theDisplay,theScreenNo ) ;
    initDefaultColors() ;                             /* set up colors */
}



void showstrt() {             /* starts or repositions graphics for SHOW */ 
    iy=-4 ; 
    if (!spare3) {
	spare3=1 ;                               /* graph and/or SHOW flag */
	GraphWindow = openWindow(X,Y,WIDTH,HEIGHT,        /* open window */
				 0,                       /* no pop-up */
				 RootWindow(theDisplay,theScreenNo), 
				 &GraphGC) ;         /* Graphics Context */

	XStoreName(theDisplay,GraphWindow,pname) ;
	XMapWindow(theDisplay,GraphWindow) ;
	XFlush(theDisplay) ; 
    }
}
 

void writarr(int xdim,int xxcols,int xaddress)    /* interpreted SHOW */
{
    int li, lk, lj , ix, icc ;

    lk=0 ; ix=31 ; if (flag>0) iy+=26 ; else iy+=48 ;         /* spacing */

    L10 :  

	for (li=0 ; li<xxcols ; li++) {
	    if ((lj=lk+li)>=xdim) goto L20 ;
	    icc=(int)relval[xaddress+lj] ;      
/* integer for color and size */
	    if (flag>0) {
		XSetForeground(theDisplay,GraphGC,
			       thePixels[(7*icc)%maxPixels]) ;
		XFillRectangle(theDisplay,GraphWindow,GraphGC,
			       ix+9*li,iy,8,10) ;
	    }
	    else {
		XSetForeground(theDisplay,GraphGC,thePixels[1]) ;
		XFillRectangle(theDisplay,GraphWindow,GraphGC,ix+17*li,iy,
			       icc%16,icc%19) ;
	    }
	}
    if (flag>0) iy+=13 ; else iy+=29 ;
    lk+=xxcols ; goto L10 ;

    L20 :

	XFlush(theDisplay) ; 
}


void cshow() {                                        /* compiled SHOW */
    int li, lk, lj, ix, icc, xdim, xxcols, xaddress ;
    xxcols=dptr[di++] ; xdim=dptr[di++] ; xaddress=dptr[di++] ;
    if (ICOM<=0) return ;              /* ICOM<=0, no communication pt. */

    lk=0 ; ix=31 ; if (flag>0) iy+=26 ; else iy+=48 ;        /* spacing */
    L10 :

	for (li=0 ; li<xxcols ; li++) {
	    if ((lj=lk+li)>=xdim) goto L20 ;
	    icc=(int)relval[xaddress+lj] ;      
/* integer for color and size */
	    if (flag>0) {
		XSetForeground(theDisplay,GraphGC,
			       thePixels[(7*icc)%maxPixels]) ;
		XFillRectangle(theDisplay,GraphWindow,
			       GraphGC,ix+9*li,iy,8,10) ;
	    }
	    else {
		XSetForeground(theDisplay,GraphGC,thePixels[16]) ;
		XFillRectangle(theDisplay,GraphWindow,GraphGC,ix+17*li,iy-24,
			       15,48) ;
		XSetForeground(theDisplay,GraphGC,thePixels[1]) ;
		XFillRectangle(theDisplay,GraphWindow,GraphGC,ix+17*li,iy,
			       icc%16,icc%19) ;
	    }
	}
    if (flag>0) iy+=13 ; else iy+=29 ;   
    lk+=xxcols ; goto L10 ;

    L20 :

	XFlush(theDisplay) ;
}


void drstart() {   /* initialize display; display coordinate net, axes */

    char  scalstr[107], SCALSTR[23], lstring[64] ;      /* scales, label */
    int  lj, k ;

    if (!spare3) {
	spare3=1 ;                               /* graph and/or SHOW flag */
	GraphWindow = openWindow(X,Y,WIDTH,HEIGHT,          /* open window */
				 0,
				 /* no pop-up */
				 RootWindow(theDisplay,theScreenNo), 
				 &GraphGC) ;
	/* Graphics Context */
	XStoreName(theDisplay,GraphWindow,pname) ;
	XMapWindow(theDisplay,GraphWindow) ;
	XFlush(theDisplay) ; 
    }
    else XClearWindow(theDisplay,GraphWindow) ;
 
    XSetClipMask(theDisplay,GraphGC,None) ; /* needed for repeated drun */
    XSetForeground(theDisplay,GraphGC,thePixels[CoordColor%maxPixels]) ;
    XSetLineAttributes(theDisplay,GraphGC,0,LineOnOffDash,
		       CapButt,JoinMiter) ;

    XDrawSegments(theDisplay,GraphWindow,GraphGC,clines,7) ;     /* net */

    if (dmode==0) 
	XDrawLine(theDisplay,GraphWindow,GraphGC,335,9,335,544) ;
    else XDrawLine(theDisplay,GraphWindow,GraphGC,31,9,31,544) ;

    XSetLineAttributes(theDisplay,GraphGC,0,LineSolid,CapButt,JoinMiter) ;

    XDrawLine(theDisplay,GraphWindow,GraphGC,31,277,639,277) ;/* x-axis */

    if (dmode==0)                                             /* y-axis */
	XDrawLine(theDisplay,GraphWindow,GraphGC,31,9,31,544) ; /* dispt */
    else XDrawLine(theDisplay,GraphWindow,GraphGC,335,9,335,544) ;

    /* display graph scales and label */

    XSetForeground(theDisplay,GraphGC,FOREGROUND) ;
    XDrawString(theDisplay,GraphWindow,GraphGC,7,16,"+",1) ;/* vertical */
    XDrawString(theDisplay,GraphWindow,GraphGC,7,280,"0",1) ;  /* scale */
    XDrawString(theDisplay,GraphWindow,GraphGC,7,548,"-",1) ;
    for (k=0 ; k<=63 ; k++) lstring[k]=blank ;  /* erase old label string */
    k=0 ;
    if (!localflag) {
	lj=dbegin ;                                /* display label */
	while (warea[lj]!=lf && warea[lj]!='|'&& k<=47) 
	    lstring[k++]=warea[lj++] ;
	if (k>=48) 
	{ lstring[k++]='.' ; lstring[k++]='.' ; lstring[k++]='.' ; }
    }
         
    if (dmode==0) {                                            /* dispt */
	if (!localflag) {
	    lstring[k++]=' ' ; lstring[k++]='v' ; lstring[k++]='s' ;
	    lstring[k++]='.' ; lstring[k++]=' ' ; lstring[k++]='t' ;
	}
	lstring[k]=EOS ;
	XDrawString(theDisplay,GraphWindow,GraphGC,287,586,lstring,k);

	tscal=itscal/relval[mmm+4] ;
	sprintf(scalstr,"%s%-9.3g%s%9.3g%s%9.3g",             /* scale */
		"   ",t0,"                                    ",
		t0+0.5*TMAX,"                                        ",
		t0+TMAX);
	XDrawString(theDisplay,GraphWindow,GraphGC,7,568,scalstr,106) ; 
/***%%%***/
    }
    else {                               /* dispxy or DISPXY */
	lstring[k]=EOS ;
	XDrawString(theDisplay,GraphWindow,GraphGC,287,586,lstring,k);
	xscal=ixscl1/scale ;
	XDrawString(theDisplay,GraphWindow,GraphGC,7,568,       /* scale */

		    "  -1.0                     -0.5                       0.0\
                      0.5                     1.0",106) ;
    }
    for (lj=7 ; lj<=22 ; lj++) SCALSTR[lj]=blank ;   /* erase old value */
    sprintf(SCALSTR,"%s%-9g"," scale = ",scale) ;              /* scale */
    XDrawString(theDisplay,GraphWindow,GraphGC,7,586,SCALSTR,17) ;
    /* label */
    /* markers */
    for (k=0 ; k<=7 ; k++) {
	XSetForeground(theDisplay,GraphGC,
		       thePixels[(CurveColor+k)%maxPixels]) ;
	XFillRectangle(theDisplay,GraphWindow,
		       GraphGC,191+9*k,578,7,7) ;
    }
    /* set up scale, clip rectangle for curves */
    yscal=iyscl1/scale ;
    XSetClipRectangles(theDisplay,GraphGC,0,0,GraphArea,1,Unsorted) ;

    XFlush(theDisplay) ;
}


void dflush() { XFlush(theDisplay) ; }    /* parameter-less for DESIRE */


void chkevent() {                                /* check for an event */
 L10 :  XSelectInput(theDisplay,GraphWindow,GraphEmask) ;
    XNextEvent(theDisplay,&GraphEvent) ;             /* wait for event */
    QuitGraph() ;
    }
    
/*******   XNextEvent(theDisplay,&GraphEvent) ;        
              switch(GraphEvent.type) {
	  case KeyPress : XLookupString(&GraphEvent,KeyBuffer,Klength,
	  &Ksym,&ComposeStatus) ;
	  if (KeyBuffer[0]=='q') 
	  XClearWindow(theDisplay,GraphEvent.xany.window) ;
	  goto L10 ;

	  case ButtonPress :    

	  XNextEvent(theDisplay,&GraphEvent) ;        
    
	  break ;
	  }                               ******/



/** void cpollX() {
 
if (XPending(theDisplay)) { 
chkevent() ; 
derror(231) ; }
}                                                               **/

/* ------------------------------------------------------------------- */

void displa() {                                  /* DISPLAY statements */

    switch(warea[gi++]) {

	case 'A':   dmode=1 ; if (spare3) QuitGraph() ; 
	    localflag=1 ; drstart() ; localflag=0 ; break ;

	case 'B':   dmode=0 ; if (spare3) QuitGraph() ;
	    t0=relval[mmm+5] ;
	    if (relval[mmm+4]<=0) relval[mmm+4]=1 ;
	    localflag=1 ; drstart() ; localflag=0 ; break ;        
	    break ;

	case 'C':   CoordColor=(int) evalexp() ; if (errnum) derror(errnum) ;
	    if (CoordColor==17) {
		BACKGROUND = WhitePixel(theDisplay,theScreenNo) ;
		FOREGROUND = BlackPixel(theDisplay,theScreenNo) ;
		CoordColor=16 ;
	    }
	    else {
		FOREGROUND = WhitePixel(theDisplay,theScreenNo) ;
		BACKGROUND = BlackPixel(theDisplay,theScreenNo) ;
	    }
	    break ;

	case 'F':   if (spare3) QuitGraph() ; system("clear") ;  break ;

	case 'N':   CurveColor=(int) evalexp() ; if (errnum) derror(errnum) ;
	    break;

	case 'Q':   bdummy=0 ; break ;                     /* thin dots */
	case 'R':   bdummy=1 ; break ;                      /* big dots */
        case 'Y':   bdummy=-1 ; break ;                /* very big dots */

	case 'S':   savindx=-1 ; break ;       /* turn on flushing */
	case 'T':    ;savindx=1 ; break ;      /* turn off flushing */ 
       
	case 'W':   X=(int) evalexp() ; if (errnum) derror(errnum) ; 
	    if (warea[gi++]!=',') derror(25) ;
	    Y=(int) evalexp() ; if (errnum) derror(errnum) ;
	    break ;
	case 'Z':  if (spare3) QuitGraph() ; break ;   /* quit graphics */
	case '0':   dispp=0 ; break ;
	case '1':   dispp=1 ; break ;
	case '2':   dispp=-1 ; break ;
	default : derror(1) ;
    }
}                                                        /* displa */


void plott() {                           /*  plots one point x,y,color */

    int  savgi,ix,iy ;  double  x,y,val ;

    savgi=gi ;
    x=evalexp() ;
    if (errnum==31) goto L11 ; if (errnum) derror(errnum) ;
    if (warea[gi++]!=',') derror(44) ;                 /* skip comma */
    y=evalexp() ; if (errnum) derror(errnum) ; goto L20 ;

    L11 : errnum=0 ; gi=savgi ;                                 /* COMPLEX */
    wptr=1 ; gjj=maxrlvar ; flevel=0 ;
    cvalexp(&x,&y) ;

    L20 : ix=ixctr+xscal*x ;  iy=iyctr+yscal*y ;

    /*     if (relval[mmm+11]!=0) {
	   if (ix<ixctr-ixscl1 || ix>ixctr+ixscl1 || iy<iyctr-iyscl1 || 
	   iy>iyctr+iyscl1)
	   relval[mmm+11]=-relval[mmm+11] ;   */        /* set flag */
    /* } */

    if (warea[gi]==lf || warea[gi]=='|') goto L30 ;      /* old color */
    if ( warea[gi++]!=',') derror(4) ;                  /* skip comma */

    CurveColor=(int) evalexp() ; if (errnum) derror(errnum); /* color */

    L30 : XSetForeground(theDisplay,GraphGC,
	 thePixels[CurveColor%maxPixels]) ;

    if (bdummy==0) XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy) ;
    else if (bdummy==1) {
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy) ;
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy) ;
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy+1) ;
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy-1) ;
    }
    else {
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy+1) ;
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy) ;
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy-1) ;
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy+1) ;
        XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy) ;
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy-1) ;
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy+1) ;
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy) ;
	XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy-1) ;
    }
    XFlush(theDisplay) ;

    /**    if (spare2) cpollX() ;  **/
} 

   

void disply() {

    int rj, ix, iy, color ;          /* color is integer for CC.ASM */

    rj=1 ; color=CurveColor ;
    if (dmode==0) ix=itctr+tscal*(relval[mmm+5]-t0) ;      /* dispt */

    else if (dmode==1) {                                  /* dispxy */
	ix=ixctr+xscal**bufdisp[1] ;
	rj=2 ;
    }

    else {                                                /* DISPXY */
	do {
	    ix=ixctr+xscal**bufdisp[rj] ;
	    iy=iyctr+yscal**bufdisp[++rj] ;

	    XSetForeground(theDisplay,GraphGC,
			   thePixels[(color)%maxPixels]) ;
           
	    if (CurveColor<15) color++ ;  
/* do curves have the same color? **/ 

	   if (bdummy==0) XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy) ;
           else if (bdummy==1) {
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy+1) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy-1) ;
           }
           else {
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy+1) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy-1) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy+1) ;
               XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy-1) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy+1) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy-1) ;
           }

	} while (bufdisp[++rj]!=NULL ) ;
	return ;
    }                                             /* else DISPXY */

    do {                                         /* dispt or dispxy */
	iy=iyctr+yscal**bufdisp[rj] ;

	XSetForeground(theDisplay,GraphGC,
		       thePixels[(color)%maxPixels]) ;

	if (CurveColor<15) color++ ;  /* do curves have the same color? **/ 

	if (bdummy==0) XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy) ;
           else if (bdummy==1) {
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy+1) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy-1) ;
           }
           else {
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy+1) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix-1,iy-1) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy+1) ;
               XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix,iy-1) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy+1) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy) ;
	       XDrawPoint(theDisplay,GraphWindow,GraphGC,ix+1,iy-1) ;
           }
    } while (bufdisp[++rj]!=NULL ) ;

    if (savindx<0) XFlush(theDisplay) ;   
    return ;

}                                                     /* disply */



void endrun() {                                      /* run-end marker */
    XDrawString(theDisplay,GraphWindow,GraphGC,160,578,">",2) ;
}
