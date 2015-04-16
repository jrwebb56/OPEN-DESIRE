CC	=	gcc
LD	=	icc
HEADERS	=	global.h declare.h
SRC	=	aexcut2.c ainit.c amain4reg.c call.c  complx.c \
		def.c dprfile2.c dump.c error.c expr.c fft.c gearer.c \
		ifloop.c list.c matrix.c prcomp.c print2.c qopenf2.c \
		QQread.c rule1to8.c srun.c util.c vecdot.c  xgraph.c zdrun.c 
OBJ	=	aexcut2.o ainit.o amain4reg.o call.o  complx.o \
		def.o dprfile2.o dump.o error.o expr.o fft.o gearer.o \
		ifloop.o list.o matrix.o prcomp.o print2.o qopenf2.o \
		QQread.o rule1to8.o srun.o util.o vecdot.o  xgraph.o zdrun.o
TARGET	=	xdesire64
LIBS	=	-lm -lX11
CFLAGS	=	
CFLAGS	=	-march=native -O2 -pipe -fprefetch-loop-arrays -funroll-all-loops

$(TARGET):	$(HEADERS) $(OBJ)
		$(LD) -o $(TARGET) $(OBJ) $(LIBS)

clean:		
		rm $(TARGET)
		rm $(OBJ)	  
