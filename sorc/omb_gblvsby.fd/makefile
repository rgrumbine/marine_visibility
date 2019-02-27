SHELL=/bin/ksh
#
SRCS=	 blkdta000.f calvis.f findnh.f gblvsby.f grbfog.f mixr.f rdgrbsst.f smth25.f writef.f
OBJS=	 blkdta000.o calvis.o findnh.o gblvsby.o grbfog.o mixr.o rdgrbsst.o smth25.o writef.o
CMD =		omb_gblvsby
# Linux
FC =		ftn
FFLAGS =	-O0 
LDFLAGS =	-O0 
#LIBS = -L/nwprod/lib -lw3nco_4 -lw3emc_4 -lbacio_4
LIBS = $(W3NCO_LIB4) $(W3EMC_LIB4) $(BACIO_LIB4)

############################################################
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(CMD) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)
