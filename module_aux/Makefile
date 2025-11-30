IFS	=	module_aux.a

include ../options.$(ARCH)

SOURCE	=\
Makefile.old module_aux.a parkind1.F90 random_numbers_mix.F90 \
random_numbers_mix.F90.orig yomlun_ifsaux.F90

OBJECTS1	=	$(SOURCE:.f=.o)
OBJECTS2	=	$(SOURCE:.F90=.o)
OBJECTS = $(OBJECTS1) $(OBJECTS2)

.SUFFIXES:
.SUFFIXES:	.o .f .F90

.f.o :
	$(FC) -c $(FFLAGS) $(CP_OPT) $*.f
.F90.o :
	$(FC) -c $(FFLAGS) $(CP_OPT) $*.F90

all:	$(OBJECTS)
	touch module_aux.a

clean:
	 rm -f $(OBJECTS)
# DO NOT DELETE THIS LINE - used by make depend
random_numbers_mix.o: parkind1.mod
yomlun_ifsaux.o: parkind1.mod
parkind1.mod: parkind1.o
random_numbers_mix.mod: random_numbers_mix.o
yomlun_ifsaux.mod: yomlun_ifsaux.o
