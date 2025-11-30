IFS	=	module.a

include ../options.$(ARCH)

SOURCE	=\
Makefile.old module.a parkind1.F90.orig parkind1.mod parrrtm.F90 \
parrrtm.F90.orig parrtm1d.F90 parrtm1d.F90.orig parsrtm.F90 \
parsrtm.F90.orig random_numbers_mix.mod type_gfls.F90 yoeaeratm.F90 \
yoeaerc.F90 yoeaerd.F90 yoeaer.F90 yoeaermap.F90 yoeaermie.F90 \
yoeaerop.F90 yoeaersnk.F90 yoeaersrc.F90 yoeaerst1.F90 yoeaersu.F90 \
yoecld.F90 yoecldp.F90 yoedbug.F90 yoelw.F90 yoelwrad.F90 yoe_mcica.F90 \
yoeozoc.F90 yoephli.F90 yoephy.F90 yoerad.F90 yoeradghg.F90 yoerdi.F90 \
yoerdu.F90 yoerrta10.F90 yoerrta11.F90 yoerrta12.F90 yoerrta13.F90 \
yoerrta14.F90 yoerrta15.F90 yoerrta16.F90 yoerrta1.F90 yoerrta2.F90 \
yoerrta3.F90 yoerrta4.F90 yoerrta5.F90 yoerrta6.F90 yoerrta7.F90 \
yoerrta8.F90 yoerrta9.F90 yoerrtab.F90 yoerrtbg2.F90 yoerrtftr.F90 \
yoerrto10.F90 yoerrto11.F90 yoerrto12.F90 yoerrto13.F90 yoerrto14.F90 \
yoerrto15.F90 yoerrto16.F90 yoerrto1.F90 yoerrto2.F90 yoerrto3.F90 \
yoerrto4.F90 yoerrto5.F90 yoerrto6.F90 yoerrto7.F90 yoerrto8.F90 \
yoerrto9.F90 yoerrtrf.F90 yoerrtrwt.F90 yoerrtwn.F90 yoesrta16.F90 \
yoesrta17.F90 yoesrta18.F90 yoesrta19.F90 yoesrta20.F90 yoesrta21.F90 \
yoesrta22.F90 yoesrta23.F90 yoesrta24.F90 yoesrta25.F90 yoesrta26.F90 \
yoesrta27.F90 yoesrta28.F90 yoesrta29.F90 yoesrtaer.F90 yoesrtcop.F90 \
yoesrtop.F90 yoesrtwn.F90 yoesw.F90 yoethf.F90 yoe_uvrad.F90 yomcst.F90 \
yomct0.F90 yomct3.F90 yomdyncore.F90 yomdyn.F90 yomgc.F90 yomjfh.F90 \
yomlun.F90 yomlun_ifsaux.mod yomphy3.F90 yomphy.F90 yomrandom_streams.F90 \
yomrip.F90 yom_ygfl.F90 yophnc.F90 yos_sw.F90

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
	touch module.a

clean:
	 rm -f $(OBJECTS)
# DO NOT DELETE THIS LINE - used by make depend
yoerrtbg2.o: parrrtm.mod
yoerrtftr.o: parrrtm.mod
yoerrtrwt.o: parrrtm.mod
yom_ygfl.o: type_gfls.mod
parrrtm.mod: parrrtm.o
parrtm1d.mod: parrtm1d.o
parsrtm.mod: parsrtm.o
type_gfls.mod: type_gfls.o
yoe_mcica.mod: yoe_mcica.o
yoe_uvrad.mod: yoe_uvrad.o
yoeaer.mod: yoeaer.o
yoeaeratm.mod: yoeaeratm.o
yoeaerc.mod: yoeaerc.o
yoeaerd.mod: yoeaerd.o
yoeaermap.mod: yoeaermap.o
yoeaermie.mod: yoeaermie.o
yoeaerop.mod: yoeaerop.o
yoeaersnk.mod: yoeaersnk.o
yoeaersrc.mod: yoeaersrc.o
yoeaerst1.mod: yoeaerst1.o
yoeaersu.mod: yoeaersu.o
yoecld.mod: yoecld.o
yoecldp.mod: yoecldp.o
yoedbug.mod: yoedbug.o
yoelw.mod: yoelw.o
yoelwrad.mod: yoelwrad.o
yoeozoc.mod: yoeozoc.o
yoephli.mod: yoephli.o
yoephy.mod: yoephy.o
yoerad.mod: yoerad.o
yoeradghg.mod: yoeradghg.o
yoerdi.mod: yoerdi.o
yoerdu.mod: yoerdu.o
yoerrta1.mod: yoerrta1.o
yoerrta10.mod: yoerrta10.o
yoerrta11.mod: yoerrta11.o
yoerrta12.mod: yoerrta12.o
yoerrta13.mod: yoerrta13.o
yoerrta14.mod: yoerrta14.o
yoerrta15.mod: yoerrta15.o
yoerrta16.mod: yoerrta16.o
yoerrta2.mod: yoerrta2.o
yoerrta3.mod: yoerrta3.o
yoerrta4.mod: yoerrta4.o
yoerrta5.mod: yoerrta5.o
yoerrta6.mod: yoerrta6.o
yoerrta7.mod: yoerrta7.o
yoerrta8.mod: yoerrta8.o
yoerrta9.mod: yoerrta9.o
yoerrtab.mod: yoerrtab.o
yoerrtbg2.mod: yoerrtbg2.o
yoerrtftr.mod: yoerrtftr.o
yoerrto1.mod: yoerrto1.o
yoerrto10.mod: yoerrto10.o
yoerrto11.mod: yoerrto11.o
yoerrto12.mod: yoerrto12.o
yoerrto13.mod: yoerrto13.o
yoerrto14.mod: yoerrto14.o
yoerrto15.mod: yoerrto15.o
yoerrto16.mod: yoerrto16.o
yoerrto2.mod: yoerrto2.o
yoerrto3.mod: yoerrto3.o
yoerrto4.mod: yoerrto4.o
yoerrto5.mod: yoerrto5.o
yoerrto6.mod: yoerrto6.o
yoerrto7.mod: yoerrto7.o
yoerrto8.mod: yoerrto8.o
yoerrto9.mod: yoerrto9.o
yoerrtrf.mod: yoerrtrf.o
yoerrtrwt.mod: yoerrtrwt.o
yoerrtwn.mod: yoerrtwn.o
yoesrta16.mod: yoesrta16.o
yoesrta17.mod: yoesrta17.o
yoesrta18.mod: yoesrta18.o
yoesrta19.mod: yoesrta19.o
yoesrta20.mod: yoesrta20.o
yoesrta21.mod: yoesrta21.o
yoesrta22.mod: yoesrta22.o
yoesrta23.mod: yoesrta23.o
yoesrta24.mod: yoesrta24.o
yoesrta25.mod: yoesrta25.o
yoesrta26.mod: yoesrta26.o
yoesrta27.mod: yoesrta27.o
yoesrta28.mod: yoesrta28.o
yoesrta29.mod: yoesrta29.o
yoesrtaer.mod: yoesrtaer.o
yoesrtcop.mod: yoesrtcop.o
yoesrtop.mod: yoesrtop.o
yoesrtwn.mod: yoesrtwn.o
yoesw.mod: yoesw.o
yoethf.mod: yoethf.o
yom_ygfl.mod: yom_ygfl.o
yomcst.mod: yomcst.o
yomct0.mod: yomct0.o
yomct3.mod: yomct3.o
yomdyn.mod: yomdyn.o
yomdyncore.mod: yomdyncore.o
yomgc.mod: yomgc.o
yomjfh.mod: yomjfh.o
yomlun.mod: yomlun.o
yomphy.mod: yomphy.o
yomphy3.mod: yomphy3.o
yomrandom_streams.mod: yomrandom_streams.o
yomrip.mod: yomrip.o
yophnc.mod: yophnc.o
yos_sw.mod: yos_sw.o
