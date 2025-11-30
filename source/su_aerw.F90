SUBROUTINE SU_AERW (KIDIA, KFDIA, KLON, &
  & PGELAM, PGEMU )

!**** *SU_AERW*   - DEFINES INDICES AND PARAMETERS FOR VARIOUS AEROSOL VARIABLES

!     PURPOSE.
!     --------
!           INITIALIZE YOEAERATM, YOEAERSRC, YOEAERSNK, THE MODULES THAT CONTAINS INDICES
!           ALLOWING TO GET THE AEROSOL PARAMETERS RELEVANT FOR THE PROGNOSTIC AEROSOL
!           CONFIGURATION.

!**   INTERFACE.
!     ----------
!        *CALL* *SU_AERW

!        EXPLICIT ARGUMENTS :
!        --------------------
!        NONE

!        IMPLICIT ARGUMENTS :
!        --------------------
!        YOEAERATM, YOEAERSRC

!     METHOD.
!     -------
!        SEE DOCUMENTATION

!     EXTERNALS.
!     ----------

!     REFERENCE.
!     ----------
!        ECMWF RESEARCH DEPARTMENT DOCUMENTATION OF THE IFS

!     AUTHOR.
!     -------
!        JEAN-JACQUES MORCRETTE *ECMWF*

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 2005-07-08

!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE YOEAERATM, ONLY : LAERGBUD, LAERSCAV ,LAERSEDIM, LAERSURF, LAER6SDIA, &
 & LAERCLIMG, LAERCLIMZ, LAERCLIST, LAERDRYDP, LAERGTOP, LAERNGAT, LAERPRNT, &
 & LAERHYGRO, LAERCCN  , LAEROPT  , LAERRAD  , LUVINDX , REPSCAER, NAERCONF, &
 & NXT3DAER , NINIDAY
USE YOEAERMAP ,ONLY : RDDUAER
USE YOEAERSRC, ONLY : LEPAERO , NAERWND, NAERDUT, &
 & NBINAER, NINDAER , NMAXTAER, NTAER  , NTYPAER, RGELAV , RGEMUV , &
 & RDGLAV , RDGMUV  , RCLONV  , RSLONV , RDCLONV, RDSLONV, LAEREXTR, &
 & RSSFLX , RLATVOL , RLONVOL , NDDUST , NSSALT , RAERDUB, &
 & RBCPHIL, RBCPHOB , ROMPHIL , ROMPHOB, RFCTSS , RFCTDU , &
 & RFCTDUR, RFCTSSR , RSO2SO4 , JKBIN  , JKTYP

USE YOEPHY   , ONLY : LE4ALB

USE YOEDBUG  , ONLY : KSTPDBG , NSTPDBG

USE YOMCST   , ONLY : RPI
!USE YOMGC    , ONLY : GELAM, GEMU
!USE YOMGEM   , ONLY : NGPTOT
!USE YOMLEG   , ONLY : RMU
USE YOMLUN   , ONLY : NULNAM, NULOUT
USE YOMRIP   , ONLY : NINDAT, NSSSSS

USE YOM_YGFL , ONLY : NAERO, NACTAERO

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA, KFDIA, KLON
REAL(KIND=JPRB) :: PGELAM(KLON), PGEMU(KLON)


INTEGER(KIND=JPIM) :: IAER, ICAER, ITAER
INTEGER(KIND=JPIM) :: J, JAER, JL

!-- map 
INTEGER(KIND=JPIM) :: IFF, IAERWND
REAL(KIND=JPRB) :: ZDDUAER(35,2)
REAL(KIND=JPRB) :: ZBNDA, ZBNDB, ZBNDC, ZBNDD, ZBNDE, ZBNDF, ZBNDG, ZBNDH, &
  & ZBNDI, ZBNDJ, ZBNDK, ZBNDL, ZBNDM
REAL(KIND=JPRB) :: ZDEGRAD, ZINCLAT, ZLAT, ZLON, ZLONE, ZLONGB, ZLONW
CHARACTER(LEN=45) :: CLAERWND(0:3)

!     ----------------------------------------------------------------


!      ----------------------------------------------------------------

#include "naeaer.h"

!      ----------------------------------------------------------------

!*       1.       DEFAULT VALUES OF PARAMETERS
!                 ----------------------------

NMAXTAER=8
NBINAER(:) = (/ 3, 3, 2, 2, 2, 1, 1, 1 /)

LEPAERO  =.FALSE.
LAERCLIMG=.FALSE.
LAERCLIMZ=.FALSE.
LAERDRYDP=.FALSE.
LAEREXTR =.FALSE.
LAERGBUD =.FALSE.
LAERGTOP =.FALSE.
LAERHYGRO=.FALSE.
LAERNGAT =.FALSE.
LAERPRNT =.FALSE.
LAERSCAV =.FALSE.
LAERSEDIM=.FALSE.
LAERSURF =.FALSE.
LAER6SDIA=.FALSE.
LAERCCN  =.FALSE.
LAERRAD  =.FALSE.
LAEROPT(:)=.FALSE.
LUVINDX  =.FALSE.

LAERCLIST=.FALSE.

!- note that NAERCONF is now irrelevant
NAERCONF=-99
NXT3DAER=0
NINIDAY=19000101

NTAER  =0
!-- default values
!-- DDust is "a la Ginoux et al., 2001"   =f(DEP*WND3 +filter on ALUV and FrBaS)
NDDUST =7
RAERDUB=2.E-11_JPRB
!-- SSalt is Monahan et al. 1986
NSSALT =1

! the 8 types and assumed number of bins are:
!  NTYPAER    bins  type
!     1       1- 3  sea-salt  0.03 - 0.5 -  5  - 20 microns
!     2       4- 6  dust      0.03 - 0.5 - 0.9 - 20 microns
!     3       7- 8  POM	    hydrophilic, hydrophobic
!     4       9-10  BC	    hydrophilic, hydrophobic
!     5      11-12  SO4/SO2 including sulfate prognostic stratospheric aerosols (SO4 is 11)
!     6      13     fly ash
!     7      14     pseudo-prognostic stratospheric aerosols
!     8      15     pseudo-prognostic volcanic aerosols
 
DO JAER=1,NMAXTAER
  NTYPAER(JAER)=0
ENDDO

RLATVOL=-999._JPRB
RLONVOL=-999._JPRB
RGELAV =-999._JPRB
RGEMUV =-999._JPRB
RDGLAV = 999._JPRB
RDGMUV = 999._JPRB
RCLONV =-999._JPRB
RSLONV =-999._JPRB
RDCLONV= 999._JPRB
RDSLONV= 999._JPRB

NSTPDBG=10
DO J=1,NSTPDBG
  KSTPDBG(J)=-999
ENDDO

REPSCAER=1.E-20_JPRB

RBCPHIL = 0.8_JPRB
RBCPHOB = 1.0_JPRB-RBCPHIL
ROMPHIL = 0.5_JPRB
ROMPHOB = 1.0_JPRB-ROMPHIL
RSO2SO4 = 0.25_JPRB

ZDDUAER(:,1) = 1.00_JPRB
ZDDUAER(:,2) = 0.10_JPRB
RDDUAER(:) = 0.0_JPRB
NAERDUT=0

!-- default values are for use of 10-m wind as predictor for SS and DU
NAERWND    = 0
IAERWND    = 1
RFCTDU     = 1.0_JPRB
RFCTSS     = 1.0_JPRB  
RFCTDUR    = 0.40_JPRB
RFCTSSR    = 0.52_JPRB
CLAERWND(0) = '10-M WIND AS PREDICTOR FOR SS AND DU         '
CLAERWND(1) = 'PREDICTORS: WIND GUST FOR SS, 10M-WIND FOR DU'
CLAERWND(2) = 'PREDICTORS: WIND GUST FOR DU, 10M-WIND FOR SS'
CLAERWND(3) = 'WIND GUST AS PREDICTORS FOR SS AND DU        '

!     ------------------------------------------------------------------

!*       2.       INITIALIZE GEOGRAPHICALLY-DEPENDENT AEROSOL PARAMETERS
!                 ------------------------------------------------------

! Canada                     1	     1.    0.1
! Alaska                     2	     1.    0.1
! USA                        3	     1.    0.08
! Central America            4	     0.60  0.05
! South America              5	     0.60  0.01
! Brazil                     6 	     1.    0.1
! Iceland                    7 	     1.    0.1
! Ireland                    8 	     1.    0.1
! Britain                    9 	     1.    0.1
! Cont'al Europe            10 	     1.    0.1
! Russia (Europe)           11 	     1.    0.1
! Russia (Georgia)          12       1.    0.02

! Northern Sahara           13	     0.70  0.17
! Central Africa            14	     0.50  0.11
! Southern Africa           15	     0.60  0.04

! Siberia                   16 	     1.    0.1
! Asian deserts             17	     1.    0.1 

! Saudi Arabia              18	     0.65  0.12
! Irak, Iran, Pakistan      19	     0.65  0.12

! Central Asia Taklamakan   20	     1.6   0.025
! India                     21	     1.2   0.09

! Mongolia and Gobi         22	     2.0   0.22
! Central China             23	     2.0   0.25

! South China     	    24	     0.5   0.1
! Japan, South Korea        25	     1.0   0.1

!-- padding Asia            26 	     0.5   0.1
!
! Tropical Pacific Islands  27	     1.    0.1
! Australia. New Zealand    28	     0.5   0.03
!				    
! Greenland                 29	     1.    0.1
! Antarctica                30	     1.    0.1

! Additional areas

! Atacama and Uyuni         31	     0.5   0.02
! Pipanaco and others       32	     0.8   0.02
! Argentinian pampas        33	     0.8   0.02
! Southern Sahara (West)    34	     0.70  0.13
! Southern Sahara (East)    35	     0.70  0.15
!==============================

! USA                        3
ZDDUAER( 3,1) = 1.00_JPRB
!!!!ZDDUAER( 3,2) = 0.10_JPRB
ZDDUAER( 3,2) = 0.08_JPRB

! Central America            4
ZDDUAER( 4,1) = 0.60_JPRB
ZDDUAER( 4,2) = 0.05_JPRB

! South America              5
ZDDUAER( 5,1) = 0.60_JPRB
ZDDUAER( 5,2) = 0.01_JPRB

! Russia (Georgia)          12
ZDDUAER(12,1) = 1.00_JPRB
ZDDUAER(12,2) = 0.02_JPRB

! Northern Sahara           13
! Central Africa            14
! Southern Africa           15
ZDDUAER(13,1) = 0.70_JPRB
ZDDUAER(14,1) = 0.50_JPRB
ZDDUAER(15,1) = 0.60_JPRB

!!!!ZDDUAER(13,2) = 0.40_JPRB
ZDDUAER(13,2) = 0.17_JPRB
ZDDUAER(14,2) = 0.11_JPRB
ZDDUAER(15,2) = 0.04_JPRB

! Siberia                   16 
! Asian deserts             17

ZDDUAER(16,1) = 1.00_JPRB
ZDDUAER(17,1) = 1.00_JPRB

ZDDUAER(16,2) = 0.10_JPRB
ZDDUAER(17,2) = 0.10_JPRB

! Saudi Arabia              18
! Irak, Iran, Pakistan      19
ZDDUAER(18,1) = 0.65_JPRB
ZDDUAER(19,1) = 0.65_JPRB

ZDDUAER(18,2) = 0.12_JPRB
ZDDUAER(19,2) = 0.12_JPRB

! Central Asia Taklamakan   20
! India                     21
ZDDUAER(20,1) = 1.60_JPRB
ZDDUAER(21,1) = 1.20_JPRB

ZDDUAER(20,2) = 0.025_JPRB
ZDDUAER(21,2) = 0.09_JPRB

! Mongolia and Gobi         22
! Central China             23
ZDDUAER(22,1) = 2.00_JPRB
ZDDUAER(23,1) = 2.00_JPRB

ZDDUAER(22,2) = 0.22_JPRB
ZDDUAER(23,2) = 0.25_JPRB

! South China     	    24
! Japan, South Korea        25
ZDDUAER(24,1) = 0.50_JPRB
ZDDUAER(25,1) = 1.00_JPRB

ZDDUAER(24,2) = 0.10_JPRB
ZDDUAER(25,2) = 0.10_JPRB

!-- padding Asia            26 
ZDDUAER(26,1) = 0.50_JPRB
ZDDUAER(26,2) = 0.10_JPRB

! Tropical Pacific Islands  27
! Australia. New Zealand    28
ZDDUAER(27,1) = 1.00_JPRB
ZDDUAER(28,1) = 0.50_JPRB

ZDDUAER(27,2) = 0.10_JPRB
ZDDUAER(28,2) = 0.03_JPRB

! Atacama and other areas, Argentinian pampas
ZDDUAER(31,1) = 0.50_JPRB
ZDDUAER(32,1) = 0.80_JPRB
ZDDUAER(33,1) = 0.80_JPRB

ZDDUAER(31,2) = 0.02_JPRB
ZDDUAER(32,2) = 0.02_JPRB
ZDDUAER(33,2) = 0.02_JPRB

! Southern Sahara West
ZDDUAER(34,1) = 0.70_JPRB
ZDDUAER(34,2) = 0.13_JPRB

! Southern Sahara East
ZDDUAER(35,1) = 0.70_JPRB
ZDDUAER(35,2) = 0.13_JPRB

!     ------------------------------------------------------------------

!*       2.       READ VALUES OF PROGNOSTIC AEROSOL CONFIGURATION
!                 -----------------------------------------------

!--- for this 1D package, make sure prognostic aerosols are off
NAERO=0
WRITE(NULOUT,8000) NAERO
8000 FORMAT(1X,'Read or not the namelist? ',I3)

IF(NAERO > 0) THEN
! CALL POSNAM(NULNAM,'NAEAER')
! READ (NULNAM,NAEAER)
ENDIF

IF (NINDAT == NINIDAY .AND. NSSSSS == 00000) THEN
  LAERCLIST=.TRUE.
ENDIF

!     ------------------------------------------------------------------

!*       3.       DISTRIBUTE DUST AEROSOL SOURCE FUNCTIONS
!                 ----------------------------------------

!-- if NAERWND < 2, DU uses 10m-wind; if >= 2, DU uses gust

if (NAERWND == 0) THEN
!  for NAERWND  = 0, both SS and DU use 10-m wind
  RFCTDU=1.0_JPRB
  RFCTSS=1.0_JPRB
  IAERWND=1
elseif (NAERWND == 1) THEN
!  for NAERWND  = 1, SS takes "gust" values
  RFCTDU=1.0_JPRB
  RFCTSS=RFCTSSR
  IAERWND=1
elseif (NAERWND == 2) THEN
!  for NAERWND  = 2, DU takes "gust" values
  RFCTDU=RFCTDUR
  RFCTSS=1.0_JPRB
  IAERWND=2
elseif (NAERWND == 3) THEN
!  for NAERWND  = 3, both SS and DU take 10-m wind + gust
  RFCTDU=RFCTDUR
  RFCTSS=RFCTSSR
  IAERWND=2
endif

WRITE(UNIT=NULOUT,FMT='(''RDDUAERS= '',35F5.2)') (RDDUAER(J),J=1,35)
  
ZDEGRAD=180._JPRB/RPI
!DO JL=1,NGPTOT
!  ZLON=GELAM(JL)*ZDEGRAD
!  ZLAT=ASIN(GEMU(JL))*ZDEGRAD
DO JL=KIDIA,KFDIA
  ZLON=PGELAM(JL)*ZDEGRAD
  ZLAT=ASIN(PGEMU(JL))*ZDEGRAD

  zbnda= 30._JPRB+(36._JPRB -zlat)*14._JPRB/24._JPRB
  zbndb= 30._JPRB+(36._JPRB -zlat)*40._JPRB/16._JPRB
  zbndc= 38._JPRB+(zlon-124._JPRB)*12._JPRB/29._JPRB
  zbndd= 32._JPRB-(zlon-243._JPRB)* 6._JPRB/21._JPRB

!-- Eastern border Canada/USA
  zbnde= 49._JPRB
  if (zlon > 268._JPRB .and. zlon < 277._JPRB) then
    zbnde= 49._JPRB-(zlon-268._JPRB)*7._JPRB/9._JPRB
  elseif (zlon >= 277._JPRB .and. zlon < 285._JPRB) then
    zbnde= 42._JPRB+(zlon-277._JPRB)*2._JPRB/8._JPRB
  elseif (zlon >= 285._JPRB .and. zlon < 310._JPRB) then
    zbnde= 44._JPRB+(zlon-285._JPRB)*3._JPRB/25._JPRB
  endif

!-- limits Britain
  zlongb=-9999._JPRB
  if (zlon > 354._JPRB .and. zlon < 360._JPRB) then
    zlongb=zlon
  elseif (zlon >= 0._JPRB .and. zlon < 3._JPRB) then
    zlongb=zlon+360._JPRB
  endif
  zbndf= 47._JPRB+(zlongb-349._JPRB)*4.5_JPRB/14._JPRB

!-- limits Ireland
  zbndg= 61._JPRB-(zlon-349._JPRB)*7._JPRB/6._JPRB
  zbndh= 45._JPRB+(zlon-349._JPRB)*9._JPRB/6._JPRB

!-- Western border Brazil
  if (zlat <= 4._JPRB .and. zlat > 2._JPRB) then
    zbndi= 296._JPRB
  elseif (zlat <= 2._JPRB .and. zlat > -4._JPRB) then
    zbndi= 290._JPRB
  elseif (zlat <= -4._JPRB .and. zlat > -7._JPRB) then
    zbndi= 290._JPRB-(-4._JPRB-zlat)*4._JPRB/3._JPRB
  elseif (zlat <= -7._JPRB .and. zlat > -11._JPRB) then
    zbndi= 286._JPRB+(-7._JPRB-zlat)*4._JPRB/4._JPRB
  endif

  if (zlat <= -11._JPRB .and. zlat > -18._JPRB) then
    zbndj= 294._JPRB+(-11._JPRB-zlat)*8._JPRB/7._JPRB
  elseif (zlat <= -18._JPRB .and. zlat > -27._JPRB) then
    zbndj= 302._JPRB+(-18._JPRB-zlat)*4._JPRB/9._JPRB
  elseif (zlat <= -27._JPRB .and. zlat > -30._JPRB) then
    zbndj= 306._JPRB-(-27._JPRB-zlat)*3._JPRB/3._JPRB
  elseif (zlat <= -30._JPRB .and. zlat >= -34._JPRB) then
    zbndj= 303._JPRB+(-30._JPRB-zlat)*4._JPRB/4._JPRB
  endif

!-- Northern border India
  if (zlon > 70._JPRB .and. zlon <= 90._JPRB) then
    zbndk= 35._JPRB-(zlon-70._JPRB)*0.5_JPRB
  endif

!-- South border of Asian deserts
  if (zlon > 90._JPRB .and. zlon <= 135._JPRB) then
    zbndl= 25._JPRB+(zlon-90._JPRB)*15._JPRB/45._JPRB
  endif

!-- North limit of the Argentinian pampas
  if (zlon > 285._JPRB .and. zlon <= 297._JPRB) then
    zbndm= -42._JPRB+(zlon-285._JPRB)*6._JPRB/12._JPRB 
  endif

  iff=0
 
!-- North America
!----- Canada
  if ( zlat >= zbnde .and.                      &
    &      (zlon > 190._JPRB .and. zlon < 330._JPRB) ) then
    iff=1
!----- USA
  elseif ( (zlat >= zbndd .and. zlat < zbnde ) &
    &      .and. (zlon > 190._JPRB .and. zlon < 330._JPRB) ) then
    iff=3
  endif
!-- Alaska
  if ( (zlat < 72._JPRB .and. zlat > 52._JPRB) &
    & .and. (zlon > 190._JPRB .and. zlon <= 219._JPRB) ) then
    iff=2
  endif

!-- Central America
  if (zlat < zbndd .and. &
    & (zlon > 190._JPRB .and. zlon < 330._JPRB) ) then
    iff=4
  endif

!-- South America
  if ( zlat < 12._JPRB .and. &
    & (zlon > 190._JPRB .and. zlon < 330._JPRB) ) then
    iff=5
  endif
!-- Brazil
  if ( (zlat <= 4._JPRB .and. zlat > 2._JPRB) &
    & .and. (zlon >= 296._JPRB .and. zlon <= 300._JPRB) ) then
    iff=6 
  endif
  if ( (zlat <= 2._JPRB .and. zlat > -11._JPRB) &
    & .and. (zlon >= zbndi .and. zlon < 330._JPRB) ) then
    iff=6
  endif
  if ( (zlat <= -11._JPRB .and. zlat >= -34._JPRB) &
    & .and. (zlon >= zbndj .and. zlon < 330._JPRB) ) then
    iff=6
  endif

!-- Western Europe
  if ( zlat > 36._JPRB .and. ( zlon >= 330._JPRB .or. zlon <= 30._JPRB) ) then
    iff=10
  endif

!----- Iceland
  if ( (zlat < 67._JPRB .and. zlat > 63._JPRB) &
    & .and. ( zlon > 335._JPRB .and. zlon < 353._JPRB) ) then
    iff=7
  endif
!----- Britain  
  if ( (zlat < 63._JPRB .and. zlat > zbndf) &
    & .and. ( zlon > 354._JPRB .or. zlon < 3._JPRB) ) then
    iff=9
  endif
!----- Ireland
  if ( (zlat < zbndg .and. zlat > zbndh) &
    & .and. ( zlon > 349._JPRB .and. zlon < 355._JPRB) ) then
    iff=8
  endif


!-- Russia to Urals
  if ( zlon > 30._JPRB .and. zlon <= 70._JPRB ) then
    if ( zlat > 51._JPRB ) then
      iff=11
    elseif ( zlat > 36._JPRB ) then
      iff=12
    endif
  endif

!-- Northern Sahara
  if ( ( zlat <= 36._JPRB .and. zlat >= 21._JPRB) &
    & .and. ( zlon >= 330._JPRB .or. zlon <= zbnda) ) then
    iff=13
!-- Southern Sahara (West)
  elseif ( ( zlat <= 21._JPRB .and. zlat >= 12._JPRB) &
    & .and. ( zlon >= 330._JPRB .or. zlon < 15._JPRB) ) then
    iff=34
!-- Southern Sahara (East)
  elseif ( ( zlat <= 21._JPRB .and. zlat >= 12._JPRB) &
    & .and. ( zlon >= 15._JPRB .and. zlon <= zbnda) ) then
    iff=35
!-- Central Africa
  elseif ( (zlat < 12._JPRB .and. zlat >= -12._JPRB) &
    & .and. ( zlon >= 330._JPRB .or. zlon <= 60._JPRB) ) then
    iff=14
!-- Southern Africa
  elseif ( zlat < -12._JPRB .and. zlat >= -60._JPRB &
    & .and. ( zlon >= 330._JPRB .or. zlon <= 60._JPRB) ) then
    iff=15
  endif


!-- Australasia
  if (zlon > 70._JPRB .and. zlon <= 190._JPRB) then
    iff=26

!-- Siberia
    if (zlat <= 90._JPRB .and. zlat > 51._JPRB) then
      iff=16

!-- Australasia
!---- Tropical Pacific Islands
    elseif ( zlat > -10.5_JPRB) then
      iff=27
!---- Australia
    elseif ( zlat <= -10.5_JPRB .and. zlat >= -60._JPRB) then
      iff=28
    endif
  endif

!-- Asian deserts
  if ((zlon > 90._JPRB .and. zlon <= 135._JPRB) &
    & .and. (zlat <= 51._JPRB .and. zlat > zbndl)) then
      iff=17
  endif



!-- Saudi Arabia
 if ((zlat <= 36._JPRB .and. zlat >= 12._JPRB) &
    & .and.( zlon > zbnda .and. zlon < zbndb) ) then
    iff=18
  endif
!-- Irak, Iran, Pakistan
  if ((zlat <= 36._JPRB   .and. zlat >= 20._JPRB) &
    & .and.( zlon > zbndb .and. zlon < 70._JPRB) ) then
    iff=19
  endif



!-- Central Asia and India
  if ( zlon > 70._JPRB .and. zlon <= 90._JPRB) then
!----- Central Asia: Taklamakan
    if (zlat <= 43._JPRB .and. zlat >= zbndk) then
      iff=20
!----- India
    elseif (zlat <= zbndk .and. zlat > 7._JPRB) then
      iff=21
    endif
  endif
!-- other Gobi(s) in South Mongolia and Central China
  if ( zlat <= 49._JPRB .and. zlat > 35._JPRB) then
    if (zlon > 90._JPRB .and. zlon <= 110._JPRB)  then
      iff=22
    elseif (zlon > 110._JPRB .and. zlon <= 125._JPRB) then
      iff=23
    endif
  endif

!-- South China  
  if ( (zlon > 90._JPRB .and. zlon <= 135._JPRB) .and. &
    & (zlat <= zbndl .and. zlat > 7._JPRB) ) then
      iff=24
  endif



!-- Japan and S.Korea
  if ( (zlon > 124._JPRB .and. zlon < 153._JPRB) &
    & .and. (zlat > 24._JPRB .and. zlat < zbndc) ) then
    iff=25
  endif

!-- Greenland
  if (zlat > 50._JPRB) then
    zinclat=(90._JPRB-zlat)/40._JPRB*45._JPRB
    zlonw=270._JPRB +zinclat
    zlone=360._JPRB -zinclat
    if ( zlon > zlonw .and. zlon <  zlone ) then
      iff=29
    endif
  endif

!-- Antarctica
  if (zlat < -60._JPRB) then
    iff=30
  endif

!-- awaiting a proper recoding, new areas are set between iff=31 and 35

  if ( zlon > 285._JPRB .and. zlon < 295._JPRB) then
!- Atacama desert and Salar de Uyuni
    if ( zlat < -16._JPRB  .and. zlat > -28._JPRB) then
      iff=31
    endif
!- Salar de Pipanaco and other small ones
    if ( zlat <= -28._JPRB .and. zlat >  zbndm) then
      iff=32
    endif
  endif
!- Argentianian pampas
  if ( (zlon > 285._JPRB .and. zlon < 297._JPRB) &
    & .and. zlat <= zbndm ) then
    iff=33
  endif

!-- if area is recognized       
  IF (IFF /= 0) THEN
!-- either use the default value
    IF (RDDUAER(iff) == 0._JPRB) THEN
      RDDUAER(iff) = ZDDUAER(iff,IAERWND)
    ELSE
!-- or what is read in via the namelist
    ENDIF
  ELSE
    WRITE(NULOUT,FMT='(''su_aerw: Unassigned grid for Lat,Lon='',2F8.2)') ZLAT,ZLON
  ENDIF
ENDDO

!     ------------------------------------------------------------------

!*       4.       DEFINE VALUES OF PROGNOSTIC AEROSOL CONFIGURATION
!                 -------------------------------------------------

IF (.NOT.LE4ALB) THEN
  NDDUST=2
ENDIF

WRITE(UNIT=NULOUT,FMT='(''NAERO='',I2,'' NACTAERO='',I2)') NAERO,NACTAERO

IF (NACTAERO > 0) THEN

  WRITE(UNIT=NULOUT,FMT='(''NAERO='',I2,'' NACTAERO='',I2,3X,8I3)') NAERO,NACTAERO,(NTYPAER(JAER),JAER=1,8)

! define a composite index for each bin of each different aerosol type to be used
! in source, sedimentation and deposition routines
 
  ICAER=0
  DO JAER=1,NMAXTAER
    IF (NTYPAER(JAER) /= 0) THEN
      NTAER=NTAER+1
      ITAER=NTYPAER(JAER)
      DO IAER=1,ITAER
        ICAER=ICAER+1
        NINDAER(ICAER)=JAER*10+IAER
        JKTYP(ICAER)  =JAER
        JKBIN(ICAER)  =IAER
      ENDDO
    ENDIF
  ENDDO

!-- if volcanic aerosols, define the model coordinates

!  IF (NTYPAER(8) /= 0) THEN
!    RGEMUV=(RLATVOL+90._JPRB)*RPI/180._JPRB
!    RGELAV=RLONVOL*RPI/180._JPRB
!    RCLONV=COS(RGELAV)
!    RSLONV=SIN(RGELAV)
!    DO J=1,NGPTOT-1
!      IF (RGELAV > GELAM(J) .AND. RGELAV <= GELAM(J+1) .AND. &
!        & RGEMUV < RMU(JL) .AND. RGEMUV >= RMU(JL+1) ) THEN
!        RDGMUV=ABS( RMU(J+1) - RMU(J))
!        RDGLAV=ABS( GELAM(J+1)-GELAM(J) )
!        RDSLONV=ABS( SIN(GELAM(JL+1))-SIN(GELAM(JL)) )
!        RDCLONV=ABS( COS(GELAM(JL+1))-COS(GELAM(JL)) )
!      ENDIF
!    ENDDO
!  ENDIF  

!     ------------------------------------------------------------------

!*       5.    INITIALIZE PROGNOSTIC AEROSOL PHYSICAL AND OPTICAL PARAMETERS
!              -------------------------------------------------------------

  CALL SU_AERP
  CALL SU_AEROP

  DO JAER=1,NMAXTAER
    IF (LAEROPT(JAER)) THEN
      LAERRAD=.TRUE.
    ENDIF
  ENDDO

!      ----------------------------------------------------------------

!*       6.    PRINT FINAL VALUES.
!              -------------------

  WRITE(UNIT=NULOUT,FMT='('' LEPAERO = '',L5 &
   & ,'' NTAER   = '',I2 ,'' NDDUST = '',I1,'' NSSALT = '',I1,/&
   & ,'' NTYPAER = '',8I3,/ &
   & ,'' NBINAER = '',8I3,/ &
   & ,'' NINIDAY = '',I8    &
   & ,'' JKTYP   = '',15I3,/&
   & ,'' JKBIN   = '',15I3  &
   & )')&
   & LEPAERO,NTAER,NDDUST,NSSALT,(NTYPAER(JAER),JAER=1,8),(NBINAER(JAER),JAER=1,8), &
   & NINIDAY, (JKTYP(JAER),JAER=1,15), (JKBIN(JAER),JAER=1,15)
    
  WRITE(UNIT=NULOUT,FMT='('' NAERO = '',I2,'' NACTAERO = '',I2,&
   &'' NXT3DAER = '',I2,'' NAERCONF = '',I3)') &
   & NAERO,NACTAERO,NXT3DAER,NAERCONF

  WRITE(UNIT=NULOUT,FMT='('' LAERGBUD = '',L1 &
   & ,'' LAERNGAT = '',L1 &
   & ,'' LAERDRYDP= '',L1 &
   & ,'' LAERSEDIM= '',L1 &
   & ,'' LAERGTOP = '',L1 &
   & ,'' LAERHYGRO= '',L1 &
   & ,'' LAERSCAV = '',L1 &
   & ,'' LAER6SDIA= '',L1 &
   & ,'' LAERCLIMZ= '',L1 &
   & ,'' LAERCLIMG= '',L1 &
   & ,'' LAERCLIST= '',L1 &
   & )')&
   & LAERGBUD,LAERNGAT, LAERDRYDP,LAERSEDIM,LAERGTOP,LAERHYGRO,LAERSCAV,LAER6SDIA, &
   & LAERCLIMZ,LAERCLIMG,LAERCLIST

  WRITE(UNIT=NULOUT,FMT='('' RSSFLX= '',10E10.3)') RSSFLX
  WRITE(UNIT=NULOUT,FMT='('' NAERWND= '',I1,'' RFCTSS= '',F4.1,'' RFCTDU= '',F4.1,2X,A45)') &
   & NAERWND, RFCTSS, RFCTDU, CLAERWND(NAERWND)
  WRITE(UNIT=NULOUT,FMT='('' NAERDUT= '',I2,'' RAERDUB= '',E10.3,'' RSO2SO4= '',F5.2)') &
   & NAERDUT, RAERDUB, RSO2SO4
  WRITE(UNIT=NULOUT,FMT='(''RDDUAERE= '',35F5.2)') (RDDUAER(J),J=1,35)
  WRITE(UNIT=NULOUT,FMT='('' LUVINDX= '',L3    )') LUVINDX

  WRITE(UNIT=NULOUT,FMT='('' Interaction prognostic aerosols and radiation: LAERRAD= '',L1,&
    &'' LAEROPT= '',8L3)') LAERRAD,LAEROPT
  WRITE(UNIT=NULOUT,FMT='('' Interaction prognostic aerosols and eff.radius of liq.wat.clouds: LAERCCN= '',&
    &L1)') LAERCCN

  IF (NTYPAER(8) /= 0) THEN
    WRITE(UNIT=NULOUT,FMT='('' RLATVOL= '',F5.2 &
     & ,'' RLONVOL= '',F6.2,'' RGEMUV= '',F6.4,'' RGELAV= '',F6.4 &
     & ,'' RCLONV = '',F6.4,'' RSLONV= '',F6.4,'' RDGMUV= '',F6.4 &
     & ,'' RDGLAV = '',F6.4,'' RDCLONV= '',F6.4,'' RDSLONV= '',F6.4 &
     & )')&
     & RLATVOL,RLONVOL,RGEMUV,RGELAV,RCLONV,RSLONV,RDGMUV,RDGLAV,RDCLONV,RDSLONV
  ENDIF
ENDIF

!     ----------------------------------------------------------------
END SUBROUTINE SU_AERW




































