SUBROUTINE SU_AERP

!**** *SU_AERP*   - INITIALIZE MODULES YOEAERSRC, YOEAERSNK

!     PURPOSE.
!     --------
!           INITIALIZE YOEAERSRC AND YOEAERSNK, THE MODULES THAT CONTAINS 
!           COEFFICIENTS NEEDED TO RUN THE PROGNOSTIC AEROSOLS

!**   INTERFACE.
!     ----------
!        *CALL* *SU_AERP

!        EXPLICIT ARGUMENTS :
!        --------------------
!        NONE

!        IMPLICIT ARGUMENTS :
!        --------------------
!        YOEAERSRC, YOEAERSNK, YOEAERATM

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
!        from O.BOUCHER (LOA, 1998-03) 

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 2004-05-10
!        JJMorcrette 20070222 9bins for SS and DU for testing
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPRB , JPIM

USE YOEAERSRC , ONLY : RSSFLX, RSSLIM, NMAXTAER, NTYPAER

USE YOEAERSNK , ONLY : R_R, R_S, RALPHAR, RALPHAS, RFRAER, RFRGAS, &
  &  RRHMAX, RRHTAB, RRHO_SS, RSSGROW, RMMD_SS, RMMD_DD, RRHO_DD, &
  &  RFRBC , RFRIF , RFROM  , RFRSO4 , RFRDD  , RFRSS , RHO_WAT, RHO_ICE, &
  &  RVDPOCE, RVDPSIC, RVDPLND, RVDPLIC, RVSEDOCE, RVSEDSIC, RVSEDLND, RVSEDLIC, &
  &  NBRH

USE YOEAERATM , ONLY : RGRATE, RMASSE, RMFMIN, NDD1, NSS1

IMPLICIT NONE


!*       0.5   LOCAL VARIABLES
!              ---------------

INTEGER(KIND=JPIM) :: IDU, ISS

!-- sea-salt (3 or 9 bins)
REAL(KIND=JPRB) :: ZSSFLX3(3),ZMMD_SS3(3),ZFRSS3(3),ZVDEPOCS3(3),ZVDEPSIS3(3),ZVDEPLDS3(3),ZVDEPLIS3(3)
REAL(KIND=JPRB) :: ZSSFLX9(9),ZMMD_SS9(9),ZFRSS9(9),ZVDEPOCS9(9),ZVDEPSIS9(9),ZVDEPLDS9(9),ZVDEPLIS9(9)
REAL(KIND=JPRB) :: ZVSEDOCS3(3),ZVSEDSIS3(3),ZVSEDLDS3(3),ZVSEDLIS3(3)
REAL(KIND=JPRB) :: ZVSEDOCS9(9),ZVSEDSIS9(9),ZVSEDLDS9(9),ZVSEDLIS9(9)
REAL(KIND=JPRB) :: ZSSLIM3(4)

!-- desert dust (3 or 9 bins)
REAL(KIND=JPRB) :: ZMMD_DD3(3),ZRHO_DD3(3),ZFRDD3(3),ZVDEPOCD3(3),ZVDEPSID3(3),ZVDEPLDD3(3),ZVDEPLID3(3)
REAL(KIND=JPRB) :: ZMMD_DD9(9),ZRHO_DD9(9),ZFRDD9(9),ZVDEPOCD9(9),ZVDEPSID9(9),ZVDEPLDD9(9),ZVDEPLID9(9)
REAL(KIND=JPRB) :: ZVSEDOCD3(3),ZVSEDSID3(3),ZVSEDLDD3(3),ZVSEDLID3(3)
REAL(KIND=JPRB) :: ZVSEDOCD9(9),ZVSEDSID9(9),ZVSEDLDD9(9),ZVSEDLID9(9)
REAL(KIND=JPRB) :: ZSSLIM9(10)

!     ----------------------------------------------------------------

!-- For the ECMWF model, the following tables when dimensioned to 12 
!   can refer to 12 values of RH
!      (RHTAB, RHHO_SS, RSSGROW)
!   or to 12 types/bins of aerosols with the following mapping: NTYPAER
!   1- 3  sea-salt  0.03 - 0.5 -  5  - 20 microns                  1
!   4- 6  dust      0.03 - 0.5 - 0.9 - 20 microns                  2
!   7- 8  POM       hydrophilic, hydrophobic                       3
!   9-10  BC        hydrophilic, hydrophobic                       4
!  11-12  sulfate   11: SO4     12:SO2                             5
!  13     fly ash                                                  6
!  14     stratospheric aerosols                                   7
!  15     volcanic aerosols                                        8
!      (RVDPOCE, RVDSIC, RVDPLND, RVDPLIC)
!      (RVSEDOCE,RVSEDSIC,RVSEDLND,RVSEDLIC)

!-- parameters are:
! RFRxx    efficiency for scavenging
! RVDPyy   efficiency for dry deposition
! RVSEDyy  efficiency for sedimentation



!*      0.    INITIALISATION
!             --------------

RSSLIM(:)  = 0._JPRB
RSSFLX(:)  = 0._JPRB
RMMD_SS(:) = 0._JPRB
RFRSS(:)   = 0._JPRB

RMMD_DD(:) = 0._JPRB
RRHO_DD(:) = 0._JPRB
RFRDD(:)   = 0._JPRB

RVDPOCE(:) = 0._JPRB
RVDPSIC(:) = 0._JPRB
RVDPLND(:) = 0._JPRB
RVDPLIC(:) = 0._JPRB

RVSEDOCE(:)= 0._JPRB
RVSEDSIC(:)= 0._JPRB
RVSEDLND(:)= 0._JPRB
RVSEDLIC(:)= 0._JPRB



!*      1.    PARAMETERS RELATED TO AEROSOL TYPES
!             -----------------------------------

! maximum possible number of aerosol types
NMAXTAER=8


!*      2.    PARAMETERS RELATED TO SOURCES/SINKS
!             -----------------------------------

R_R = 0.001_JPRB
R_S = 0.001_JPRB

RFRAER = 0.5_JPRB
RFRGAS = 1.0_JPRB

!*      2.1   SEA SALT
!             -------- 
!-- parameters related to SEA SALT: 12 relates to 12 values of relative humidity

NBRH=12
RRHMAX = 95._JPRB
RRHTAB = (/ 0._JPRB, 10._JPRB, 20._JPRB, 30._JPRB, 40._JPRB, 50._JPRB &
       & , 60._JPRB, 70._JPRB, 80._JPRB, 85._JPRB, 90._JPRB, 95._JPRB /)
RRHO_SS = (/ 2160._JPRB, 2160._JPRB, 2160._JPRB, 2160._JPRB, 1451.6_JPRB &
     & , 1367.9_JPRB, 1302.9_JPRB, 1243.2_JPRB, 1182.7_JPRB, 1149.5_JPRB &
     & , 1111.6_JPRB, 1063.1_JPRB /)
RSSGROW = (/ 0.503_JPRB, 0.503_JPRB, 0.503_JPRB, 0.503_JPRB, 0.724_JPRB &
         & , 0.782_JPRB, 0.838_JPRB, 0.905_JPRB, 1.000_JPRB, 1.072_JPRB &
         & , 1.188_JPRB, 1.447_JPRB /)
RHO_WAT = 1000._JPRB
RHO_ICE = 500._JPRB


!-- PARAMETERS DEPENDENT ON NUMBER OF BINS
!   -------------------------------------- 

!-- ECMWF 3 bins of sea salt
!  bins are 0.03 - 0.5 -  5.0  - 20 microns
ZSSLIM3 = (/ 0.03_JPRB, 0.5_JPRB, 5._JPRB, 20._JPRB /)

!N.B. Fluxes of sea salt for each size bin are given in mg m-2 s-1 at wind 
!     speed of 1 m s-1 at 10m height (at 80% RH) in OB's seasalt.F
!     RSSFLX also in mg m-2 s-1       
ZSSFLX3  = (/ 4.85963536E-09_JPRB, 4.15358556E-07_JPRB, 5.04905813E-07_JPRB /)

ZMMD_SS3 = (/ 0.30_JPRB, 3.00_JPRB, 10.00_JPRB /)
ZFRSS3   = (/  0.5_JPRB,  0.5_JPRB,   0.5_JPRB /)

!-  computed off-line by gems_ss.f (in vsed_ss.txt, 9th colum is 80%)

ZVSEDOCS3 = (/  0.240E-04_JPRB,  0.195E-02_JPRB,  0.180E-01_JPRB /)
ZVSEDSIS3 = (/  0.240E-04_JPRB,  0.195E-02_JPRB,  0.180E-01_JPRB /)
ZVSEDLDS3 = (/  0.240E-04_JPRB,  0.195E-02_JPRB,  0.180E-01_JPRB /)
ZVSEDLIS3 = (/  0.240E-04_JPRB,  0.195E-02_JPRB,  0.180E-01_JPRB /)


! adapted from LMDZ   (m s-1)

ZVDEPOCS3  = (/ 0.100E-02_JPRB , 0.110E-01_JPRB ,  0.145E-01_JPRB /)
ZVDEPSIS3  = (/ 0.100E-02_JPRB , 0.110E-01_JPRB ,  0.145E-01_JPRB /)
ZVDEPLDS3  = (/ 0.100E-02_JPRB , 0.110E-01_JPRB ,  0.145E-01_JPRB /)
ZVDEPLIS3  = (/ 0.100E-02_JPRB , 0.110E-01_JPRB ,  0.145E-01_JPRB /)


!-- ECMWF 9 bins of sea salt
!  bins are 0.03 - 0.05 - 0.10 - 0.20 - 0.50 - 1.00 - 2.00 - 5.00 - 10.00 - 20.00 microns
ZSSLIM9 = (/ 0.03_JPRB, 0.05_JPRB, 0.1_JPRB, 0.2_JPRB, 0.5_JPRB, &
  &            1._JPRB,   2._JPRB,  5._JPRB, 10._JPRB, 20._JPRB /)

!N.B. Fluxes of sea salt for each size bin are given in mg m-2 s-1 at wind 
!     speed of 1 m s-1 at 10m height (at 80% RH) in OB's seasalt.F
!     RSSFLX also in mg m-2 s-1       
ZSSFLX9 = (/ 1.36417419E-10_JPRB, 3.46521922E-10_JPRB, 7.53073937E-10_JPRB, 3.62362051E-09_JPRB, &
  &          1.62356013E-08_JPRB, 8.62133902E-08_JPRB, 3.12909776E-07_JPRB, 2.46328341E-07_JPRB, &
  &          2.58577359E-07_JPRB /)

!-- empirically adjusted
ZMMD_SS9 = (/  0.042_JPRB, 0.081_JPRB, 0.170_JPRB, 0.353_JPRB, 0.792_JPRB, &
  &            1.711_JPRB, 3.543_JPRB, 7.951_JPRB,17.122_JPRB /)
ZFRSS9   = (/   0.5_JPRB ,  0.5_JPRB ,  0.5_JPRB ,  0.5_JPRB ,  0.5_JPRB , &
  &             0.5_JPRB ,  0.5_JPRB ,  0.5_JPRB ,  0.5_JPRB  /)

!-  computed off-line by gems_ss_9bins.f (in vsed_ss.txt, 9th colum is 80%)

ZVSEDOCS9 = (/ 8.46E-07_JPRB,2.17E-06_JPRB,6.17E-06_JPRB,2.57E-05_JPRB,9.34E-05_JPRB, &
  &            4.08E-04_JPRB,2.19E-03_JPRB,8.41E-03_JPRB,3.00E-02_JPRB /)
ZVSEDSIS9 = (/ 8.46E-07_JPRB,2.17E-06_JPRB,6.17E-06_JPRB,2.57E-05_JPRB,9.34E-05_JPRB, &
  &            4.08E-04_JPRB,2.19E-03_JPRB,8.41E-03_JPRB,3.00E-02_JPRB /)
ZVSEDLDS9 = (/ 8.46E-07_JPRB,2.17E-06_JPRB,6.17E-06_JPRB,2.57E-05_JPRB,9.34E-05_JPRB, &
  &            4.08E-04_JPRB,2.19E-03_JPRB,8.41E-03_JPRB,3.00E-02_JPRB /)
ZVSEDLIS9 = (/ 8.46E-07_JPRB,2.17E-06_JPRB,6.17E-06_JPRB,2.57E-05_JPRB,9.34E-05_JPRB, &
  &            4.08E-04_JPRB,2.19E-03_JPRB,8.41E-03_JPRB,3.00E-02_JPRB /)


! adapted from LMDZ   (m s-1)

ZVDEPOCS9 = (/ 0.10E-02_JPRB, 0.10E-02_JPRB, 0.10E-02_JPRB, 0.20E-02_JPRB, 0.40E-02_JPRB, &
 &             0.70E-02_JPRB, 1.00E-02_JPRB, 1.21E-02_JPRB, 1.65E-02_JPRB /)
ZVDEPSIS9 = (/ 0.10E-02_JPRB, 0.10E-02_JPRB, 0.10E-02_JPRB, 0.20E-02_JPRB, 0.40E-02_JPRB, &
 &             0.70E-02_JPRB, 1.00E-02_JPRB, 1.21E-02_JPRB, 1.65E-02_JPRB /)
ZVDEPLDS9 = (/ 0.10E-02_JPRB, 0.10E-02_JPRB, 0.10E-02_JPRB, 0.20E-02_JPRB, 0.40E-02_JPRB, &
 &             0.70E-02_JPRB, 1.00E-02_JPRB, 1.21E-02_JPRB, 1.65E-02_JPRB /)
ZVDEPLIS9 = (/ 0.10E-02_JPRB, 0.10E-02_JPRB, 0.10E-02_JPRB, 0.20E-02_JPRB, 0.40E-02_JPRB, &
 &             0.70E-02_JPRB, 1.00E-02_JPRB, 1.21E-02_JPRB, 1.65E-02_JPRB /)

ISS=NTYPAER(1)

IF (ISS == 9) THEN
  RSSLIM(1:10) = ZSSLIM9(1:10)
  RSSFLX(1:9)  = ZSSFLX9(1:9)
  RMMD_SS(1:9) = ZMMD_SS9(1:9)
  RFRSS(1:9)   = ZFRSS9(1:9)

  RVDPOCE(1:9) = ZVDEPOCS9(1:9)
  RVDPSIC(1:9) = ZVDEPSIS9(1:9)
  RVDPLND(1:9) = ZVDEPLDS9(1:9)
  RVDPLIC(1:9) = ZVDEPLIS9(1:9)

  RVSEDOCE(1:9)= ZVSEDOCS9(1:9)
  RVSEDSIC(1:9)= ZVSEDSIS9(1:9)
  RVSEDLND(1:9)= ZVSEDLDS9(1:9)
  RVSEDLIC(1:9)= ZVSEDLIS9(1:9)
ELSE
  RSSLIM(1:4)  = ZSSLIM3(1:4)
  RSSFLX(1:3)  = ZSSFLX3(1:3)
  RMMD_SS(1:3) = ZMMD_SS3(1:3)
  RFRSS(1:3)   = ZFRSS3(1:3)

  RVDPOCE(1:3) = ZVDEPOCS3(1:3)
  RVDPSIC(1:3) = ZVDEPSIS3(1:3)
  RVDPLND(1:3) = ZVDEPLDS3(1:3)
  RVDPLIC(1:3) = ZVDEPLIS3(1:3)

  RVSEDOCE(1:3)= ZVSEDOCS3(1:3)
  RVSEDSIC(1:3)= ZVSEDSIS3(1:3)
  RVSEDLND(1:3)= ZVSEDLDS3(1:3)
  RVSEDLIC(1:3)= ZVSEDLIS3(1:3)
ENDIF







!*      2.2   DESERT DUST
!             ----------- 

!-- ECMWF 3 bins of desert dust

!- parameters related to DESERT DUST  (ECMWF 3 bins)
!  bins are 0.03 - 0.55 -  0.9  - 20 microns

ZMMD_DD3(1:3) = (/  0.32_JPRB,  0.75_JPRB,   9.0_JPRB /)
ZRHO_DD3(1:3) = (/ 2600._JPRB, 2600._JPRB, 2600._JPRB /)
ZFRDD3(1:3)   = (/   0.5_JPRB,   0.5_JPRB,   0.5_JPRB /)

!-  computed off-line by gems_dust.f  (in vsed_ss.txt, 9th colum is 80%)

ZVSEDOCD3 = (/ 0.6904E-04_JPRB, 0.1982E-03_JPRB, 0.1962E-02_JPRB /)
ZVSEDSID3 = (/ 0.6904E-04_JPRB, 0.1982E-03_JPRB, 0.1962E-02_JPRB /)
ZVSEDLDD3 = (/ 0.6904E-04_JPRB, 0.1982E-03_JPRB, 0.1962E-02_JPRB /)
ZVSEDLID3 = (/ 0.6904E-04_JPRB, 0.1982E-03_JPRB, 0.1962E-02_JPRB /)

!-- same values as sea-salt as in full and simplified LMD models
ZVDEPOCD3  = (/ 0.100E-02_JPRB , 0.110E-01_JPRB ,  0.145E-01_JPRB /)
ZVDEPSID3  = (/ 0.100E-02_JPRB , 0.110E-01_JPRB ,  0.145E-01_JPRB /)
ZVDEPLDD3  = (/ 0.100E-02_JPRB , 0.110E-01_JPRB ,  0.145E-01_JPRB /)
ZVDEPLID3  = (/ 0.100E-02_JPRB , 0.110E-01_JPRB ,  0.145E-01_JPRB /)



!-- ECMWF 9 bins of desert dust
!  bins are 0.03 - 0.05 - 0.10 - 0.20 - 0.50 - 1.00 - 2.00 - 5.00 - 10.00 - 20.00 microns

ZMMD_DD9 = (/  0.042_JPRB, 0.081_JPRB, 0.170_JPRB, 0.353_JPRB, 0.792_JPRB, &
  &            1.711_JPRB, 3.543_JPRB, 7.951_JPRB,17.122_JPRB /)
ZRHO_DD9 = (/  2600._JPRB, 2600._JPRB, 2600._JPRB, 2600._JPRB, 2600._JPRB, &
  &            2600._JPRB, 2600._JPRB, 2600._JPRB, 2600._JPRB /)
ZFRDD9   = (/    0.5_JPRB,   0.5_JPRB,   0.5_JPRB,   0.5_JPRB,   0.5_JPRB, &
  &              0.5_JPRB,   0.5_JPRB,   0.5_JPRB,   0.5_JPRB /)

!-  computed off-line by gems_dust_9bins.f  (sedimentation velocity m s-1)

ZVSEDOCD9 = (/ 1.859E-06_JPRB, 4.848E-06_JPRB, 1.393E-05_JPRB, 6.145E-05_JPRB, 2.144E-04_JPRB, &
  &            7.393E-04_JPRB, 3.013E-03_JPRB, 1.379E-02_JPRB, 5.055E-02_JPRB /)
ZVSEDSID9 = (/ 1.859E-06_JPRB, 4.848E-06_JPRB, 1.393E-05_JPRB, 6.145E-05_JPRB, 2.144E-04_JPRB, &
  &            7.393E-04_JPRB, 3.013E-03_JPRB, 1.379E-02_JPRB, 5.055E-02_JPRB /)
ZVSEDLDD9 = (/ 1.859E-06_JPRB, 4.848E-06_JPRB, 1.393E-05_JPRB, 6.145E-05_JPRB, 2.144E-04_JPRB, &
  &            7.393E-04_JPRB, 3.013E-03_JPRB, 1.379E-02_JPRB, 5.055E-02_JPRB /)
ZVSEDLID9 = (/ 1.859E-06_JPRB, 4.848E-06_JPRB, 1.393E-05_JPRB, 6.145E-05_JPRB, 2.144E-04_JPRB, &
  &            7.393E-04_JPRB, 3.013E-03_JPRB, 1.379E-02_JPRB, 5.055E-02_JPRB /)

! adapted from LMDZ   (m s-1)

ZVDEPOCD9 = (/ 0.10E-02_JPRB, 0.10E-02_JPRB, 0.10E-02_JPRB, 0.20E-02_JPRB, 0.40E-02_JPRB, &
 &             0.70E-02_JPRB, 1.00E-02_JPRB, 1.21E-02_JPRB, 1.65E-02_JPRB /)
ZVDEPSID9 = (/ 0.10E-02_JPRB, 0.10E-02_JPRB, 0.10E-02_JPRB, 0.20E-02_JPRB, 0.40E-02_JPRB, &
 &             0.70E-02_JPRB, 1.00E-02_JPRB, 1.21E-02_JPRB, 1.65E-02_JPRB /)
ZVDEPLDD9 = (/ 0.10E-02_JPRB, 0.10E-02_JPRB, 0.10E-02_JPRB, 0.20E-02_JPRB, 0.40E-02_JPRB, &
 &             0.70E-02_JPRB, 1.00E-02_JPRB, 1.21E-02_JPRB, 1.65E-02_JPRB /)
ZVDEPLID9 = (/ 0.10E-02_JPRB, 0.10E-02_JPRB, 0.10E-02_JPRB, 0.20E-02_JPRB, 0.40E-02_JPRB, &
 &             0.70E-02_JPRB, 1.00E-02_JPRB, 1.21E-02_JPRB, 1.65E-02_JPRB /)



IDU=NTYPAER(2)

IF (IDU == 9) THEN
  RMMD_DD(1:9) = ZMMD_DD9(1:9)
  RRHO_DD(1:9) = ZRHO_DD9(1:9)
  RFRDD(1:9)   = ZFRDD9(1:9)

  RVDPOCE(1:9) = ZVDEPOCD9(1:9)
  RVDPSIC(1:9) = ZVDEPSID9(1:9)
  RVDPLND(1:9) = ZVDEPLDD9(1:9)
  RVDPLIC(1:9) = ZVDEPLID9(1:9)

  RVSEDOCE(1:9)= ZVSEDOCD9(1:9)
  RVSEDSIC(1:9)= ZVSEDSID9(1:9)
  RVSEDLND(1:9)= ZVSEDLDD9(1:9)
  RVSEDLIC(1:9)= ZVSEDLID9(1:9)
ELSE
  RMMD_DD(1:3) = ZMMD_DD3(1:3)
  RRHO_DD(1:3) = ZRHO_DD3(1:3)
  RFRDD(1:3)   = ZFRDD3(1:3)

  RVDPOCE(4:6) = ZVDEPOCD3(1:3)
  RVDPSIC(4:6) = ZVDEPSID3(1:3)
  RVDPLND(4:6) = ZVDEPLDD3(1:3)
  RVDPLIC(4:6) = ZVDEPLID3(1:3)

  RVSEDOCE(4:6)= ZVSEDOCD3(1:3)
  RVSEDSIC(4:6)= ZVSEDSID3(1:3)
  RVSEDLND(4:6)= ZVSEDLDD3(1:3)
  RVSEDLIC(4:6)= ZVSEDLID3(1:3)
ENDIF


IF (NTYPAER(1) /= 9 .AND. NTYPAER(2) /= 9) THEN

!*      2.3   OTHER AEROSOLS (to be improved later!)
!             --------------
!- parameters related to other aerosol types

!- particulate organic matter POM (-philic, -phobic)
!-- efficiency for scavenging
  RFROM = (/  0.8_JPRB,  0.5_JPRB /)

!-- dry deposition
  RVDPOCE(7:8) = (/   0.28E-02_JPRB,   0.28E-02_JPRB /)
  RVDPSIC(7:8) = (/   0.17E-02_JPRB,   0.17E-02_JPRB /)
  RVDPLND(7:8) = (/   0.14E-02_JPRB,   0.14E-02_JPRB /)
  RVDPLIC(7:8) = (/   0.17E-02_JPRB,   0.17E-02_JPRB /)

! adapted from LMDZ   (m s-1)     NB: no explicit sedimentation accounted for

  RVSEDOCE(7:8) = (/   0.10E-02_JPRB,   0.10E-02_JPRB /)
  RVSEDSIC(7:8) = (/   0.10E-02_JPRB,   0.10E-02_JPRB /)
  RVSEDLND(7:8) = (/   0.10E-02_JPRB,   0.10E-02_JPRB /)
  RVSEDLIC(7:8) = (/   0.10E-02_JPRB,   0.10E-02_JPRB /)


!- black carbon
!-- efficiency for scavenging BC (-philic, -phobic)
  RFRBC = (/  0.8_JPRB,  0.5_JPRB /)

!-- dry deposition
  RVDPOCE(9:10) = (/   0.28E-02_JPRB,   0.28E-02_JPRB /)
  RVDPSIC(9:10) = (/   0.17E-02_JPRB,   0.17E-02_JPRB /)
  RVDPLND(9:10) = (/   0.14E-02_JPRB,   0.14E-02_JPRB /)
  RVDPLIC(9:10) = (/   0.17E-02_JPRB,   0.17E-02_JPRB /)

! adapted from LMDZ   (m s-1)     NB: no explicit sedimentation accounted for

  RVSEDOCE(9:10) = (/   0.10E-02_JPRB,   0.10E-02_JPRB /)
  RVSEDSIC(9:10) = (/   0.10E-02_JPRB,   0.10E-02_JPRB /)
  RVSEDLND(9:10) = (/   0.10E-02_JPRB,   0.10E-02_JPRB /)
  RVSEDLIC(9:10) = (/   0.10E-02_JPRB,   0.10E-02_JPRB /)


!- sulfate (SO4 / SO2)
!-- efficiency for scavenging (no scavenging/wet deposition for SO2)
!  RFRSO4 = (/ 0.5_JPRB, 0.8_JPRB /)
  RFRSO4 = (/ 0.5_JPRB, 0.0_JPRB /)

!-- dry deposition
  RVDPOCE(11:12) = (/ 0.05E-02_JPRB, 0.70E-02_JPRB /)
  RVDPSIC(11:12) = (/ 0.25E-02_JPRB, 0.20E-02_JPRB /)
  RVDPLND(11:12) = (/ 0.25E-02_JPRB, 0.30E-02_JPRB /)
  RVDPLIC(11:12) = (/ 0.25E-02_JPRB, 0.20E-02_JPRB /)

! adapted from LMDZ   (m s-1)     NB: no explicit sedimentation accounted for SO2

!  RVSEDOCE(11:12) = (/ 0.05E-02_JPRB, 0.05E-02_JPRB /)
!  RVSEDSIC(11:12) = (/ 0.25E-02_JPRB, 0.25E-02_JPRB /)
!  RVSEDLND(11:12) = (/ 0.25E-02_JPRB, 0.25E-02_JPRB /)
!  RVSEDLIC(11:12) = (/ 0.25E-02_JPRB, 0.25E-02_JPRB /)
  RVSEDOCE(11:12) = (/ 0.05E-02_JPRB, 0.00E+00_JPRB /)
  RVSEDSIC(11:12) = (/ 0.25E-02_JPRB, 0.00E+00_JPRB /)
  RVSEDLND(11:12) = (/ 0.25E-02_JPRB, 0.00E+00_JPRB /)
  RVSEDLIC(11:12) = (/ 0.25E-02_JPRB, 0.00E+00_JPRB /)


!- fly ash
  RFRIF  = 0.8_JPRB

  RVDPOCE(13) = 0.20E-02_JPRB
  RVDPSIC(13) = 0.20E-02_JPRB
  RVDPLND(13) = 0.20E-02_JPRB
  RVDPLIC(13) = 0.20E-02_JPRB

! adapted from LMDZ   (m s-1)     NB: no explicit sedimentation accounted for

  RVSEDOCE(13) = 0.20E-02_JPRB
  RVSEDSIC(13) = 0.20E-02_JPRB
  RVSEDLND(13) = 0.20E-02_JPRB
  RVSEDLIC(13) = 0.20E-02_JPRB


ENDIF




!- NB: 15 values for all possible types of ECMWF aerosols (all the same except 12=SO2)
RALPHAR = (/ &
 & 0.001_JPRB, 0.001_JPRB, 0.001_JPRB, 0.001_JPRB, 0.001_JPRB, 0.001_JPRB, 0.001_JPRB, 0.001_JPRB, &
 & 0.001_JPRB, 0.001_JPRB, 0.001_JPRB, 0.000_JPRB, 0.001_JPRB, 0.001_JPRB, 0.001_JPRB /)
RALPHAS = (/ &
 &  0.01_JPRB,  0.01_JPRB,  0.01_JPRB,  0.01_JPRB,  0.01_JPRB,  0.01_JPRB,  0.01_JPRB,  0.01_JPRB, &
 &  0.01_JPRB,  0.01_JPRB,  0.01_JPRB,  0.00_JPRB,  0.01_JPRB,  0.01_JPRB,  0.01_JPRB /)


!*      3.    PARAMETERS RELATED TO TRANSPORT WITHIN THE FREE ATMOSPHERE
!             ----------------------------------------------------------

NDD1=NTYPAER(1)+1
NSS1=1

RGRATE = 7.1E-06_JPRB

RMFMIN = 1.E-10_JPRB

RMASSE = (/ &
  &  6.02E+23_JPRB, 6.02E+23_JPRB, 6.02E+23_JPRB, 6.02E+23_JPRB, 6.02E+23_JPRB, 6.02E+23_JPRB &
  &, 6.02E+23_JPRB, 6.02E+23_JPRB, 6.02E+23_JPRB, 6.02E+23_JPRB, 6.02E+23_JPRB, 6.02E+23_JPRB &
  &, 6.02E+23_JPRB, 6.02E+23_JPRB, 6.02E+23_JPRB /)

!     ----------------------------------------------------------------
END SUBROUTINE SU_AERP

