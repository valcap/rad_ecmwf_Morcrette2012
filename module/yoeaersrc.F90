MODULE YOEAERSRC

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEAERSRC* - CONTROL OPTIONS FOR AEROSOLS' SOURCE
!     ------------------------------------------------------------------
INTEGER(KIND=JPIM) :: NMAXTAER
INTEGER(KIND=JPIM) :: NTAER
INTEGER(KIND=JPIM) :: NBINAER(8)
INTEGER(KIND=JPIM) :: NINDAER(15), JKTYP(15), JKBIN(15)
INTEGER(KIND=JPIM) :: NTYPAER(8)
INTEGER(KIND=JPIM) :: NDDUST, NSSALT, NAERWND, NAERDUT

LOGICAL :: LEPAERO, LAEREXTR

REAL(KIND=JPRB) :: RGELAV, RGEMUV, RDGLAV, RDGMUV
REAL(KIND=JPRB) :: RCLONV, RSLONV, RDCLONV, RDSLONV
REAL(KIND=JPRB) :: RLATVOL,RLONVOL
REAL(KIND=JPRB) :: RBCPHIL, RBCPHOB, ROMPHIL, ROMPHOB
REAL(KIND=JPRB) :: RFCTDU, RFCTSS, RFCTDUR, RFCTSSR
REAL(KIND=JPRB) :: RAERDUB,RSO2SO4

REAL(KIND=JPRB) :: RSSFLX(9), RSSLIM(10)

!     ------------------------------------------------------------------
! MMAXTAER   : MAXIMUM TOTAL NUMBER OF AEROSOLS
! NTAER      : TOTAL NUMBER OF AEROSOLS
! NTYPAER( ) : NBINAER( )
!        (1) :         3/9 FOR SEA-SALT 
!        (2) :         3/9 FOR DESERT DUST
!        (3) :         2 FOR ORGANIC MATTERS
!        (4) :         2 FOR BLACK CARBON
!        (5) :         2 FOR SO2/SO4 (TROPOSPHERIC AND STRATOSPHERIC SULPHATE)
!        (6) :         1 FOR FLY ASH
!        (7) :         1 FOR PSEUDO-PROGNOSTIC STRATOSPHERIC AEROSOLS
!        (8) :         1 FOR VOLCANIC AEROSOLS

! RSSFLX     : sea salt flux for (9)3-size bins (in mg m-2 s-1) for a 1 m s-1
!              wind speed, at 10 m height and 80% RH 
! RSSLIM     : limits of bins (um)
! Rlat/lonVOL: LAT/LON of a possible volcanic eruption
! RxxPHIL, PHOB: fractions of BC and OM hydrophilic or hydrophobic
! NDDUST, NSSALT : various formulations for dust and sea-salt emissions (test only)
! RAERDUB    : base value for dust emission
! NAERWND    : 0 = 10m-wind for both DU and SS
!            : 1 = gust for SS, 10m-wind for DU
!            : 2 = gust for DU, 10m-wind for SS
!            : 3 = gust for both DU and SS
! RFCTSS, -DU: factors depending on choice of NAERWND
! RSO2SO4    : efficiency factor in (simplified) transfer SO2 to SO4
!     ------------------------------------------------------------------
END MODULE YOEAERSRC

