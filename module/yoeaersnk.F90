MODULE YOEAERSNK

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEAERSNK* - CONTROL OPTIONS FOR AEROSOLS' SINKS
!     ------------------------------------------------------------------
REAL(KIND=JPRB) :: R_R, R_S
REAL(KIND=JPRB) :: RAERTS(15)
REAL(KIND=JPRB) :: RALPHAR(15), RALPHAS(15)
REAL(KIND=JPRB) :: RFRBC(2)
REAL(KIND=JPRB) :: RFRIF 
REAL(KIND=JPRB) :: RFRDD(9), RFRSS(9) 
REAL(KIND=JPRB) :: RFROM(2) 
REAL(KIND=JPRB) :: RFRSO4(2)
REAL(KIND=JPRB) :: RFRAER, RFRGAS 

INTEGER(KIND=JPIM) :: NBRH
REAL(KIND=JPRB) :: RRHMAX, RRHTAB(12), RRHO_SS(12), RSSGROW(12)
REAL(KIND=JPRB) :: RMMD_SS(9)
REAL(KIND=JPRB) :: RMMD_DD(9), RRHO_DD(9)
REAL(KIND=JPRB) :: RHO_WAT, RHO_ICE

REAL(KIND=JPRB) :: RVDPOCE(15), RVDPSIC(15), RVDPLND(15), RVDPLIC(15)
REAL(KIND=JPRB) :: RVSEDOCE(15),RVSEDSIC(15),RVSEDLND(15),RVSEDLIC(15)
!     ------------------------------------------------------------------
! R_R        : mean radius for rain drops (m)
! R_S        : mean radius for snow crystals (m)
! RAERTS     : inverse time scale (s-1)
! RALPHAR    : impaction coefficients for rain
! RALPHAS    : IMPACTION coefficients for snow
! dissolution constants
! RFRBCn     : for black carbon (BC1: hydrophobic, BC2: hydrophilic)
! RFRDDn     : for dust     (3 bins: 0.03 - 0.55 - 0.9 - 20 um)
!                           (9 bins: 0.03 - 0.05 - 0.10 - 0.20 - 0.50 - 1.00 - 2.00 - 5.00 - 10.00 - 20.00 um)
! RFRIF      : for inorganic fraction (i.e., fly-ash)
! RFROMn     : for organic matter (OM1: hydrophobic, OM2: hydrophilic)
! RFRSSn     : for sea-salt (3 bins: 0.03 - 0.5 - 5 - 20 um)
!                           (9 bins: 0.03 - 0.05 - 0.10 - 0.20 - 0.50 - 1.00 - 2.00 - 5.00 - 10.00 - 20.00 um)

! re-evaporation constants
! RFRAER     : for aerosols
! RFRGAS     : for gases

! RRHMAX     : 
! RRHTAB     : 

! dry deposition velocity
! RVDPOCE    :
! RVDPSIC    :
! RVDPLND    :
! RVDPLIC    :

! sedimentation (gravitational settling) velocity
! RVSEDOCE   : for ocean
! RVSEDSIC   :
! RVSEDLND   :
! RVSEDLIC   :
!     -----------------------------------------------------------------
END MODULE YOEAERSNK

