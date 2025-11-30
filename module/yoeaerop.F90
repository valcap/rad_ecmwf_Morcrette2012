MODULE YOEAEROP

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEAEROP* - OPTICAL PROPERTIES FOR PROGNOSTIC AEROSOLS
!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ALF_BC(19)     , ASY_BC(19)     , OMG_BC(19)     , RALI_BC(19)
REAL(KIND=JPRB) :: ALF_DD(9,19)   , ASY_DD(9,19)   , OMG_DD(9,19)   , RALI_DD(9,19)
REAL(KIND=JPRB) :: ALF_FA(19)     , ASY_FA(19)     , OMG_FA(19)     , RALI_FA(19) 
REAL(KIND=JPRB) :: ALF_OM(12,19)  , ASY_OM(12,19)  , OMG_OM(12,19)  , RALI_OM(12,19)
REAL(KIND=JPRB) :: ALF_SS(12,19,9), ASY_SS(12,19,9), OMG_SS(12,19,9), RALI_SS(12,19,9)
REAL(KIND=JPRB) :: ALF_SU(12,19)  , ASY_SU(12,19)  , OMG_SU(12,19)  , RALI_SU(12,19) 

!     ------------------------------------------------------------------
! 9 refers to up to 9 bins (3 operationally for SS and DD) 
! 19 to 19 SW channel radiances
! 12 to 12 reference RH

! BC is for black carbon
! DD is for desert dust
! FA is fort flying ash
! OM is for organic matter
! SS is for sea-salt
! SU is for sulfate

! ALF is alpha , the mass extinction coefficient   in m2/g
! ASY is g     , the assymetry factor             ND
! OMG is pizero, the single scattering albedo     ND
! RALI is the lidar ratio  ND
!     ------------------------------------------------------------------
END MODULE YOEAEROP

