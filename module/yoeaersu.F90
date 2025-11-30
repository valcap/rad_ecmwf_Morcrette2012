MODULE YOEAERSU

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEAERSU* - OPTICAL PROPERTIES FOR O.BOUCHER SO4 AEROSOLS
!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ALF_SU(12)  , ASY_SU(12)  , OMG_SU(12), RRHTAB(12)

!     ------------------------------------------------------------------
! 12 refers to 12 reference RH
! SU is for sulfate
! ALF is alpha , the mass extinction coefficient   in m2/g
! ASY is g     , the assymetry factor             ND
! OMG is pizero, the single scattering albedo     ND
!     ------------------------------------------------------------------
END MODULE YOEAERSU

