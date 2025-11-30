MODULE YOELWRAD

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

! ------ CLOUD CHARACTERISTICS FOR SIMPLIFIED SCHEME

! RADLWM   : BUFFER FOR DOWNWARD LONGWAVE EMISSIVITY 
!            perturbation in TL/AD
! RADLWM5  : BUFFER FOR DOWNWARD LONGWAVE EMISSIVITY  
!            trajectory in TL/AD
! NLEVLWC  : level up to which cloud effects on LW fluxes are considered
! NLWFR    : frequency of calling longwave radiation for TL/AD
! NLOOPLW  : required number of loops inside of nproma for TL/AD lw radiation 
! NLOOPLWO : optimized number of loops inside of nproma
! NPROMALW : optimized length of loops inside of nproma
! NLASTLW  : length of the last loop inside of nproma
! LOPTLWPR : .true. if shorter loops inside of nproma are required 
! LWLCLHR  : .true. if cloud effects on LW are only computed to the level
!                      defined using NLEVLWC and the current cloud top height 
!            .false. if computed for all levels          

!-- not dimensioned in 1D-rad
!REAL(KIND=JPRB),ALLOCATABLE:: RADLWM (:,:,:)
!REAL(KIND=JPRB),ALLOCATABLE:: RADLWM5(:,:,:)

INTEGER(KIND=JPIM) :: NLEVLWC
INTEGER(KIND=JPIM) :: NLWFR
INTEGER(KIND=JPIM) :: NLOOPLW
INTEGER(KIND=JPIM) :: NLOOPLWO
INTEGER(KIND=JPIM) :: NPROMALW
INTEGER(KIND=JPIM) :: NLASTLW
 
LOGICAL :: LOPTLWPR
LOGICAL :: LWLCLHR

!     --------------------------------------------------------------------

END MODULE YOELWRAD
