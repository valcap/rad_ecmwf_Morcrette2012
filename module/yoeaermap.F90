MODULE YOEAERMAP

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEAERMAP* - GEOGRAPHICALLY DEPENDENT PARAMETERS
!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: NAERMAP

REAL(KIND=JPRB)    :: RDDUAER(0:35)

!- only for 1D-Rad
!!!! REAL(KIND=JPRB),ALLOCATABLE :: RAERMAP(:,:,:)
!     ------------------------------------------------------------------
END MODULE YOEAERMAP



