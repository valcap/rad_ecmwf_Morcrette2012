MODULE YOS_SW

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!       ----------------------------------------------------------------
!*    ** *YOS_SW* - COEFFICIENTS FOR SHORTWAVE RADIATION TRANSFER
!       ----------------------------------------------------------------

!REAL(KIND=JPRB), ALLOCATABLE :: RSUN(:)      ! SOLAR FRACTION IN SPECTRAL INTERVALS
REAL(KIND=JPRB) :: RSUN(14)
!REAL(KIND=JPRB), ALLOCATABLE :: RALBICE_AR(:,:) ! monthly sea-ice albedo Arctic
!REAL(KIND=JPRB), ALLOCATABLE :: RALBICE_AN(:,:) ! Antarctic
REAL(KIND=JPRB) :: RALBICE_AR(12,14)
REAL(KIND=JPRB) :: RALBICE_AN(12,14)
                                       ! in SW spectral intervals 
!REAL(KIND=JPRB), ALLOCATABLE :: RWEIGHT(:,:) ! S.W. SPECTR WEIGHT
REAL(KIND=JPRB) :: RWEIGHT(14,8) ! S.W. SPECTR WEIGHT
!                     1 - ocean (flat response)
!                     2 - sea-ice (as snow or ice depending on month)
!                     3 - wet skin (flat)
!                     4 - vegetation low and snow-free (BR, 1986)
!                     5 - snow on low vegetation       (Warren, 1982)
!                     6 - vegetation high and snow-free (BR, 1986)
!                     7 - snow under high vegetation   (Warren, 1982)
!                     8 - bare soil (Briegleb, Ramanathan, 1986)
!     -----------------------------------------------------------------

END MODULE YOS_SW
