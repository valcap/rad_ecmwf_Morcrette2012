MODULE PARRTM1D

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!     Parameters for 1-D radiation only computations from operational
!      library routines
!     991007  JJMorcrette
!     ------------------------------------------------------------------

INTEGER(KIND=JPIM), PARAMETER :: JP_LON  = 5
INTEGER(KIND=JPIM), PARAMETER :: JP_IDIA = 1
INTEGER(KIND=JPIM), PARAMETER :: JP_FDIA = JP_LON-JP_IDIA+1
INTEGER(KIND=JPIM), PARAMETER :: JP_TDIA = 1
INTEGER(KIND=JPIM), PARAMETER :: JP_LEV  = 150

INTEGER(KIND=JPIM), PARAMETER :: JP_LW   = 6
INTEGER(KIND=JPIM), PARAMETER :: JP_SW   = 14
INTEGER(KIND=JPIM), PARAMETER :: JP_NUA  = 24
INTEGER(KIND=JPIM), PARAMETER :: JP_MODE = 1
INTEGER(KIND=JPIM), PARAMETER :: JP_AER  = 6
INTEGER(KIND=JPIM), PARAMETER :: JP_LEVP1= JP_LEV+1

!     ------------------------------------------------------------------
END MODULE PARRTM1D
