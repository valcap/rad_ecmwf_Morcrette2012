MODULE YOERADGHG

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOERADGHG* - CONTROLS CLIMATOLOGICAL GHGs FOR RADIATION 
!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: RSINCL(64)   , RCLPR(0:91)
REAL(KIND=JPRB) :: RCLCH4(64,0:91), RCLCO2(64,0:91), RCLN2O(64,0:91)
REAL(KIND=JPRB) :: RCLNO2(64,0:91), RCLC11(64,0:91), RCLC12(64,0:91)
REAL(KIND=JPRB) :: RCLCCL4(64,0:91),RCLC22(64,0:91), RCLOZO(64,0:91)
!     ------------------------------------------------------------------
! RSINCL   SINE OF LATITUDES WHERE MONTHLY MEAN CLIMATOLOGIES ARE GIVEN
! RCLxxx   ZONAL MEAN CLIMATOLOGY FOR GAS xxx ALREADY INTERPOLATED IN TIME 
!     ------------------------------------------------------------------
END MODULE YOERADGHG


