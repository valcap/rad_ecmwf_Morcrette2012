SUBROUTINE SRTM_INIT

!-- read in the basic coefficients to configure RRTM_SW
!- creates module YOESRTWN with BG, NSPA, NSPB, WAVENUM1, WAVENUM2,
!  DELWAVE, PREF, PREFLOG, TREF

USE PARKIND1  ,ONLY : JPIM , JPRB

USE PARSRTM  , ONLY : JPG, JPSW
USE YOESRTWN , ONLY : NG, NGM, WT, NGC, NGN, RWGT, WTSM

IMPLICIT NONE

! Local variables
INTEGER(KIND=JPIM) :: IGC, IGCSM, IBND, IG, IND, IPR, IPRSM
REAL(KIND=JPRB)    :: ZWTSUM





CALL SUSRTM

!-- read in the molecular absorption coefficients

CALL SRTM_KGB16
CALL SRTM_KGB17
CALL SRTM_KGB18
CALL SRTM_KGB19
CALL SRTM_KGB20
CALL SRTM_KGB21
CALL SRTM_KGB22
CALL SRTM_KGB23
CALL SRTM_KGB24
CALL SRTM_KGB25
CALL SRTM_KGB26
CALL SRTM_KGB27
CALL SRTM_KGB28
CALL SRTM_KGB29

!-- read in the cloud optical properties
!- creates module YOESRTOP with EXTLIQ1, SSALIQ1, ASYLIQ1, 
!  EXTICE3, SSAICE3, ASYICE3, FDLICE3  

!-- RRTM_SW cloud optical properties are not used
!   SRTM_CLDPROP is not called
!   no need to call SUSRTOP

!CALL SUSRTOP ( -1 )


!Mike Iacono 20050804
!-- Perform g-point reduction from 16 per band (224 total points) to
!-- a band dependent number (112 total points) for all absorption
!-- coefficient input data and Planck fraction input data.
!-- Compute relative weighting for new g-point combinations.

IGCSM = 0
DO IBND = 1,JPSW
  IPRSM = 0
  IF (NGC(IBND) < JPG) THEN
    DO IGC = 1,NGC(IBND)
      IGCSM = IGCSM + 1
      ZWTSUM = 0.
      DO IPR = 1, NGN(IGCSM)
        IPRSM = IPRSM + 1
        ZWTSUM = ZWTSUM + WT(IPRSM)
      ENDDO
      WTSM(IGC) = ZWTSUM
    ENDDO

    DO IG = 1,NG(IBND+15)
      IND = (IBND-1)*JPG + IG
      RWGT(IND) = WT(IG)/WTSM(NGM(IND))
    ENDDO
  ELSE
    DO IG = 1,NG(IBND+15)
      IGCSM = IGCSM + 1
      IND = (IBND-1)*JPG + IG
      RWGT(IND) = 1.0
    ENDDO
  ENDIF
ENDDO

CALL SRTM_CMBGB16
CALL SRTM_CMBGB17
CALL SRTM_CMBGB18
CALL SRTM_CMBGB19
CALL SRTM_CMBGB20
CALL SRTM_CMBGB21
CALL SRTM_CMBGB22
CALL SRTM_CMBGB23
CALL SRTM_CMBGB24
CALL SRTM_CMBGB25
CALL SRTM_CMBGB26
CALL SRTM_CMBGB27
CALL SRTM_CMBGB28
CALL SRTM_CMBGB29

!-----------------------------------------------------------------------
END SUBROUTINE SRTM_INIT

