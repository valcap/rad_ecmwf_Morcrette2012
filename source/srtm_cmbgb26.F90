SUBROUTINE SRTM_CMBGB26

!     BAND 26:  22650-29000 cm-1 (low - nothing; high - nothing)
!-----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM , JPRB

USE YOESRTWN , ONLY : NGC, NGS, NGN, RWGT
USE YOESRTA26, ONLY : SFLUXREF, RAYL, &
                    & SFLUXREFC, RAYLC

IMPLICIT NONE

! Local variables
INTEGER(KIND=JPIM) :: IGC, IPR, IPRSM
REAL(KIND=JPRB)    :: ZSUMF1, ZSUMF2

!     ------------------------------------------------------------------

IPRSM = 0
DO IGC = 1,NGC(11)
  ZSUMF1 = 0.
  ZSUMF2 = 0.
  DO IPR = 1, NGN(NGS(10)+IGC)
    IPRSM = IPRSM + 1
    ZSUMF1 = ZSUMF1 + RAYL(IPRSM)*RWGT(IPRSM+160)
    ZSUMF2 = ZSUMF2 + SFLUXREF(IPRSM)
  ENDDO
  RAYLC(IGC) = ZSUMF1
  SFLUXREFC(IGC) = ZSUMF2
ENDDO

!     -----------------------------------------------------------------
END SUBROUTINE SRTM_CMBGB26

