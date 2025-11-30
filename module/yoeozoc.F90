MODULE YOEOZOC

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*     *YOEOZOC* SPECTRAL DISTRIBUTION OF OZONE
!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: COZQC(21)
REAL(KIND=JPRB) :: COZQS(15)
REAL(KIND=JPRB) :: COZHC(21)
REAL(KIND=JPRB) :: COZHS(15)

REAL(KIND=JPRB) :: RSINC(64)
REAL(KIND=JPRB) :: ROZT(64,0:60)
REAL(KIND=JPRB) :: RPROC(0:60)


!*     *YOEOZOC* SPECTRAL DISTRIBUTION OF OZONE
!                     (TRIANGULAR *T5* TRUNCATION FOR OZONE).

!     R.G AND M.J        E.C.M.W.F.     29/11/82.

!      NAME     TYPE      PURPOSE
!      ----     ----      -------

!     *COZ__*   REAL      *REFERS TO *OZONE.
!     *C___C*   REAL      *REFERS TO *COS COMPONENT.
!     *C___S*   REAL      *REFERS TO *SIN COMPONENT.
!       ----------------------------------------------------------------
END MODULE YOEOZOC
