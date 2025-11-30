SUBROUTINE SRTM_TAUMOL26 &
 & ( KIDIA   , KFDIA    , KLEV,&
 & P_FAC00   , P_FAC01  , P_FAC10   , P_FAC11,&
 & K_JP      , K_JT     , K_JT1     , P_ONEMINUS,&
 & P_COLH2O  , P_COLCO2 , P_COLMOL,&
 & K_LAYTROP , P_SELFFAC, P_SELFFRAC, K_INDSELF  , P_FORFAC, P_FORFRAC, K_INDFOR,&
 & P_SFLUXZEN, P_TAUG   , P_TAUR    , PRMU0   &
 & )  

!     Written by Eli J. Mlawer, Atmospheric & Environmental Research.

!     BAND 26:  22650-29000 cm-1 (low - nothing; high - nothing)

!      PARAMETER (MG=16, MXLAY=203, NBANDS=14)

! Modifications
!        M.Hamrud      01-Oct-2003 CY28 Cleaning

!     JJMorcrette 2003-02-24 adapted to ECMWF environment
!        D.Salmond  31-Oct-2007 Vector version in the style of RRTM from Meteo France & NEC

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE PARSRTM  , ONLY : JPLAY, JPG, NG26
USE YOESRTA26, ONLY : SFLUXREFC, RAYLC
IMPLICIT NONE

!-- Output
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA, KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
REAL(KIND=JPRB)                  :: P_FAC00(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: P_FAC01(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: P_FAC10(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: P_FAC11(KIDIA:KFDIA,JPLAY) ! Argument NOT used
INTEGER(KIND=JPIM)               :: K_JP(KIDIA:KFDIA,JPLAY) ! Argument NOT used
INTEGER(KIND=JPIM)               :: K_JT(KIDIA:KFDIA,JPLAY) ! Argument NOT used
INTEGER(KIND=JPIM)               :: K_JT1(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: P_ONEMINUS(KIDIA:KFDIA) ! Argument NOT used
REAL(KIND=JPRB)                  :: P_COLH2O(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: P_COLCO2(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_COLMOL(KIDIA:KFDIA,JPLAY) 
INTEGER(KIND=JPIM),INTENT(IN)    :: K_LAYTROP(KIDIA:KFDIA) 
REAL(KIND=JPRB)                  :: P_SELFFAC(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: P_SELFFRAC(KIDIA:KFDIA,JPLAY) ! Argument NOT used
INTEGER(KIND=JPIM)               :: K_INDSELF(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: P_FORFAC(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: P_FORFRAC(KIDIA:KFDIA,JPLAY) ! Argument NOT used
INTEGER(KIND=JPIM)               :: K_INDFOR(KIDIA:KFDIA,JPLAY) ! Argument NOT used

REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_SFLUXZEN(KIDIA:KFDIA,JPG) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_TAUG(KIDIA:KFDIA,JPLAY,JPG) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_TAUR(KIDIA:KFDIA,JPLAY,JPG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRMU0(KIDIA:KFDIA)
!- from AER
!- from INTFAC      
!- from INTIND
!- from PRECISE             
!- from PROFDATA             
!- from SELF             
INTEGER(KIND=JPIM) :: IG, I_LAY, I_LAYSOLFR(KIDIA:KFDIA), I_NLAYERS, IPLON



I_NLAYERS = KLEV

!     Compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  Below LAYTROP, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

I_LAYSOLFR(KIDIA:KFDIA) = K_LAYTROP(KIDIA:KFDIA)

DO I_LAY = 1, I_NLAYERS
  DO IPLON = KIDIA, KFDIA
    IF (PRMU0(IPLON) > 0.0_JPRB) THEN
      IF (I_LAY <= K_LAYTROP(IPLON)) THEN
        !  DO IG = 1, NG(26)
!CDIR UNROLL=NG26
        DO IG = 1 , NG26 
          !    TAUG(LAY,IG) = COLMOL(LAY) * RAYLC(IG)
          !    SSA(LAY,IG) = 1.0
          IF (I_LAY == I_LAYSOLFR(IPLON)) P_SFLUXZEN(IPLON,IG) = SFLUXREFC(IG) 
          P_TAUG(IPLON,I_LAY,IG) = 0.0_JPRB
          P_TAUR(IPLON,I_LAY,IG) = P_COLMOL(IPLON,I_LAY) * RAYLC(IG) 
        ENDDO
      ENDIF
    ENDIF
  ENDDO
ENDDO

DO I_LAY = 1, I_NLAYERS
  DO IPLON = KIDIA, KFDIA
    IF (PRMU0(IPLON) > 0.0_JPRB) THEN
      IF (I_LAY >= K_LAYTROP(IPLON)+1) THEN
        !  DO IG = 1, NG(26)
!CDIR UNROLL=NG26
        DO IG = 1 , NG26
          !    TAUG(LAY,IG) = COLMOL(LAY) * RAYLC(IG)
          !    SSA(LAY,IG) = 1.0
          P_TAUG(IPLON,I_LAY,IG) = 0.0_JPRB
          P_TAUR(IPLON,I_LAY,IG) = P_COLMOL(IPLON,I_LAY) * RAYLC(IG) 
        ENDDO
      ENDIF
    ENDIF
  ENDDO
ENDDO

!-----------------------------------------------------------------------
END SUBROUTINE SRTM_TAUMOL26

