!!!#ifdef RS6K
!!!@PROCESS HOT NOSTRICT
!!!#endif
SUBROUTINE SRTM_REFTRA &
 & ( KIDIA , KFDIA, KLEV  , KMODTS, &
 &   LDRTCHK, &
 &   PGG   , PRMUZ, PTAU , PW, &
 &   PREF  , PREFD, PTRA , PTRAD &
 & )

!**** *SRTM_REFTRA* - REFLECTIVITY AND TRANSMISSIVITY

!     PURPOSE.
!     --------
!           COMPUTES THE REFLECTIVITY AND TRANSMISSIVITY OF A CLEAR OR
!     CLOUDY LAYER USING A CHOICE OF VARIOUS APPROXIMATIONS.

!**   INTERFACE.
!     ----------
!          *SRTM_REFTRA* IS CALLED BY *SRTM_SPCVRT*

!        EXPLICIT ARGUMENTS :
!        --------------------
! INPUTS
! ------
!      KMODTS  = 1 EDDINGTON (JOSEPH ET AL., 1976)
!              = 2 PIFM (ZDUNKOWSKI ET AL., 1980)
!              = 3 DISCRETE ORDINATES (LIOU, 1973)
!      LDRTCHK = .T. IF CLOUDY
!              = .F. IF CLEAR-SKY
!      PGG     = ASSYMETRY FACTOR
!      PRMUZ   = COSINE SOLAR ZENITH ANGLE
!      PTAU    = OPTICAL THICKNESS
!      PW      = SINGLE SCATTERING ALBEDO

! OUTPUTS
! -------
!      PREF    : COLLIMATED BEAM REFLECTIVITY
!      PREFD   : DIFFUSE BEAM REFLECTIVITY
!      PTRA    : COLLIMATED BEAM TRANSMISSIVITY
!      PTRAD   : DIFFUSE BEAM TRANSMISSIVITY

!     METHOD.
!     -------
!          STANDARD DELTA-EDDINGTON, P.I.F.M., OR D.O.M. LAYER CALCULATIONS.

!     EXTERNALS.
!     ----------
!          NONE

!     REFERENCE.
!     ----------

!     AUTHOR.
!     -------
!        JEAN-JACQUES MORCRETTE  *ECMWF*

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 03-02-27
!        M.Hamrud   01-Oct-2003      CY28 Cleaning
!        Mike Iacono, AER, Mar 2004: bug fix
!        D.Salmond  31-Oct-2007 Vector version in the style of RRTM from Meteo France & NEC

!     ------------------------------------------------------------------

!*       0.1   ARGUMENTS
!              ---------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE PARSRTM , ONLY : JPLAY
USE YOERDU  , ONLY : REPLOG

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA, KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV
INTEGER(KIND=JPIM),INTENT(OUT)   :: KMODTS
LOGICAL           ,INTENT(IN)    :: LDRTCHK(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGG(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRMUZ(KIDIA:KFDIA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTAU(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PW(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB)   ,INTENT(INOUT)   :: PREF(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB)   ,INTENT(INOUT)   :: PREFD(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB)   ,INTENT(INOUT)   :: PTRA(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB)   ,INTENT(INOUT)   :: PTRAD(KIDIA:KFDIA,JPLAY)
!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JK, JL, IC, INDEX(1:2000), ICOUNT

REAL(KIND=JPRB) :: ZA, ZA1, ZA2
REAL(KIND=JPRB) :: ZBETA, ZDEND, ZDENR, ZDENT
REAL(KIND=JPRB) :: ZE1, ZE2, ZEM1, ZEM2, ZEMM, ZEP1, ZEP2
REAL(KIND=JPRB) :: ZG, ZG3, ZGAMMA1, ZGAMMA2, ZGAMMA3, ZGAMMA4, ZGT
REAL(KIND=JPRB) :: ZR1, ZR2, ZR3, ZR4, ZR5, ZRK, ZRK2, ZRKG, ZRM1, ZRP, ZRP1, ZRPP
REAL(KIND=JPRB) :: ZSR3, ZT1, ZT2, ZT3, ZT4, ZT5, ZTO1
REAL(KIND=JPRB) :: ZW, ZWCRIT, ZWO
REAL(KIND=JPRB) :: ZEXP500, ZEXP500R, ZEXPM500, ZSQRT_REPLOG, ZTEMP, ZZZ

!     ------------------------------------------------------------------


ZEXP500=EXP(500.0_JPRB)
ZEXP500R=1.0_JPRB/ZEXP500
ZEXPM500=EXP(-500.0_JPRB)
ZSR3=SQRT(3._JPRB)
ZWCRIT=0.9995_JPRB
KMODTS=2

ZSQRT_REPLOG = SQRT(REPLOG)

IC=0
DO JL = KIDIA, KFDIA
  IF (PRMUZ(JL) > 0.0_JPRB) THEN
    IC=IC+1
    INDEX(IC)=JL
  ENDIF
ENDDO
ICOUNT=IC
IF(ICOUNT==0)THEN
  RETURN
ENDIF

DO JK=1,KLEV
  DO IC=1,ICOUNT
    JL=INDEX(IC)
    IF (.NOT.LDRTCHK(JL,JK)) THEN
      PREF(JL,JK) =0.0_JPRB
      PTRA(JL,JK) =1.0_JPRB
      PREFD(JL,JK)=0.0_JPRB
      PTRAD(JL,JK)=1.0_JPRB
    ELSE
      ZTO1=PTAU(JL,JK)
      ZW  =PW(JL,JK)
      ZG  =PGG(JL,JK)

      !-- GENERAL TWO-STREAM EXPRESSIONS

      ZG3= 3._JPRB * ZG
      IF (KMODTS == 1) THEN
        ZGAMMA1= (7._JPRB - ZW * (4._JPRB + ZG3)) * 0.25_JPRB
        ZGAMMA2=-(1._JPRB - ZW * (4._JPRB - ZG3)) * 0.25_JPRB
        ZGAMMA3= (2._JPRB - ZG3 * PRMUZ(JL) ) * 0.25_JPRB
      ELSEIF (KMODTS == 2) THEN
        ZGAMMA1= (8._JPRB - ZW * (5._JPRB + ZG3)) * 0.25_JPRB
        ZGAMMA2=  3._JPRB *(ZW * (1._JPRB - ZG )) * 0.25_JPRB
        ZGAMMA3= (2._JPRB - ZG3 * PRMUZ(JL) ) * 0.25_JPRB
      ELSEIF (KMODTS == 3) THEN
        ZGAMMA1= ZSR3 * (2._JPRB - ZW * (1._JPRB + ZG)) * 0.5_JPRB
        ZGAMMA2= ZSR3 * ZW * (1._JPRB - ZG ) * 0.5_JPRB
        ZGAMMA3= (1._JPRB - ZSR3 * ZG * PRMUZ(JL) ) * 0.5_JPRB
      ENDIF
      ZGAMMA4= 1._JPRB - ZGAMMA3

      !-- RECOMPUTE ORIGINAL S.S.A. TO TEST FOR CONSERVATIVE SOLUTION
      !   ZTEMP=(1._JPRB - ZG)**2
      !   ZWO= ZW*ZTEMP/ (ZTEMP - (1._JPRB - ZW)*(ZG **2))

!       ZWO= ZW / (1._JPRB - (1._JPRB - ZW) * (ZG / (1._JPRB - ZG))**2)
!       IF (ZWO >= ZWCRIT) THEN

      ZZZ=(1._JPRB - ZG)**2
      IF (ZW*ZZZ >= ZWCRIT*(ZZZ - (1._JPRB - ZW)*(ZG **2))) THEN
        !!-- conservative scattering

        ZA  = ZGAMMA1 * PRMUZ(JL)
        ZA1 = ZA - ZGAMMA3
        ZGT = ZGAMMA1 * ZTO1

        !-- Homogeneous reflectance and transmittance

        ! collimated beam

!         ZE1 = MIN ( ZTO1 / PRMUZ(JL) , 500._JPRB)
!         ZE2 = EXP ( - ZE1 )
        ZZZ= ZTO1/PRMUZ(JL)
        IF(ZZZ <= 500._JPRB) THEN
          ZE2 = EXP ( - ZZZ )
        ELSE
          ZE2 = ZEXPM500
        ENDIF
        ZTEMP=1.0_JPRB/(1._JPRB + ZGT)
        PREF(JL,JK) = (ZGT - ZA1 * (1._JPRB - ZE2)) *ZTEMP
        PTRA(JL,JK) = 1._JPRB - PREF(JL,JK)

        ! isotropic incidence

        PREFD(JL,JK) = ZGT *ZTEMP
        PTRAD(JL,JK) = 1._JPRB - PREFD(JL,JK)

      ELSE

        !-- non-conservative scattering

        ZA1 = ZGAMMA1 * ZGAMMA4 + ZGAMMA2 * ZGAMMA3
        ZA2 = ZGAMMA1 * ZGAMMA3 + ZGAMMA2 * ZGAMMA4
        !      ZRK = SQRT ( ZGAMMA1**2 - ZGAMMA2**2)
!         ZRK = SQRT ( MAX ( REPLOG, ZGAMMA1**2 - ZGAMMA2**2) )
        ZZZ = ZGAMMA1**2 - ZGAMMA2**2
        IF (ZZZ >= REPLOG ) THEN
          ZRK = SQRT ( ZZZ )
        ELSE
          ZRK = ZSQRT_REPLOG
        ENDIF

        ZRP = ZRK * PRMUZ(JL)
        ZRP1 = 1._JPRB + ZRP
        ZRM1 = 1._JPRB - ZRP
        ZRK2 = 2._JPRB * ZRK
        ZRPP = 1._JPRB - ZRP*ZRP
        ZRKG = ZRK + ZGAMMA1
        ZR1  = ZRM1 * (ZA2 + ZRK * ZGAMMA3)
        ZR2  = ZRP1 * (ZA2 - ZRK * ZGAMMA3)
        ZR3  = ZRK2 * (ZGAMMA3 - ZA2 * PRMUZ(JL) )
        ZR4  = ZRPP * ZRKG
        ZR5  = ZRPP * (ZRK - ZGAMMA1)
        ZT1  = ZRP1 * (ZA1 + ZRK * ZGAMMA4)
        ZT2  = ZRM1 * (ZA1 - ZRK * ZGAMMA4)
        ZT3  = ZRK2 * (ZGAMMA4 + ZA1 * PRMUZ(JL) )
        ZT4  = ZR4
        ZT5  = ZR5
        ZBETA = - ZR5 / ZR4

        !-- Homogeneous reflectance and transmittance

        IF(ZRK * ZTO1 > 500._JPRB)THEN
          ZEP1=ZEXP500
          ZEM1=ZEXP500R
        ELSE
          ZEP1=EXP(ZRK * ZTO1)
          ZEM1=1.0_JPRB/ZEP1
        ENDIF
        IF(ZTO1 > 500._JPRB*PRMUZ(JL))THEN
          ZEP2=ZEXP500
          ZEM2=ZEXP500R
        ELSE
          ZEP2=EXP(ZTO1 / PRMUZ(JL))
          ZEM2=1.0_JPRB/ZEP2
        ENDIF

        !     ZE1 = MIN ( ZRK * ZTO1, 500._JPRB)
        !     ZE2 = MIN ( ZTO1 / PRMUZ , 500._JPRB)

        !     ZEP1 = EXP( ZE1 )
        !      ZEM1 = EXP(-ZE1 )
        !     ZEM1=1.0_JPRB/ZEP1

        !     ZEP2 = EXP( ZE2 )
        !      ZEM2 = EXP(-ZE2 )
        !     ZEM2=1.0_JPRB/ZEP2

        ! collimated beam

        ZDENR = ZR4*ZEP1 + ZR5*ZEM1
        !- bug noticed by Mike Iacono
        !      PREF(JK) = ZWO * (ZR1*ZEP1 - ZR2*ZEM1 - ZR3*ZEM2) / ZDENR
        PREF(JL,JK) = ZW  * (ZR1*ZEP1 - ZR2*ZEM1 - ZR3*ZEM2) / ZDENR

        ZDENT = ZT4*ZEP1 + ZT5*ZEM1
        !- bug noticed by Mike Iacono
        !      PTRA(JK) = ZEM2 * (1._JPRB - ZWO * (ZT1*ZEP1 - ZT2*ZEM1 - ZT3*ZEP2) / ZDENT)
        PTRA(JL,JK) = ZEM2 * (1._JPRB - ZW  * (ZT1*ZEP1 - ZT2*ZEM1 - ZT3*ZEP2) / ZDENT)

        ! diffuse beam

        ZEMM = ZEM1*ZEM1
        ZDEND = 1._JPRB / ( (1._JPRB - ZBETA*ZEMM ) * ZRKG)
        PREFD(JL,JK) =  ZGAMMA2 * (1._JPRB - ZEMM) * ZDEND
        PTRAD(JL,JK) =  ZRK2*ZEM1*ZDEND

      ENDIF

    ENDIF

  ENDDO
ENDDO

!     ------------------------------------------------------------------
END SUBROUTINE SRTM_REFTRA
