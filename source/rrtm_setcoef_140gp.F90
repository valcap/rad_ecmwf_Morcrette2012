SUBROUTINE RRTM_SETCOEF_140GP (KIDIA,KFDIA,KLEV,P_COLDRY,P_WKL,&
 & P_FAC00,P_FAC01,P_FAC10,P_FAC11,P_FORFAC,K_JP,K_JT,K_JT1,&
 & P_COLH2O,P_COLCO2,P_COLO3,P_COLN2O,P_COLCH4,P_COLO2,P_CO2MULT,&
 & K_LAYTROP,K_LAYSWTCH,K_LAYLOW,PAVEL,P_TAVEL,P_SELFFAC,P_SELFFRAC,K_INDSELF)  

!     Reformatted for F90 by JJMorcrette, ECMWF, 980714
!        NEC           25-Oct-2007 Optimisations

!     Purpose:  For a given atmosphere, calculate the indices and
!     fractions related to the pressure and temperature interpolations.
!     Also calculate the values of the integrated Planck functions 
!     for each band at the level and layer temperatures.

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE PARRRTM  , ONLY : JPLAY     ,JPINPX
USE YOERRTRF , ONLY :       PREFLOG   ,TREF

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_COLDRY(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_WKL(KIDIA:KFDIA,JPINPX,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_FAC00(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_FAC01(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_FAC10(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_FAC11(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_FORFAC(KIDIA:KFDIA,JPLAY) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: K_JP(KIDIA:KFDIA,JPLAY) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: K_JT(KIDIA:KFDIA,JPLAY) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: K_JT1(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_COLH2O(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_COLCO2(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_COLO3(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_COLN2O(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_COLCH4(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_COLO2(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_CO2MULT(KIDIA:KFDIA,JPLAY) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: K_LAYTROP(KIDIA:KFDIA) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: K_LAYSWTCH(KIDIA:KFDIA) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: K_LAYLOW(KIDIA:KFDIA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAVEL(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_TAVEL(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_SELFFAC(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_SELFFRAC(KIDIA:KFDIA,JPLAY) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: K_INDSELF(KIDIA:KFDIA,JPLAY) 
!- from INTFAC      
!- from INTIND
!- from PROFDATA             
!- from PROFILE             
!- from SELF             
INTEGER(KIND=JPIM) :: JP1, JLAY
INTEGER(KIND=JPIM) :: JLON

REAL(KIND=JPRB) :: Z_CO2REG, Z_COMPFP, Z_FACTOR, Z_FP, Z_FT, Z_FT1, Z_PLOG, Z_SCALEFAC, Z_STPFAC, Z_WATER

!#include "yoeratm.h"    


DO JLON = KIDIA, KFDIA
  Z_STPFAC = 296._JPRB/1013._JPRB

  K_LAYTROP(JLON)  = 0
  K_LAYSWTCH(JLON) = 0
  K_LAYLOW(JLON)   = 0
  DO JLAY = 1, KLEV
!        Find the two reference pressures on either side of the
!        layer pressure.  Store them in JP and JP1.  Store in FP the
!        fraction of the difference (in ln(pressure)) between these
!        two values that the layer pressure lies.
    Z_PLOG = LOG(PAVEL(JLON,JLAY))
    K_JP(JLON,JLAY) = INT(36._JPRB - 5*(Z_PLOG+0.04_JPRB))
    IF (K_JP(JLON,JLAY)  <  1) THEN
      K_JP(JLON,JLAY) = 1
    ELSEIF (K_JP(JLON,JLAY)  >  58) THEN
      K_JP(JLON,JLAY) = 58
    ENDIF
    JP1 = K_JP(JLON,JLAY) + 1
    Z_FP = 5._JPRB * (PREFLOG(K_JP(JLON,JLAY)) - Z_PLOG)

!        Determine, for each reference pressure (JP and JP1), which
!        reference temperature (these are different for each  
!        reference pressure) is nearest the layer temperature but does
!        not exceed it.  Store these indices in JT and JT1, resp.
!        Store in FT (resp. FT1) the fraction of the way between JT
!        (JT1) and the next highest reference temperature that the 
!        layer temperature falls.

    K_JT(JLON,JLAY) = INT(3._JPRB + (P_TAVEL(JLON,JLAY)-TREF(K_JP(JLON,JLAY)))/15._JPRB)
    IF (K_JT(JLON,JLAY)  <  1) THEN
      K_JT(JLON,JLAY) = 1
    ELSEIF (K_JT(JLON,JLAY)  >  4) THEN
      K_JT(JLON,JLAY) = 4
    ENDIF
    Z_FT = ((P_TAVEL(JLON,JLAY)-TREF(K_JP(JLON,JLAY)))/15._JPRB) - REAL(K_JT(JLON,JLAY)-3)
    K_JT1(JLON,JLAY) = INT(3._JPRB + (P_TAVEL(JLON,JLAY)-TREF(JP1))/15._JPRB)
    IF (K_JT1(JLON,JLAY)  <  1) THEN
      K_JT1(JLON,JLAY) = 1
    ELSEIF (K_JT1(JLON,JLAY)  >  4) THEN
      K_JT1(JLON,JLAY) = 4
    ENDIF
    Z_FT1 = ((P_TAVEL(JLON,JLAY)-TREF(JP1))/15._JPRB) - REAL(K_JT1(JLON,JLAY)-3)

    Z_WATER = P_WKL(JLON,1,JLAY)/P_COLDRY(JLON,JLAY)
    Z_SCALEFAC = PAVEL(JLON,JLAY) * Z_STPFAC / P_TAVEL(JLON,JLAY)

!        If the pressure is less than ~100mb, perform a different
!        set of species interpolations.
!         IF (PLOG .LE. 4.56) GO TO 5300
!--------------------------------------         
    IF (Z_PLOG  >  4.56_JPRB) THEN
      K_LAYTROP(JLON) =  K_LAYTROP(JLON) + 1
!        For one band, the "switch" occurs at ~300 mb. 
      IF (Z_PLOG  >=  5.76_JPRB) K_LAYSWTCH(JLON) = K_LAYSWTCH(JLON) + 1
      IF (Z_PLOG  >=  6.62_JPRB) K_LAYLOW(JLON) = K_LAYLOW(JLON) + 1

      P_FORFAC(JLON,JLAY) = Z_SCALEFAC / (1.0_JPRB+Z_WATER)

!        Set up factors needed to separately include the water vapor
!        self-continuum in the calculation of absorption coefficient.
!C           SELFFAC(LAY) = WATER * SCALEFAC / (1.+WATER)
      P_SELFFAC(JLON,JLAY) = Z_WATER * P_FORFAC(JLON,JLAY)
      Z_FACTOR = (P_TAVEL(JLON,JLAY)-188.0_JPRB)/7.2_JPRB
      K_INDSELF(JLON,JLAY) = MIN(9, MAX(1, INT(Z_FACTOR)-7))
      P_SELFFRAC(JLON,JLAY) = Z_FACTOR - REAL(K_INDSELF(JLON,JLAY) + 7)

!        Calculate needed column amounts.
      P_COLH2O(JLON,JLAY) = 1.E-20_JPRB * P_WKL(JLON,1,JLAY)
      P_COLCO2(JLON,JLAY) = 1.E-20_JPRB * P_WKL(JLON,2,JLAY)
      P_COLO3(JLON,JLAY)  = 1.E-20_JPRB * P_WKL(JLON,3,JLAY)
      P_COLN2O(JLON,JLAY) = 1.E-20_JPRB * P_WKL(JLON,4,JLAY)
      P_COLCH4(JLON,JLAY) = 1.E-20_JPRB * P_WKL(JLON,6,JLAY)
      P_COLO2(JLON,JLAY)  = 1.E-20_JPRB * P_WKL(JLON,7,JLAY)
      IF (P_COLCO2(JLON,JLAY)  ==  0.0_JPRB) P_COLCO2(JLON,JLAY) = 1.E-32_JPRB * P_COLDRY(JLON,JLAY)
      IF (P_COLN2O(JLON,JLAY)  ==  0.0_JPRB) P_COLN2O(JLON,JLAY) = 1.E-32_JPRB * P_COLDRY(JLON,JLAY)
      IF (P_COLCH4(JLON,JLAY)  ==  0.0_JPRB) P_COLCH4(JLON,JLAY) = 1.E-32_JPRB * P_COLDRY(JLON,JLAY)
!        Using E = 1334.2 cm-1.
      Z_CO2REG = 3.55E-24_JPRB * P_COLDRY(JLON,JLAY)
      P_CO2MULT(JLON,JLAY)= (P_COLCO2(JLON,JLAY) - Z_CO2REG) *&
       & 272.63_JPRB*EXP(-1919.4_JPRB/P_TAVEL(JLON,JLAY))/(8.7604E-4_JPRB*P_TAVEL(JLON,JLAY))  
!         GO TO 5400
!------------------
    ELSE
!        Above LAYTROP.
! 5300    CONTINUE

!        Calculate needed column amounts.
      P_FORFAC(JLON,JLAY) = Z_SCALEFAC / (1.0_JPRB+Z_WATER)

      P_COLH2O(JLON,JLAY) = 1.E-20_JPRB * P_WKL(JLON,1,JLAY)
      P_COLCO2(JLON,JLAY) = 1.E-20_JPRB * P_WKL(JLON,2,JLAY)
      P_COLO3(JLON,JLAY)  = 1.E-20_JPRB * P_WKL(JLON,3,JLAY)
      P_COLN2O(JLON,JLAY) = 1.E-20_JPRB * P_WKL(JLON,4,JLAY)
      P_COLCH4(JLON,JLAY) = 1.E-20_JPRB * P_WKL(JLON,6,JLAY)
      P_COLO2(JLON,JLAY)  = 1.E-20_JPRB * P_WKL(JLON,7,JLAY)
      IF (P_COLCO2(JLON,JLAY)  ==  0.0_JPRB) P_COLCO2(JLON,JLAY) = 1.E-32_JPRB * P_COLDRY(JLON,JLAY)
      IF (P_COLN2O(JLON,JLAY)  ==  0.0_JPRB) P_COLN2O(JLON,JLAY) = 1.E-32_JPRB * P_COLDRY(JLON,JLAY)
      IF (P_COLCH4(JLON,JLAY)  ==  0.0_JPRB) P_COLCH4(JLON,JLAY) = 1.E-32_JPRB * P_COLDRY(JLON,JLAY)
      Z_CO2REG = 3.55E-24_JPRB * P_COLDRY(JLON,JLAY)
      P_CO2MULT(JLON,JLAY)= (P_COLCO2(JLON,JLAY) - Z_CO2REG) *&
       & 272.63_JPRB*EXP(-1919.4_JPRB/P_TAVEL(JLON,JLAY))/(8.7604E-4_JPRB*P_TAVEL(JLON,JLAY))  
!----------------     
    ENDIF
! 5400    CONTINUE

!        We have now isolated the layer ln pressure and temperature,
!        between two reference pressures and two reference temperatures 
!        (for each reference pressure).  We multiply the pressure 
!        fraction FP with the appropriate temperature fractions to get 
!        the factors that will be needed for the interpolation that yields
!        the optical depths (performed in routines TAUGBn for band n).

    Z_COMPFP = 1.0_JPRB - Z_FP
    P_FAC10(JLON,JLAY) = Z_COMPFP * Z_FT
    P_FAC00(JLON,JLAY) = Z_COMPFP * (1.0_JPRB - Z_FT)
    P_FAC11(JLON,JLAY) = Z_FP * Z_FT1
    P_FAC01(JLON,JLAY) = Z_FP * (1.0_JPRB - Z_FT1)

  ENDDO

! MT 981104 
!-- Set LAYLOW for profiles with surface pressure less than 750 hPa. 
  IF (K_LAYLOW(JLON) == 0) K_LAYLOW(JLON)=1
ENDDO


END SUBROUTINE RRTM_SETCOEF_140GP
