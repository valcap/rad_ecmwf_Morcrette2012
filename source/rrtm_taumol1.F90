!******************************************************************************
!                                                                             *
!                  Optical depths developed for the                           *
!                                                                             *
!                RAPID RADIATIVE TRANSFER MODEL (RRTM)                        *
!                                                                             *
!            ATMOSPHERIC AND ENVIRONMENTAL RESEARCH, INC.                     *
!                        840 MEMORIAL DRIVE                                   *
!                        CAMBRIDGE, MA 02139                                  *
!                                                                             *
!                           ELI J. MLAWER                                     *
!                         STEVEN J. TAUBMAN                                   *
!                         SHEPARD A. CLOUGH                                   *
!                                                                             *
!                       email:  mlawer@aer.com                                *
!                                                                             *
!        The authors wish to acknowledge the contributions of the             *
!        following people:  Patrick D. Brown, Michael J. Iacono,              *
!        Ronald E. Farren, Luke Chen, Robert Bergstrom.                       *
!                                                                             *
!******************************************************************************
! Modified by:                                                                *
!      JJ Morcrette 980714 ECMWF      for use on ECMWF's Fujitsu VPP770       *
!         Reformatted for F90 by JJMorcrette, ECMWF                           * 
!         - replacing COMMONs by MODULEs                                      *
!         - changing labelled to unlabelled DO loops                          *
!         - creating set-up routines for all block data statements            *
!         - reorganizing the parameter statements                             * 
!         - passing KLEV as argument                                          *
!         - suppressing some equivalencing                                    *
!                                                                             *
!      D Salmond    9907   ECMWF      Speed-up modifications                  *
!      D Salmond    000515 ECMWF      Speed-up modifications                  *
!******************************************************************************
!     TAUMOL                                                                  *
!                                                                             *
!     This file contains the subroutines TAUGBn (where n goes from            *
!     1 to 16).  TAUGBn calculates the optical depths and Planck fractions    *
!     per g-value and layer for band n.                                       *
!                                                                             *
!  Output:  optical depths (unitless)                                         *
!           fractions needed to compute Planck functions at every layer       *
!               and g-value                                                   *
!                                                                             *
!     COMMON /TAUGCOM/  TAUG(MXLAY,MG)                                        *
!     COMMON /PLANKG/   FRACS(MXLAY,MG)                                       *
!                                                                             *
!  Input                                                                      *
!                                                                             *
!     COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                  *
!     COMMON /PRECISE/  ONEMINUS                                              *
!     COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                    *
!    &                  PZ(0:MXLAY),TZ(0:MXLAY),TBOUND                        *
!     COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                              *
!    &                  COLH2O(MXLAY),COLCO2(MXLAY),                          *
!    &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),             *
!    &                  COLO2(MXLAY),CO2MULT(MXLAY)                           *
!     COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                            *
!    &                  FAC10(MXLAY),FAC11(MXLAY)                             *
!     COMMON /INTIND/   JP(MXLAY),JT(KIDIA:KFDIA,MXLAY),JT1(KIDIA:KFDIA,MXLAY)                        *
!     COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(KIDIA:KFDIA,MXLAY)       *
!                                                                             *
!     Description:                                                            *
!     NG(IBAND) - number of g-values in band IBAND                            *
!     NSPA(IBAND) - for the lower atmosphere, the number of reference         *
!                   atmospheres that are stored for band IBAND per            *
!                   pressure level and temperature.  Each of these            *
!                   atmospheres has different relative amounts of the         *
!                   key species for the band (i.e. different binary           *
!                   species parameters).                                      *
!     NSPB(IBAND) - same for upper atmosphere                                 *
!     ONEMINUS - since problems are caused in some cases by interpolation     *
!                parameters equal to or greater than 1, for these cases       *
!                these parameters are set to this value, slightly < 1.        *
!     PAVEL - layer pressures (mb)                                            *
!     TAVEL - layer temperatures (degrees K)                                  *
!     PZ - level pressures (mb)                                               *
!     TZ - level temperatures (degrees K)                                     *
!     LAYTROP - layer at which switch is made from one combination of         *
!               key species to another                                        *
!     COLH2O, COLCO2, COLO3, COLN2O, COLCH4 - column amounts of water         *
!               vapor,carbon dioxide, ozone, nitrous ozide, methane,          *
!               respectively (molecules/cm**2)                                *
!     CO2MULT - for bands in which carbon dioxide is implemented as a         *
!               trace species, this is the factor used to multiply the        *
!               band's average CO2 absorption coefficient to get the added    *
!               contribution to the optical depth relative to 355 ppm.        *
!     FACij(LAY) - for layer LAY, these are factors that are needed to        *
!                  compute the interpolation factors that multiply the        *
!                  appropriate reference k-values.  A value of 0 (1) for      *
!                  i,j indicates that the corresponding factor multiplies     *
!                  reference k-value for the lower (higher) of the two        *
!                  appropriate temperatures, and altitudes, respectively.     *
!     JP - the index of the lower (in altitude) of the two appropriate        *
!          reference pressure levels needed for interpolation                 *
!     JT, JT1 - the indices of the lower of the two appropriate reference     *
!               temperatures needed for interpolation (for pressure           *
!               levels JP and JP+1, respectively)                             *
!     SELFFAC - scale factor needed to water vapor self-continuum, equals     *
!               (water vapor density)/(atmospheric density at 296K and        *
!               1013 mb)                                                      *
!     SELFFRAC - factor needed for temperature interpolation of reference     *
!                water vapor self-continuum data                              *
!     INDSELF - index of the lower of the two appropriate reference           *
!               temperatures needed for the self-continuum interpolation      *
!                                                                             *
!  Data input                                                                 *
!     COMMON /Kn/ KA(NSPA(n),5,13,MG), KB(NSPB(n),5,13:59,MG), SELFREF(10,MG) *
!        (note:  n is the band number)                                        *
!                                                                             *
!     Description:                                                            *
!     KA - k-values for low reference atmospheres (no water vapor             *
!          self-continuum) (units: cm**2/molecule)                            *
!     KB - k-values for high reference atmospheres (all sources)              *
!          (units: cm**2/molecule)                                            *
!     SELFREF - k-values for water vapor self-continuum for reference         *
!               atmospheres (used below LAYTROP)                              *
!               (units: cm**2/molecule)                                       *
!                                                                             *
!     DIMENSION ABSA(65*NSPA(n),MG), ABSB(235*NSPB(n),MG)                     *
!     EQUIVALENCE (KA,ABSA),(KB,ABSB)                                         *
!                                                                             *
!******************************************************************************

SUBROUTINE RRTM_TAUMOL1 (KIDIA,KFDIA,KLEV,P_TAU,&
 & P_TAUAERL,P_FAC00,P_FAC01,P_FAC10,P_FAC11,P_FORFAC,K_JP,K_JT,K_JT1,&
 & P_COLH2O,K_LAYTROP,P_SELFFAC,P_SELFFRAC,K_INDSELF,PFRAC)  

!     Written by Eli J. Mlawer, Atmospheric & Environmental Research.
!     Revised by Michael J. Iacono, Atmospheric & Environmental Research.

!     BAND 1:  10-250 cm-1 (low - H2O; high - H2O)
 
! Modifications
!        M.Hamrud      01-Oct-2003 CY28 Cleaning

!     D Salmond   2000-05-15 speed-up
!     JJMorcrette 2000-05-17 speed-up
!        NEC           25-Oct-2007 Optimisations

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE PARRRTM  , ONLY : JPLAY  ,JPBAND ,JPGPT  ,NG1
USE YOERRTWN , ONLY : NSPA   ,NSPB
USE YOERRTA1 , ONLY : ABSA   ,ABSB   ,FRACREFA, FRACREFB,&
 & FORREF   ,SELFREF   

!#include "yoeratm.h"

!      REAL TAUAER(JPLAY)

IMPLICIT NONE

!  Output
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_TAU(KIDIA:KFDIA,JPGPT,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_TAUAERL(KIDIA:KFDIA,JPLAY,JPBAND) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_FAC00(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_FAC01(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_FAC10(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_FAC11(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_FORFAC(KIDIA:KFDIA,JPLAY) 
INTEGER(KIND=JPIM),INTENT(IN)    :: K_JP(KIDIA:KFDIA,JPLAY) 
INTEGER(KIND=JPIM),INTENT(IN)    :: K_JT(KIDIA:KFDIA,JPLAY) 
INTEGER(KIND=JPIM),INTENT(IN)    :: K_JT1(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_COLH2O(KIDIA:KFDIA,JPLAY) 
INTEGER(KIND=JPIM),INTENT(IN)    :: K_LAYTROP(KIDIA:KFDIA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_SELFFAC(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_SELFFRAC(KIDIA:KFDIA,JPLAY) 
INTEGER(KIND=JPIM),INTENT(IN)    :: K_INDSELF(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PFRAC(KIDIA:KFDIA,JPGPT,JPLAY) 
!- from AER
!- from INTFAC      
!- from INTIND
!- from PRECISE             
!- from PROFDATA             
!- from SELF             
!- from SP             
INTEGER(KIND=JPIM) :: IND0(JPLAY),IND1(JPLAY),INDS(JPLAY)

INTEGER(KIND=JPIM) :: JG, JLAY
INTEGER(KIND=JPIM) :: JLON

!      EQUIVALENCE (TAUAERL(1,1),TAUAER)

!     Compute the optical depth by interpolating in ln(pressure) and 
!     temperature.  Below LAYTROP, the water vapor self-continuum 
!     is interpolated (in temperature) separately.  


DO JLAY = 1, KLEV
  DO JLON = KIDIA, KFDIA
  IF (JLAY <= K_LAYTROP(JLON)) THEN
    IND0(JLAY) = ((K_JP(JLON,JLAY)-1)*5+(K_JT(JLON,JLAY)-1))*NSPA(1) + 1
    IND1(JLAY) = (K_JP(JLON,JLAY)*5+(K_JT1(JLON,JLAY)-1))*NSPA(1) + 1
    INDS(JLAY) = K_INDSELF(JLON,JLAY)

!CDIR UNROLL=NG1
    DO JG = 1, NG1
!-- DS_000515  
      P_TAU(JLON,JG,JLAY) = P_COLH2O(JLON,JLAY) *&
       & (P_FAC00(JLON,JLAY) * ABSA(IND0(JLAY)  ,JG) +&
       & P_FAC10(JLON,JLAY) * ABSA(IND0(JLAY)+1,JG) +&
       & P_FAC01(JLON,JLAY) * ABSA(IND1(JLAY)  ,JG) +&
       & P_FAC11(JLON,JLAY) * ABSA(IND1(JLAY)+1,JG) +&
       & P_SELFFAC(JLON,JLAY) * (SELFREF(INDS(JLAY),JG) + &
       & P_SELFFRAC(JLON,JLAY) *&
       & (SELFREF(INDS(JLAY)+1,JG) - SELFREF(INDS(JLAY),JG)))&
       & + P_FORFAC(JLON,JLAY) * FORREF(JG) ) &
       & + P_TAUAERL(JLON,JLAY,1)  
      PFRAC(JLON,JG,JLAY) = FRACREFA(JG)
    ENDDO
  ENDIF

  IF (JLAY > K_LAYTROP(JLON)) THEN
    IND0(JLAY) = ((K_JP(JLON,JLAY)-13)*5+(K_JT(JLON,JLAY)-1))*NSPB(1) + 1
    IND1(JLAY) = ((K_JP(JLON,JLAY)-12)*5+(K_JT1(JLON,JLAY)-1))*NSPB(1) + 1

!-- JJM000517
!CDIR UNROLL=NG1
    DO JG = 1, NG1
!-- JJM000517
      P_TAU(JLON,JG,JLAY) = P_COLH2O(JLON,JLAY) *&
       & (P_FAC00(JLON,JLAY) * ABSB(IND0(JLAY)  ,JG) +&
       & P_FAC10(JLON,JLAY) * ABSB(IND0(JLAY)+1,JG) +&
       & P_FAC01(JLON,JLAY) * ABSB(IND1(JLAY)  ,JG) +&
       & P_FAC11(JLON,JLAY) * ABSB(IND1(JLAY)+1,JG)&
       & + P_FORFAC(JLON,JLAY) * FORREF(JG) ) &
       & + P_TAUAERL(JLON,JLAY,1)  
      PFRAC(JLON,JG,JLAY) = FRACREFB(JG)
    ENDDO
  ENDIF
  ENDDO
ENDDO


END SUBROUTINE RRTM_TAUMOL1
