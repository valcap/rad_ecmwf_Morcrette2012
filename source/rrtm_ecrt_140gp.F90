!****************** SUBROUTINE RRTM_ECRT_140GP **************************

SUBROUTINE RRTM_ECRT_140GP &
 & (KLON,KIDIA,KFDIA, KLEV, KCLD,&
 & PAER , PAPH , PAP,&
 & PTS  , PTH  , PT,&
 & P_ZEMIS, P_ZEMIW,&
 & PQ   , PCO2, PCH4, PN2O, PNO2, PC11, PC12, PC22, PCL4, POZN, PCLDF, PTAUCLD, PTCLEAR,&
 & P_CLDFRAC,P_TAUCLD,P_COLDRY,P_WKL,P_WX,&
 & P_TAUAERL,PAVEL,P_TAVEL,PZ,P_TZ,P_TBOUND,K_NLAYERS,P_SEMISS,K_IREFLECT)  

!     Reformatted for F90 by JJMorcrette, ECMWF, 980714

!     Read in atmospheric profile from ECMWF radiation code, and prepare it
!     for use in RRTM.  Set other RRTM input parameters.  Values are passed
!     back through existing RRTM arrays and commons.

!- Modifications

!     2000-05-15 Deborah Salmond  Speed-up
!     JJMorcrette 20071015  3D fields of CO2, CH4, N2O and NO2
!        NEC           25-Oct-2007 Optimisations

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE PARRRTM  , ONLY : JPBAND   ,JPXSEC   ,JPLAY   ,&
 & JPINPX  
USE YOERAD   , ONLY : NOVLP
USE YOERDI   , ONLY : RCH4     ,RN2O    ,RCFC11  ,RCFC12, RCFC22, RCCL4
USE YOESW    , ONLY : RAER

!------------------------------Arguments--------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLON! Number of atmospheres (longitudes) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV! Number of atmospheric layers 
INTEGER(KIND=JPIM),INTENT(OUT)   :: KCLD(KIDIA:KFDIA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAER(KLON,6,KLEV) ! Aerosol optical thickness
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPH(KLON,KLEV+1) ! Interface pressures (Pa)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAP(KLON,KLEV) ! Layer pressures (Pa)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS(KLON) ! Surface temperature (K)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTH(KLON,KLEV+1) ! Interface temperatures (K)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) ! Layer temperature (K)
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_ZEMIS(KLON) ! Non-window surface emissivity
REAL(KIND=JPRB)   ,INTENT(IN)    :: P_ZEMIW(KLON) ! Window surface emissivity
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQ(KLON,KLEV) ! H2O specific humidity (mmr)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCO2(KLON,KLEV) ! CO2 mass mixing ratio
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCH4(KLON,KLEV) ! CH4 mass mixing ratio
REAL(KIND=JPRB)   ,INTENT(IN)    :: PN2O(KLON,KLEV) ! N2O mass mixing ratio
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNO2(KLON,KLEV) ! NO2 mass mixing ratio
REAL(KIND=JPRB)   ,INTENT(IN)    :: PC11(KLON,KLEV) ! CFC11 mass mixing ratio
REAL(KIND=JPRB)   ,INTENT(IN)    :: PC12(KLON,KLEV) ! CFC12 mass mixing ratio
REAL(KIND=JPRB)   ,INTENT(IN)    :: PC22(KLON,KLEV) ! CFC22 mass mixing ratio
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCL4(KLON,KLEV) ! CCL4  mass mixing ratio
REAL(KIND=JPRB)   ,INTENT(IN)    :: POZN(KLON,KLEV) ! O3 mass mixing ratio
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCLDF(KLON,KLEV) ! Cloud fraction
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTAUCLD(KLON,KLEV,JPBAND) ! Cloud optical depth
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PTCLEAR (KIDIA:KFDIA)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_CLDFRAC(KIDIA:KFDIA,JPLAY) ! Cloud fraction
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_TAUCLD(KIDIA:KFDIA,JPLAY,JPBAND) ! Spectral optical thickness
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_COLDRY(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_WKL(KIDIA:KFDIA,JPINPX,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_WX(KIDIA:KFDIA,JPXSEC,JPLAY) ! Amount of trace gases
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_TAUAERL(KIDIA:KFDIA,JPLAY,JPBAND) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PAVEL(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_TAVEL(KIDIA:KFDIA,JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ(KIDIA:KFDIA,0:JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_TZ(KIDIA:KFDIA,0:JPLAY) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_TBOUND(KIDIA:KFDIA) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: K_NLAYERS(KIDIA:KFDIA) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P_SEMISS(KIDIA:KFDIA,JPBAND) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: K_IREFLECT(KIDIA:KFDIA) 
!      real rch4                       ! CH4 mass mixing ratio
!      real rn2o                       ! N2O mass mixing ratio
!      real rcfc11                     ! CFC11 mass mixing ratio
!      real rcfc12                     ! CFC12 mass mixing ratio
!      real rcfc22                     ! CFC22 mass mixing ratio
!      real rccl4                      ! CCL4  mass mixing ratio
!- from AER
!- from PROFILE             
!- from SURFACE             
REAL(KIND=JPRB) :: ztauaer(5)
REAL(KIND=JPRB) :: ZC1J(KIDIA:KFDIA,0:klev)               ! total cloud from top and level k
REAL(KIND=JPRB) :: ZAMD                  ! Effective molecular weight of dry air (g/mol)
REAL(KIND=JPRB) :: ZAMW                  ! Molecular weight of water vapor (g/mol)
REAL(KIND=JPRB) :: ZAMCO2                ! Molecular weight of carbon dioxide (g/mol)
REAL(KIND=JPRB) :: ZAMO                  ! Molecular weight of ozone (g/mol)
REAL(KIND=JPRB) :: ZAMCH4                ! Molecular weight of methane (g/mol)
REAL(KIND=JPRB) :: ZAMN2O                ! Molecular weight of nitrous oxide (g/mol)
REAL(KIND=JPRB) :: ZAMC11                ! Molecular weight of CFC11 (g/mol) - CFCL3
REAL(KIND=JPRB) :: ZAMC12                ! Molecular weight of CFC12 (g/mol) - CF2CL2
REAL(KIND=JPRB) :: ZAMC22                ! Molecular weight of CFC22 (g/mol) - CF2CL2
REAL(KIND=JPRB) :: ZAMCL4                ! Molecular weight of CCL4  (g/mol) - CCl4
REAL(KIND=JPRB) :: ZAVGDRO               ! Avogadro's number (molecules/mole)
REAL(KIND=JPRB) :: ZGRAVIT               ! Gravitational acceleration (cm/sec2)

! Atomic weights for conversion from mass to volume mixing ratios; these
!  are the same values used in ECRT to assure accurate conversion to vmr
data ZAMD   /  28.970_JPRB    /
data ZAMW   /  18.0154_JPRB   /
data ZAMCO2 /  44.011_JPRB    /
data ZAMO   /  47.9982_JPRB   /
data ZAMCH4 /  16.043_JPRB    /
data ZAMN2O /  44.013_JPRB    /
data ZAMC11 / 137.3686_JPRB   /
data ZAMC12 / 120.9140_JPRB   /
data ZAMC22 /  86.4690_JPRB   /
data ZAMCL4 / 153.8230_JPRB   /
data ZAVGDRO/ 6.02214E23_JPRB /
data ZGRAVIT/ 9.80665E02_JPRB /

INTEGER(KIND=JPIM) :: IATM, IMOL, IXMAX, J1, J2, IAE, JB, JK, JL, JLEV
INTEGER(KIND=JPIM), PARAMETER :: I_NMOL = 6
INTEGER(KIND=JPIM) :: JLON

REAL(KIND=JPRB) :: ZAMM, ZCLDLY, ZCLEAR(KIDIA:KFDIA), ZCLOUD(KIDIA:KFDIA), ZEPSEC

! ***

! *** mji
! Initialize all molecular amounts and aerosol optical depths to zero here, 
! then pass ECRT amounts into RRTM arrays below.

!      DATA ZWKL /MAXPRDW*0.0/
!      DATA ZWX  /MAXPROD*0.0/
!      DATA KREFLECT /0/

! Activate cross section molecules:
!     NXMOL     - number of cross-sections input by user
!     IXINDX(I) - index of cross-section molecule corresponding to Ith
!                 cross-section specified by user
!                 = 0 -- not allowed in RRTM
!                 = 1 -- CCL4
!                 = 2 -- CFC11
!                 = 3 -- CFC12
!                 = 4 -- CFC22
!      DATA KXMOL  /2/
!      DATA KXINDX /0,2,3,0,31*0/

!      IREFLECT(KIDIA:KFDIA)=KREFLECT
!      NXMOL=KXMOL


DO JLON=KIDIA,KFDIA
  K_IREFLECT(JLON)=0
ENDDO

DO J1=1,35
! IXINDX(J1)=0
  DO J2=1,KLEV
    DO JLON=KIDIA,KFDIA
      P_WKL(JLON,J1,J2)=0.0_JPRB 
    ENDDO
  ENDDO
ENDDO
!IXINDX(2)=2
!IXINDX(3)=3

!     Set parameters needed for RRTM execution:
IATM    = 0
!      IXSECT  = 1
!      NUMANGS = 0
!      IOUT    = -1
IXMAX   = 4

!     Bands 6,7,8 are considered the 'window' and allowed to have a
!     different surface emissivity (as in ECMWF).  Eli wrote this part....
DO JLON=KIDIA,KFDIA
  P_SEMISS(JLON,1)  = P_ZEMIS(JLON)
  P_SEMISS(JLON,2)  = P_ZEMIS(JLON)
  P_SEMISS(JLON,3)  = P_ZEMIS(JLON)
  P_SEMISS(JLON,4)  = P_ZEMIS(JLON)
  P_SEMISS(JLON,5)  = P_ZEMIS(JLON)
  P_SEMISS(JLON,6)  = P_ZEMIW(JLON)
  P_SEMISS(JLON,7)  = P_ZEMIW(JLON)
  P_SEMISS(JLON,8)  = P_ZEMIW(JLON)
  P_SEMISS(JLON,9)  = P_ZEMIS(JLON)
  P_SEMISS(JLON,10) = P_ZEMIS(JLON)
  P_SEMISS(JLON,11) = P_ZEMIS(JLON)
  P_SEMISS(JLON,12) = P_ZEMIS(JLON)
  P_SEMISS(JLON,13) = P_ZEMIS(JLON)
  P_SEMISS(JLON,14) = P_ZEMIS(JLON)
  P_SEMISS(JLON,15) = P_ZEMIS(JLON)
  P_SEMISS(JLON,16) = P_ZEMIS(JLON)

!     Set surface temperature.  

  P_TBOUND(JLON) = pts(JLON)

!     Install ECRT arrays into RRTM arrays for pressure, temperature,
!     and molecular amounts.  Pressures are converted from Pascals
!     (ECRT) to mb (RRTM).  H2O, CO2, O3 and trace gas amounts are 
!     converted from mass mixing ratio to volume mixing ratio.  CO2
!     converted with same dry air and CO2 molecular weights used in 
!     ECRT to assure correct conversion back to the proper CO2 vmr.
!     The dry air column COLDRY (in molec/cm2) is calculated from 
!     the level pressures PZ (in mb) based on the hydrostatic equation
!     and includes a correction to account for H2O in the layer.  The
!     molecular weight of moist air (amm) is calculated for each layer.
!     Note: RRTM levels count from bottom to top, while the ECRT input
!     variables count from the top down and must be reversed here.

  K_NLAYERS(JLON) = klev
  PZ(JLON,0) = paph(JLON,klev+1)/100._JPRB
  P_TZ(JLON,0) = pth(JLON,klev+1)
ENDDO

DO JLON=KIDIA,KFDIA
  DO JLEV = 1, KLEV
    PAVEL(JLON,JLEV) = pap(JLON,KLEV-JLEV+1)/100._JPRB
    P_TAVEL(JLON,JLEV) = pt(JLON,KLEV-JLEV+1)
    PZ(JLON,JLEV) = paph(JLON,KLEV-JLEV+1)/100._JPRB
    P_TZ(JLON,JLEV) = pth(JLON,KLEV-JLEV+1)
    P_WKL(JLON,1,JLEV) = PQ(JLON,KLEV-JLEV+1)  *ZAMD/ZAMW
    P_WKL(JLON,2,JLEV) = PCO2(JLON,KLEV-JLEV+1)*ZAMD/ZAMCO2
    P_WKL(JLON,3,JLEV) = POZN(JLON,KLEV-JLEV+1)*ZAMD/ZAMO
    P_WKL(JLON,4,JLEV) = PN2O(JLON,KLEV-JLEV+1)*ZAMD/ZAMN2O
    P_WKL(JLON,6,JLEV) = PCH4(JLON,KLEV-JLEV+1)*ZAMD/ZAMCH4
    ZAMM = (1-P_WKL(JLON,1,JLEV))*ZAMD + P_WKL(JLON,1,JLEV)*ZAMW
    P_COLDRY(JLON,JLEV) = (PZ(JLON,JLEV-1)-PZ(JLON,JLEV))*1.E3_JPRB*ZAVGDRO/(ZGRAVIT*ZAMM*(1+P_WKL(JLON,1,JLEV)))
  ENDDO
ENDDO

!- Fill RRTM aerosol arrays with operational ECMWF aerosols,
!  do the mixing and distribute over the 16 spectral intervals

DO JLON=KIDIA,KFDIA
  DO JLEV=1,KLEV
    JK=KLEV-JLEV+1
    IAE=1
    ZTAUAER(IAE) =&
   & RAER(IAE,1)*PAER(JLON,1,JK)+RAER(IAE,2)*PAER(JLON,2,JK)&
   & +RAER(IAE,3)*PAER(JLON,3,JK)+RAER(IAE,4)*PAER(JLON,4,JK)&
   & +RAER(IAE,5)*PAER(JLON,5,JK)+RAER(IAE,6)*PAER(JLON,6,JK)  
    P_TAUAERL(JLON,JLEV, 1)=ZTAUAER(1)
    P_TAUAERL(JLON,JLEV, 2)=ZTAUAER(1)
    IAE=2
    ZTAUAER(IAE) =&
   & RAER(IAE,1)*PAER(JLON,1,JK)+RAER(IAE,2)*PAER(JLON,2,JK)&
   & +RAER(IAE,3)*PAER(JLON,3,JK)+RAER(IAE,4)*PAER(JLON,4,JK)&
   & +RAER(IAE,5)*PAER(JLON,5,JK)+RAER(IAE,6)*PAER(JLON,6,JK)  
    P_TAUAERL(JLON,JLEV, 3)=ZTAUAER(2)
    P_TAUAERL(JLON,JLEV, 4)=ZTAUAER(2)
    P_TAUAERL(JLON,JLEV, 5)=ZTAUAER(2)
    IAE=3
    ZTAUAER(IAE) =&
   & RAER(IAE,1)*PAER(JLON,1,JK)+RAER(IAE,2)*PAER(JLON,2,JK)&
   & +RAER(IAE,3)*PAER(JLON,3,JK)+RAER(IAE,4)*PAER(JLON,4,JK)&
   & +RAER(IAE,5)*PAER(JLON,5,JK)+RAER(IAE,6)*PAER(JLON,6,JK)  
    P_TAUAERL(JLON,JLEV, 6)=ZTAUAER(3)
    P_TAUAERL(JLON,JLEV, 8)=ZTAUAER(3)
    P_TAUAERL(JLON,JLEV, 9)=ZTAUAER(3)
    IAE=4
    ZTAUAER(IAE) =&
   & RAER(IAE,1)*PAER(JLON,1,JK)+RAER(IAE,2)*PAER(JLON,2,JK)&
   & +RAER(IAE,3)*PAER(JLON,3,JK)+RAER(IAE,4)*PAER(JLON,4,JK)&
   & +RAER(IAE,5)*PAER(JLON,5,JK)+RAER(IAE,6)*PAER(JLON,6,JK)  
    P_TAUAERL(JLON,JLEV, 7)=ZTAUAER(4)
    IAE=5
    ZTAUAER(IAE) =&
   & RAER(IAE,1)*PAER(JLON,1,JK)+RAER(IAE,2)*PAER(JLON,2,JK)&
   & +RAER(IAE,3)*PAER(JLON,3,JK)+RAER(IAE,4)*PAER(JLON,4,JK)&
   & +RAER(IAE,5)*PAER(JLON,5,JK)+RAER(IAE,6)*PAER(JLON,6,JK)  
    P_TAUAERL(JLON,JLEV,10)=ZTAUAER(5)
    P_TAUAERL(JLON,JLEV,11)=ZTAUAER(5)
    P_TAUAERL(JLON,JLEV,12)=ZTAUAER(5)
    P_TAUAERL(JLON,JLEV,13)=ZTAUAER(5)
    P_TAUAERL(JLON,JLEV,14)=ZTAUAER(5)
    P_TAUAERL(JLON,JLEV,15)=ZTAUAER(5)
    P_TAUAERL(JLON,JLEV,16)=ZTAUAER(5)
  ENDDO
ENDDO

DO JLON=KIDIA,KFDIA
  DO J2=1,KLEV
    DO J1=1,JPXSEC
      P_WX(JLON,J1,J2)=0.0_JPRB
    ENDDO
  ENDDO
ENDDO

DO JLON=KIDIA,KFDIA
  DO JLEV = 1, KLEV
!- Set cross section molecule amounts from ECRT; convert to vmr
    P_WX(JLON,1,JLEV) = PCL4(JLON,KLEV-JLEV+1) * ZAMD/ZAMCL4
    P_WX(JLON,2,JLEV) = PC11(JLON,KLEV-JLEV+1) * ZAMD/ZAMC11
    P_WX(JLON,3,JLEV) = PC12(JLON,KLEV-JLEV+1) * ZAMD/ZAMC12
    P_WX(JLON,4,JLEV) = PC22(JLON,KLEV-JLEV+1) * ZAMD/ZAMC22
    P_WX(JLON,1,JLEV) = P_COLDRY(JLON,JLEV) * P_WX(JLON,1,JLEV) * 1.E-20_JPRB
    P_WX(JLON,2,JLEV) = P_COLDRY(JLON,JLEV) * P_WX(JLON,2,JLEV) * 1.E-20_JPRB
    P_WX(JLON,3,JLEV) = P_COLDRY(JLON,JLEV) * P_WX(JLON,3,JLEV) * 1.E-20_JPRB
    P_WX(JLON,4,JLEV) = P_COLDRY(JLON,JLEV) * P_WX(JLON,4,JLEV) * 1.E-20_JPRB

!- Here, all molecules in WKL and WX are in volume mixing ratio; convert to
!  molec/cm2 based on COLDRY for use in RRTM

!CDIR UNROLL=I_NMOL
    DO IMOL = 1, I_NMOL
      P_WKL(JLON,IMOL,JLEV) = P_COLDRY(JLON,JLEV) * P_WKL(JLON,IMOL,JLEV)
    ENDDO  
  
! DO IX = 1,JPXSEC
! IF (IXINDX(IX)  /=  0) THEN
!     WX(IXINDX(IX),L) = COLDRY(L) * WX(IX,L) * 1.E-20_JPRB
! ENDIF
! ENDDO  

  ENDDO
ENDDO

!- Approximate treatment for various cloud overlaps
DO JLON=KIDIA,KFDIA
  ZCLEAR(JLON)=1.0_JPRB
  ZCLOUD(JLON)=0.0_JPRB
  ZC1J(JLON,0)=0.0_JPRB
  ZEPSEC=1.E-03_JPRB
ENDDO

IF (NOVLP == 1) THEN

  DO JLON=KIDIA,KFDIA
    JL=JLON
    DO JK=1,KLEV
      IF (PCLDF(JL,JK) > ZEPSEC) THEN
        ZCLDLY=PCLDF(JL,JK)
        ZCLEAR(JLON)=ZCLEAR(JLON) &
       & *(1.0_JPRB-MAX( ZCLDLY , ZCLOUD(JLON) ))&
       & /(1.0_JPRB-MIN( ZCLOUD(JLON) , 1.0_JPRB-ZEPSEC ))  
        ZCLOUD(JLON) = ZCLDLY
        ZC1J(JLON,JK)= 1.0_JPRB - ZCLEAR(JLON)
      ELSE
        ZCLDLY=0.0_JPRB
        ZCLEAR(JLON)=ZCLEAR(JLON) &
       & *(1.0_JPRB-MAX( ZCLDLY , ZCLOUD(JLON) ))&
       & /(1.0_JPRB-MIN( ZCLOUD(JLON) , 1.0_JPRB-ZEPSEC ))  
        ZCLOUD(JLON) = ZCLDLY
        ZC1J(JLON,JK)= 1.0_JPRB - ZCLEAR(JLON)
      ENDIF
    ENDDO
  ENDDO

ELSEIF (NOVLP == 2) THEN

  DO JLON=KIDIA,KFDIA
    JL=JLON
    DO JK=1,KLEV
      IF (PCLDF(JL,JK) > ZEPSEC) THEN
        ZCLDLY=PCLDF(JL,JK)
        ZCLOUD(JLON) = MAX( ZCLDLY , ZCLOUD(JLON) )
        ZC1J(JLON,JK) = ZCLOUD(JLON)
      ELSE
        ZCLDLY=0.0_JPRB
        ZCLOUD(JLON) = MAX( ZCLDLY , ZCLOUD(JLON) )
        ZC1J(JLON,JK) = ZCLOUD(JLON)
      ENDIF
    ENDDO
  ENDDO

ELSEIF (NOVLP == 3) THEN

  DO JLON=KIDIA,KFDIA
    JL=JLON
    DO JK=1,KLEV
      IF (PCLDF(JL,JK) > ZEPSEC) THEN
        ZCLDLY=PCLDF(JL,JK)
        ZCLEAR(JLON) = ZCLEAR(JLON) * (1.0_JPRB-ZCLDLY)
        ZCLOUD(JLON) = 1.0_JPRB - ZCLEAR(JLON)
        ZC1J(JLON,JK) = ZCLOUD(JLON)
      ELSE
        ZCLDLY=0.0_JPRB
        ZCLEAR(JLON) = ZCLEAR(JLON) * (1.0_JPRB-ZCLDLY)
        ZCLOUD(JLON) = 1.0_JPRB - ZCLEAR(JLON)
        ZC1J(JLON,JK) = ZCLOUD(JLON)
      ENDIF
    ENDDO
  ENDDO

ELSEIF (NOVLP == 4) THEN

ENDIF

DO JLON=KIDIA,KFDIA
  PTCLEAR(JLON)=1.0_JPRB-ZC1J(JLON,KLEV)

! Transfer cloud fraction and cloud optical depth to RRTM arrays; 
! invert array index for PCLDF to go from bottom to top for RRTM

!- clear-sky column
  IF (PTCLEAR(JLON)  >  1.0_JPRB-ZEPSEC) THEN
    KCLD(JLON)=0
    DO JLEV = 1, KLEV
      P_CLDFRAC(JLON,JLEV) = 0.0_JPRB
!CDIR UNROLL=JPBAND
      DO JB=1,JPBAND
        P_TAUCLD(JLON,JLEV,JB) = 0.0_JPRB
      ENDDO
    ENDDO

  ELSE

!- cloudy column
!   The diffusivity factor (Savijarvi, 1997) on the cloud optical 
!   thickness TAUCLD has already been applied in RADLSW

    KCLD(JLON)=1
    DO JLEV=1,KLEV
      P_CLDFRAC(JLON,JLEV) = PCLDF(JLON,JLEV)
!CDIR UNROLL=JPBAND
      DO JB=1,JPBAND
        P_TAUCLD(JLON,JLEV,JB) = PTAUCLD(JLON,JLEV,JB)
      ENDDO
    ENDDO

  ENDIF
ENDDO

!     ------------------------------------------------------------------

END SUBROUTINE RRTM_ECRT_140GP
