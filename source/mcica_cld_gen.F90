SUBROUTINE MCICA_CLD_GEN &
 &( PNUAGE    , PLIQWCIN, PICEWCIN, PRLC_CF, PRLC_CW, PGLAT , PGLON ,&
 &  PSIGMA_QCW, PT, PETA, PSUR, KLON, KIDIA, KFDIA, KLEV, KNX_LOC, &
 &  LDPPH     , LDMAXRAN  , &
 &  KNCLDY    , PLIQWCIN_S, PICEWCIN_S)

! from Barker, Cole and Raisanen

! JJMorcrette 20060115  adapted to ECMWF system by 

! --------------------------------------------------------------------
! Driver to call stochastic cloud generator and produce subcolumns of
! cloud liquid and ice water contents that can be used by the McICA 
! radiative transfer routine.
! --------------------------------------------------------------------   

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

!EXTERNAL McICA_CLD_GENERATOR

! Note: KLEV    => Number of layers
! Note: KNX_LOC => Number of subcolumns to generate


! PARAMETER


REAL(KIND=JPRB), PARAMETER :: &
 & M2KM = 1.0_JPRB/1000.0_JPRB, & ! Convert meters to kilometers
 & CUT  = 0.001_JPRB          , & ! Cutoff for cloud amount    
 & GRAV = 9.80665_JPRB        , & ! Gravitational acceleration,  (M s-2)
 & RGASD = 287.05_JPRB            ! Gas constant for dry air     (J K-1 kg-1)

! KNX_LOC, LDPPH, LDMAXRAN should be passed in somehow not defined as PARAMETERS

!-- all the following are now passed as arguments
!================================================
!INTEGER(KIND=JPIM), PARAMETER :: &
! & KNX_LOC = 150       ! Number of subcolumns to generate

!LOGICAL, PARAMETER :: &
! & LDPPH    = .TRUE., &
! & LDMAXRAN = .TRUE.

!-----------------------------------------------------------

!     
! INPUT DATA

INTEGER(KIND=JPIM), INTENT(IN) :: KNX_LOC
LOGICAL, INTENT(IN) :: LDPPH, LDMAXRAN


REAL(KIND=JPRB), INTENT(IN) :: &
  & PNUAGE(KLON,KLEV),     & ! Column cloud fraction
  & PICEWCIN(KLON,KLEV),   & ! Column in-cloud ice water mixing ratio profile    (kg/kg)
  & PLIQWCIN(KLON,KLEV),   & ! Column in-cloud liquid water mixing ratio profile (kg/kg)
  & PGLAT(KLON),          & ! Latitude
  & PGLON(KLON)             ! Longitude

INTEGER(KIND=JPIM), INTENT(IN) :: & ! Counter and array sizes
  & KLON,                 & ! Length of latitude band   
  & KIDIA,                & ! Starting index for latitude
  & KFDIA,                & ! Ending point for latitude
  & KLEV                    ! Number of layers

REAL(KIND=JPRB), INTENT(IN) :: &
  & PRLC_CF(KLON,KLEV),     & ! Cloud fraction decorrelation length               (km)
  & PRLC_CW(KLON,KLEV),     & ! Cloud condensate decorrelation length             (km)
  & PSIGMA_QCW(KLON,KLEV),  & ! Normalized standard deviation of cloud condensate (unitless)      
  & PT(KLON,KLEV),          & ! Column temperature at layer midpoint              (K)
  & PSUR(KLON),             & ! Surface pressure                                  (Pa)
  & PETA(KLON,KLEV)            ! Column hybrid coord at layer midpoint             (unitless)

! OUTPUT DATA


REAL(KIND=JPRB), INTENT(OUT) :: & ! Subcolumns
  & PICEWCIN_S(KLON,KLEV,KNX_LOC), & ! Column ice water mixing ratio profile          (g/m^3)
  & PLIQWCIN_S(KLON,KLEV,KNX_LOC)    ! Column liquid water mixing ratio profile       (g/m^3) 

INTEGER(KIND=JPIM),INTENT(OUT) :: &
  & KNCLDY(KLON)                 ! Number of cloudy subcolumns


! LOCAL DATA


INTEGER(KIND=JPIM) :: &
  & JL,     & ! Counter over GCM columns
  & II,     & ! Counter over NX subcolumns
  & JK        ! Counter over lay vertical layers

REAL(KIND=JPRB) :: &
  & ZRHO      ! Density of air                                 (g/m^3)

REAL(KIND=JPRB) :: &
  & ZPLAY,  & ! Pressure at layer midpoint                     (Pa)
  & ZROG      ! Dry air gas constant/gravity (RGASD/GRAV)

REAL(KIND=JPRB) :: &
  & ZQI_PROF(KLON,KLEV), & ! Temporary array to convert ice water content     (kg/kg) -> (g/m^3)
  & ZQC_PROF(KLON,KLEV), & ! Temporary array to convert liquid water content  (kg/kg) -> (g/m^3)
  & ZM(KLON,KLEV)     , & ! Mid-layer height                                 (m)
  & ZDMULT(KLON,KLEV)

INTEGER(KIND=JPIM) :: &
  & IND1      ! Random index


!---------------------------------------------------------

! Zero out fields
DO JL = KIDIA,KFDIA
  KNCLDY(JL) = 0
ENDDO ! JL
      
DO JK = 1 , KLEV 
  DO JL = KIDIA,KFDIA
    ZQC_PROF(JL,JK) = 0.0_JPRB
    ZQI_PROF(JL,JK) = 0.0_JPRB
  ENDDO ! JL
ENDDO ! JK

DO II = 1, KNX_LOC
  DO JK = 1, KLEV
    DO JL = KIDIA,KFDIA
      PICEWCIN_S(JL,JK,II) = 0.0_JPRB
      PLIQWCIN_S(JL,JK,II) = 0.0_JPRB
    ENDDO
  ENDDO
ENDDO

! Compute the heights of mid-layers

ZROG=RGASD/GRAV    

DO JK = 1, KLEV
  DO JL = KIDIA, KFDIA
    ZPLAY = PETA(JL,JK)*PSUR(JL)
    ZM(JL,JK) = ZROG*PT(JL,JK)*LOG(PSUR(JL)/ZPLAY)*M2KM
  ENDDO
ENDDO

! Convert the cloud condensate from kg/kg to g/m^3 (is the cloud condensate cloud or domain mean)?
DO JK = 1, KLEV
  DO JL = KIDIA, KFDIA
    ZPLAY = PETA(JL,JK)*PSUR(JL)
! Compute layer height               
    IF (PNUAGE(JL,JK) > CUT) THEN
! ZRHO in kg/m^3                  
      ZRHO            = 1000.0_JPRB * ZPLAY/(RGASD*PT(JL,JK))
      ZDMULT(JL,JK)   = ZRHO !/PNUAGE(JL,JK) ! Divide by cloud amount if not provided in-cloud water contents
      ZQI_PROF(JL,JK) = PICEWCIN(JL,JK)*ZDMULT(JL,JK)
      ZQC_PROF(JL,JK) = PLIQWCIN(JL,JK)*ZDMULT(JL,JK)
    ENDIF
  ENDDO  ! JL
ENDDO ! JK
         
! Call cloud generator

CALL MCICA_CLD_GENERATOR &
  &( ZM, PNUAGE, ZQC_PROF, ZQI_PROF, PGLAT, PGLON, &
  &  PRLC_CF, PRLC_CW, PSIGMA_QCW,   &
  &  KLON, KIDIA, KFDIA, KLEV, KNX_LOC,  &
  &  LDPPH, LDMAXRAN, &
  &  PICEWCIN_S, PLIQWCIN_S, KNCLDY)

! Note that all of the cloudy subcolumns in the arrays PICEWCIN_S and PLIQWCIN_S 
! should be at the "front" of the arrays and the array KNCLDY contains the
! number of cloud subcolumns


!-- put back the cloud water in same unit as inputs!
DO JK= 1, KLEV
  DO JL= KIDIA, KFDIA
    IF (PNUAGE(JL,JK) > CUT) THEN
      DO II= 1, KNX_LOC
        PICEWCIN_S(JL,JK,II)=PICEWCIN_S(JL,JK,II)/ZDMULT(JL,JK)
        PLIQWCIN_S(JL,JK,II)=PLIQWCIN_S(JL,JK,II)/ZDMULT(JL,JK)
      ENDDO
    ENDIF
  ENDDO
ENDDO 

!---------------------------------------------------------
END SUBROUTINE MCICA_CLD_GEN

