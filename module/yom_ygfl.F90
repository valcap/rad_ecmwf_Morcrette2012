MODULE YOM_YGFL

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE TYPE_GFLS ,ONLY : TYPE_GFLD,TYPE_GFL_COMP,TYPE_GFL_NAML

IMPLICIT NONE
SAVE

!-------------------------------------------------------------------------
! Contains the descriptors of GFL arrays 

! YGFL  : GFL general descriptor, for info about content see comments
!         in type declaration module : type_gfls.F90

! JPGFL : Max number of GFL fields
! JPNAMED_GFL : Number of currently pre-defined components of GFL
! JPGHG : Number of greenhouse gas fields
! JPGRG : Number of reactive gas fields
! JPAERO : Number of active aerosol fields
! JPAEROUT: Number of output aerosol fields
! JPUVP : Number of output from UV processor
! JPTRAC : Number of tracers for diagnostics
! JPERA40 : Number of ERA40 diagnostic fields
! JPCH4S  : Number of added fields related to methane 
! JPNOGW  : Number of diagnostic fields for NORO GWD SCHEME
! JPSLDIA : Number of SL dynamics diagnostic fields
!-------------------------------------------------------------------------

INTEGER(KIND=JPIM),PARAMETER :: JPGFL=237
INTEGER(KIND=JPIM),PARAMETER :: JPNAMED_GFL=23
INTEGER(KIND=JPIM),PARAMETER :: JPGHG=3
INTEGER(KIND=JPIM),PARAMETER :: JPTRAC=2
INTEGER(KIND=JPIM),PARAMETER :: JPGRG=5       ! naj data assimilation code still assumes JPGRG=5 , to be replaced with JPGRGVAR
INTEGER(KIND=JPIM),PARAMETER :: JPGRGVAR=5
INTEGER(KIND=JPIM),PARAMETER :: JPAERO=15
INTEGER(KIND=JPIM),PARAMETER :: JPFORC=25
INTEGER(KIND=JPIM),PARAMETER :: JPERA40=14
INTEGER(KIND=JPIM),PARAMETER :: JPSLDIA=7
INTEGER(KIND=JPIM),PARAMETER :: JPEZDIAG=25
INTEGER(KIND=JPIM),PARAMETER :: JPCH4S=2
INTEGER(KIND=JPIM),PARAMETER :: JPNOGW=2
INTEGER(KIND=JPIM),PARAMETER :: JPAEROUT=8
INTEGER(KIND=JPIM),PARAMETER :: JPUVP=2
INTEGER(KIND=JPIM) :: NGFL_EXT
INTEGER(KIND=JPIM) :: NGFL_FORC
INTEGER(KIND=JPIM) :: NGFL_EZDIAG
INTEGER(KIND=JPIM) :: NGHG
INTEGER(KIND=JPIM) :: NTRAC
INTEGER(KIND=JPIM) :: NGRG, NGRG_CPLO4, NGRG_ASSIM
INTEGER(KIND=JPIM) :: NAERO  , NACTAERO, NDDHAERO,NERA40, NNOGW, NAEROUT, NUVP
INTEGER(KIND=JPIM) :: NSLDIA, NSLDIAGP
LOGICAL ::            LCO2SFC, LCH4, LAEROSFC, LSF6SFC , LFIRE, LAERODIU, LGRGSFC , LTRCMFIX
LOGICAL ::            LAEROUT, LUVPOUT

TYPE(TYPE_GFLD) :: YGFL
TYPE(TYPE_GFL_COMP),TARGET  :: YGFLC(JPGFL)  ! General descriptor of all components

TYPE(TYPE_GFL_COMP),POINTER  :: YQ            ! Specific humidity
TYPE(TYPE_GFL_COMP),POINTER  :: YI            ! Ice water
TYPE(TYPE_GFL_COMP),POINTER  :: YL            ! Liquid water
TYPE(TYPE_GFL_COMP),POINTER  :: YS            ! Snow
TYPE(TYPE_GFL_COMP),POINTER  :: YR            ! Rain
TYPE(TYPE_GFL_COMP),POINTER  :: YG            ! Graupel
TYPE(TYPE_GFL_COMP),POINTER  :: YH            ! Hail
TYPE(TYPE_GFL_COMP),POINTER  :: YTKE          ! Turbulent Kinetic Energy
TYPE(TYPE_GFL_COMP),POINTER  :: YA            ! Cloud fraction
TYPE(TYPE_GFL_COMP),POINTER  :: YO3           ! Ozone
TYPE(TYPE_GFL_COMP),POINTER  :: YSRC          ! Second-order flux for AROME
                                              ! s'rc'/2Sigma_s2 
                                              ! multiplied by Lambda_3
TYPE(TYPE_GFL_COMP),POINTER  :: YCPF          ! Convective precipitation flux
TYPE(TYPE_GFL_COMP),POINTER  :: YSPF          ! Stratiform precipitation flux
TYPE(TYPE_GFL_COMP),POINTER  :: YCVGQ         ! Moisture Convergence for french physics
TYPE(TYPE_GFL_COMP),POINTER  :: YQVA          ! total humidity variation
TYPE(TYPE_GFL_COMP),POINTER  :: YGHG(:)       ! Greenhouse Gases
TYPE(TYPE_GFL_COMP),POINTER  :: YGRG(:)       ! Reactive Gases
TYPE(TYPE_GFL_COMP),POINTER  :: YGRGTEND(:)       ! Reactive Gases Tendecies
TYPE(TYPE_GFL_COMP),POINTER  :: YAERO(:)      ! Aerosols
TYPE(TYPE_GFL_COMP),POINTER  :: YTRAC(:)      ! tracers for diagnostics
TYPE(TYPE_GFL_COMP),POINTER  :: YLRCH4        ! CH4 loss rate (instantaneous field)
TYPE(TYPE_GFL_COMP),POINTER  :: YCH4S         ! CH4 atmospheric sink (accumulated field)
TYPE(TYPE_GFL_COMP),POINTER  :: YFORC(:)      ! large scale forcing
TYPE(TYPE_GFL_COMP),POINTER  :: YEZDIAG(:)    ! easy diagnostics
TYPE(TYPE_GFL_COMP),POINTER  :: YERA40(:)      ! ERA40 diagnostic fields
TYPE(TYPE_GFL_COMP),POINTER  :: YNOGW(:)      ! NORO GWD SCHEME
TYPE(TYPE_GFL_COMP),POINTER  :: YSLDIA(:)      ! SL dynamics diagnostics
TYPE(TYPE_GFL_COMP),POINTER  :: YAEROUT(:)    ! Aerosol outputs
TYPE(TYPE_GFL_COMP),POINTER  :: YUVP(:)       ! UV-processor output

TYPE(TYPE_GFL_COMP),POINTER  :: YSDSAT        ! Standard Deviation of the
                                              ! SATuration Depression (Sigma_s) 
TYPE(TYPE_GFL_COMP),POINTER  :: YCVV          ! Convective Vertical Velocity 

! Prognostic convection variables: add 6 named components
TYPE(TYPE_GFL_COMP),POINTER  :: YUOM          ! Updraught vert velocity
TYPE(TYPE_GFL_COMP),POINTER  :: YUAL          ! Updraught mesh fraction
TYPE(TYPE_GFL_COMP),POINTER  :: YDOM          ! Downdraught vert velocity
TYPE(TYPE_GFL_COMP),POINTER  :: YDAL          ! Downdraught mesh fraction
TYPE(TYPE_GFL_COMP),POINTER  :: YUEN          ! Updraught entrainment
TYPE(TYPE_GFL_COMP),POINTER  :: YUNEBH        ! pseudo-historic convective

! Extra fields 

TYPE(TYPE_GFL_COMP),POINTER  :: YEXT(:)       ! Extra fields

TYPE(TYPE_GFL_NAML)  :: YQ_NL            ! Specific humidity
TYPE(TYPE_GFL_NAML)  :: YI_NL            ! Ice water
TYPE(TYPE_GFL_NAML)  :: YL_NL            ! Liquid water
TYPE(TYPE_GFL_NAML)  :: YS_NL            ! Snow
TYPE(TYPE_GFL_NAML)  :: YR_NL            ! Rain
TYPE(TYPE_GFL_NAML)  :: YG_NL            ! Graupels
TYPE(TYPE_GFL_NAML)  :: YH_NL            ! Hail
TYPE(TYPE_GFL_NAML)  :: YTKE_NL          ! Turbulent Kinetic Energy
TYPE(TYPE_GFL_NAML)  :: YA_NL            ! Cloud fraction
TYPE(TYPE_GFL_NAML)  :: YO3_NL           ! Ozone
TYPE(TYPE_GFL_NAML)  :: YSRC_NL          ! Second-order flux for AROME
                                         ! s'rc'/2Sigma_s2
                                         ! multiplied by Lambda_3
TYPE(TYPE_GFL_NAML)  :: YCPF_NL          ! Convective precipitation flux
TYPE(TYPE_GFL_NAML)  :: YSPF_NL          ! Stratiform precipitation flux
TYPE(TYPE_GFL_NAML)  :: YCVGQ_NL         ! Moisture Convergence for french physics
TYPE(TYPE_GFL_NAML)  :: YQVA_NL          ! Total humidity variation

TYPE(TYPE_GFL_NAML)  :: YGHG_NL(JPGHG)   ! Greenhouse Gases
TYPE(TYPE_GFL_NAML)  :: YGRG_NL(JPGRG)   ! Reactive Gases
TYPE(TYPE_GFL_NAML)  :: YGRGTEND_NL(JPGRG)   ! Reactive Gases Tendecies
TYPE(TYPE_GFL_NAML)  :: YAERO_NL(JPAERO) ! Aerosol fields
TYPE(TYPE_GFL_NAML)  :: YTRAC_NL(JPTRAC)   ! Tracers for diagnostics
TYPE(TYPE_GFL_NAML)  :: YERA40_NL(JPERA40)      ! ERA40 diagnostic fields
TYPE(TYPE_GFL_NAML)  :: YNOGW_NL(JPNOGW)    ! NORO GWD SCHEME
TYPE(TYPE_GFL_NAML)  :: YSLDIA_NL(JPSLDIA)  ! SL dynamics diagnostics
TYPE(TYPE_GFL_NAML)  :: YLRCH4_NL           ! CH4 loss rate
TYPE(TYPE_GFL_NAML)  :: YCH4S_NL            ! CH4 atmospheric sink
TYPE(TYPE_GFL_NAML)  :: YAEROUT_NL(JPAEROUT)! Aerosol outputs
TYPE(TYPE_GFL_NAML)  :: YUVP_NL(JPUVP)      ! UV-processor outputs

! Extra fields 

TYPE(TYPE_GFL_NAML)  :: YSDSAT_NL        ! Standard Deviation of the
                                         ! SATuration Depression (Sigma_s) 
TYPE(TYPE_GFL_NAML)  :: YCVV_NL          ! Convective Vertical Velocity 
TYPE(TYPE_GFL_NAML)  :: YFORC_NL(JPFORC) ! Forcing precursor
TYPE(TYPE_GFL_NAML)  :: YEZDIAG_NL(JPEZDIAG) ! Easy diagnostics
TYPE(TYPE_GFL_NAML)  :: YEXT_NL(JPGFL-JPNAMED_GFL-JPGHG-JPGRG-JPFORC-JPEZDIAG-JPAERO-JPTRAC-JPERA40-&
 &JPNOGW-JPSLDIA-JPCH4S-JPAEROUT-JPUVP) ! Extra fields

! Prognostic convection variables: 6 more namelist components
TYPE(TYPE_GFL_NAML)  :: YUOM_NL          ! Updraught vert velocity
TYPE(TYPE_GFL_NAML)  :: YUAL_NL          ! Updraught mesh fraction
TYPE(TYPE_GFL_NAML)  :: YDOM_NL          ! Downdraught vert velocity
TYPE(TYPE_GFL_NAML)  :: YDAL_NL          ! Downdraught mesh fraction
TYPE(TYPE_GFL_NAML)  :: YUEN_NL          ! Updraught entrainment
TYPE(TYPE_GFL_NAML)  :: YUNEBH_NL        ! Pseudi Hist Conv cloud fraction

!------------------------------------------------------------------
END MODULE YOM_YGFL
