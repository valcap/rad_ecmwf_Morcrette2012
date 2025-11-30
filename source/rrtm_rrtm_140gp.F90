!***************************************************************************
!                                                                          *
!                RRTM :  RAPID RADIATIVE TRANSFER MODEL                    *
!                                                                          *
!             ATMOSPHERIC AND ENVIRONMENTAL RESEARCH, INC.                 *
!                        840 MEMORIAL DRIVE                                *
!                        CAMBRIDGE, MA 02139                               *
!                                                                          *
!                           ELI J. MLAWER                                  *
!                         STEVEN J. TAUBMAN~                               *
!                         SHEPARD A. CLOUGH                                *
!                                                                          *
!                        ~currently at GFDL                                *
!                                                                          *
!                       email:  mlawer@aer.com                             *
!                                                                          *
!        The authors wish to acknowledge the contributions of the          *
!        following people:  Patrick D. Brown, Michael J. Iacono,           *
!        Ronald E. Farren, Luke Chen, Robert Bergstrom.                    *
!                                                                          *
!***************************************************************************
!     Reformatted for F90 by JJMorcrette, ECMWF, 980714                    * 
!                                                                          *
!***************************************************************************
! *** mji ***
! *** This version of RRTM has been altered to interface with either
!     the ECMWF numerical weather prediction model or the ECMWF column 
!     radiation model (ECRT) package. 

!     Revised, April, 1997;  Michael J. Iacono, AER, Inc.
!          - initial implementation of RRTM in ECRT code
!     Revised, June, 1999;  Michael J. Iacono and Eli J. Mlawer, AER, Inc.
!          - to implement generalized maximum/random cloud overlap
!        NEC           25-Oct-2007 Optimisations
!        D. Salmond    11-Dec-2007 Optimizations

SUBROUTINE RRTM_RRTM_140GP &
 & ( KIDIA , KFDIA , KLON , KLEV,&
 & PAER  , PAPH  , PAP,&
 & PTS   , PTH   , PT,&
 & P_ZEMIS , P_ZEMIW,&
 & PQ    , PCO2  , PCH4, PN2O, PNO2, PC11, PC12, PC22, PCL4, POZN,&
 & PCLDF , PTAUCLD,&
 & PEMIT , PFLUX , PFLUC, PTCLEAR &
 & )  

! *** This program is the driver for RRTM, the AER rapid model.  
!     For each atmosphere the user wishes to analyze, this routine
!     a) calls ECRTATM to read in the atmospheric profile 
!     b) calls SETCOEF to calculate various quantities needed for 
!        the radiative transfer algorithm
!     c) calls RTRN to do the radiative transfer calculation for
!        clear or cloudy sky
!     d) writes out the upward, downward, and net flux for each
!        level and the heating rate for each layer

!     JJMorcrette 20080424 3D fields of CO2, CH4, N2O, NO2, CFC11, 12, 22, and CCL4

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE PARRRTM  , ONLY : JPBAND   ,JPXSEC   ,JPGPT    ,JPLAY    ,&
 & JPINPX  
!------------------------------Arguments--------------------------------

! Input arguments

IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON! Number of atmospheres (longitudes) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV! Number of atmospheric layers 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA ! First atmosphere index
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA ! Last atmosphere index
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAER(KLON,6,KLEV) ! Aerosol optical thickness
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPH(KLON,KLEV+1) ! Interface pressures (Pa)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAP(KLON,KLEV) ! Layer pressures (Pa)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS(KLON) ! Surface temperature (JLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTH(KLON,KLEV+1) ! Interface temperatures (JLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) ! Layer temperature (JLEV)
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
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PEMIT(KLON) ! Surface LW emissivity
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PFLUX(KLON,2,KLEV+1) ! LW total sky flux (1=up, 2=down)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PFLUC(KLON,2,KLEV+1) ! LW clear sky flux (1=up, 2=down)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PTCLEAR(KLON) ! clear-sky fraction of column
INTEGER(KIND=JPIM) :: ICLDLYR(KIDIA:KFDIA,JPLAY)        ! Cloud indicator
REAL(KIND=JPRB) :: Z_CLDFRAC(KIDIA:KFDIA,JPLAY)           ! Cloud fraction
REAL(KIND=JPRB) :: Z_TAUCLD(KIDIA:KFDIA,JPLAY,JPBAND)     ! Spectral optical thickness

REAL(KIND=JPRB) :: Z_ATR1  (KIDIA:KFDIA,JPGPT,JPLAY)

REAL(KIND=JPRB) :: Z_OD    (JPGPT,JPLAY,KIDIA:KFDIA)

REAL(KIND=JPRB) :: Z_TF1   (KIDIA:KFDIA,JPGPT,JPLAY)

REAL(KIND=JPRB) :: Z_COLDRY(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_WKL(KIDIA:KFDIA,JPINPX,JPLAY)

REAL(KIND=JPRB) :: Z_WX(KIDIA:KFDIA,JPXSEC,JPLAY)         ! Amount of trace gases

REAL(KIND=JPRB) :: Z_CLFNET  (KIDIA:KFDIA,0:JPLAY)
REAL(KIND=JPRB) :: Z_CLHTR   (KIDIA:KFDIA,0:JPLAY)
REAL(KIND=JPRB) :: Z_FNET    (KIDIA:KFDIA,0:JPLAY)
REAL(KIND=JPRB) :: Z_HTR     (KIDIA:KFDIA,0:JPLAY)
REAL(KIND=JPRB) :: Z_TOTDFLUC(KIDIA:KFDIA,0:JPLAY)
REAL(KIND=JPRB) :: Z_TOTDFLUX(KIDIA:KFDIA,0:JPLAY)
REAL(KIND=JPRB) :: Z_TOTUFLUC(KIDIA:KFDIA,0:JPLAY)
REAL(KIND=JPRB) :: Z_TOTUFLUX(KIDIA:KFDIA,0:JPLAY)

INTEGER(KIND=JPIM) :: ICLD(KIDIA:KFDIA), JLON, JLEV, JI
INTEGER(KIND=JPIM) :: ISTART
INTEGER(KIND=JPIM) :: IEND

REAL(KIND=JPRB) :: Z_FLUXFAC, Z_HEATFAC, Z_PI, ZEPSEC, ZTCLEAR(KIDIA:KFDIA)

!- from AER
REAL(KIND=JPRB) :: Z_TAUAERL(KIDIA:KFDIA,JPLAY,JPBAND)

!- from INTFAC      
REAL(KIND=JPRB) :: Z_FAC00(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_FAC01(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_FAC10(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_FAC11(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_FORFAC(KIDIA:KFDIA,JPLAY)

!- from INTIND
INTEGER(KIND=JPIM) :: JP(KIDIA,KFDIA,JPLAY)
INTEGER(KIND=JPIM) :: JT(KIDIA:KFDIA,JPLAY)
INTEGER(KIND=JPIM) :: JT1(KIDIA:KFDIA,JPLAY)

!- from PRECISE             
REAL(KIND=JPRB) :: Z_ONEMINUS

!- from PROFDATA             
REAL(KIND=JPRB) :: Z_COLH2O(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_COLCO2(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_COLO3 (KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_COLN2O(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_COLCH4(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_COLO2 (KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_CO2MULT(KIDIA:KFDIA,JPLAY)
INTEGER(KIND=JPIM) :: I_LAYTROP(KIDIA:KFDIA)
INTEGER(KIND=JPIM) :: I_LAYSWTCH(KIDIA:KFDIA)
INTEGER(KIND=JPIM) :: I_LAYLOW(KIDIA:KFDIA)

!- from PROFILE             
REAL(KIND=JPRB) :: Z_PAVEL(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_TAVEL(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_PZ(KIDIA:KFDIA,0:JPLAY)
REAL(KIND=JPRB) :: Z_TZ(KIDIA:KFDIA,0:JPLAY)
REAL(KIND=JPRB) :: Z_TBOUND(KIDIA:KFDIA)
INTEGER(KIND=JPIM) :: I_NLAYERS(KIDIA:KFDIA)

!- from SELF             
REAL(KIND=JPRB) :: Z_SELFFAC(KIDIA:KFDIA,JPLAY)
REAL(KIND=JPRB) :: Z_SELFFRAC(KIDIA:KFDIA,JPLAY)
INTEGER(KIND=JPIM) :: INDSELF(KIDIA:KFDIA,JPLAY)

!- from SP             
REAL(KIND=JPRB) :: Z_PFRAC(KIDIA:KFDIA,JPGPT,JPLAY)

!- from SURFACE             
REAL(KIND=JPRB) :: Z_SEMISS(KIDIA:KFDIA,JPBAND)
REAL(KIND=JPRB) :: Z_SEMISLW(KIDIA:KFDIA)
INTEGER(KIND=JPIM) :: IREFLECT(KIDIA:KFDIA)


!     HEATFAC is the factor by which one must multiply delta-flux/ 
!     delta-pressure, with flux in w/m-2 and pressure in mbar, to get 
!     the heating rate in units of degrees/day.  It is equal to 
!           (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
!        =  (9.8066)(86400)(1e-5)/(1.004)

ZEPSEC = 1.E-06_JPRB
Z_ONEMINUS = 1.0_JPRB - ZEPSEC
Z_PI = 2.0_JPRB*ASIN(1.0_JPRB)
Z_FLUXFAC = Z_PI * 2.D4
Z_HEATFAC = 8.4391_JPRB

! *** mji ***
! For use with ECRT, this loop is over atmospheres (or longitudes)
DO JLON = KIDIA,KFDIA

! *** mji ***
!- Prepare atmospheric profile from ECRT for use in RRTM, and define
!  other RRTM input parameters.  Arrays are passed back through the
!  existing RRTM commons and arrays.
  ZTCLEAR(JLON)=1.0_JPRB
ENDDO

  CALL RRTM_ECRT_140GP &
   & (KLON,KIDIA,KFDIA, KLEV, ICLD,&
   & PAER , PAPH , PAP,&
   & PTS  , PTH  , PT,&
   & P_ZEMIS, P_ZEMIW,&
   & PQ   , PCO2, PCH4, PN2O, PNO2, PC11, PC12, PC22, PCL4, POZN, PCLDF, PTAUCLD, ZTCLEAR,&
   & Z_CLDFRAC,Z_TAUCLD,Z_COLDRY,Z_WKL,Z_WX,&
   & Z_TAUAERL,Z_PAVEL,Z_TAVEL,Z_PZ,Z_TZ,Z_TBOUND,I_NLAYERS,Z_SEMISS,IREFLECT)  

DO JLON = KIDIA,KFDIA
  PTCLEAR(JLON)=ZTCLEAR(JLON)
ENDDO

  ISTART = 1
  IEND   = 16

!  Calculate information needed by the radiative transfer routine
!  that is specific to this atmosphere, especially some of the 
!  coefficients and indices needed to compute the optical depths
!  by interpolating data from stored reference atmospheres. 

  CALL RRTM_SETCOEF_140GP (KIDIA,KFDIA,KLEV,Z_COLDRY,Z_WKL,&
   & Z_FAC00,Z_FAC01,Z_FAC10,Z_FAC11,Z_FORFAC,JP,JT,JT1,&
   & Z_COLH2O,Z_COLCO2,Z_COLO3,Z_COLN2O,Z_COLCH4,Z_COLO2,Z_CO2MULT,&
   & I_LAYTROP,I_LAYSWTCH,I_LAYLOW,Z_PAVEL,Z_TAVEL,Z_SELFFAC,Z_SELFFRAC,INDSELF)  

  CALL RRTM_GASABS1A_140GP (KIDIA,KFDIA,KLEV,Z_ATR1,Z_OD,Z_TF1,Z_COLDRY,Z_WX,&
   & Z_TAUAERL,Z_FAC00,Z_FAC01,Z_FAC10,Z_FAC11,Z_FORFAC,JP,JT,JT1,Z_ONEMINUS,&
   & Z_COLH2O,Z_COLCO2,Z_COLO3,Z_COLN2O,Z_COLCH4,Z_COLO2,Z_CO2MULT,&
   & I_LAYTROP,I_LAYSWTCH,I_LAYLOW,Z_SELFFAC,Z_SELFFRAC,INDSELF,Z_PFRAC)  

!- Call the radiative transfer routine.

! *** mji ***
!  Check for cloud in column.  Use ECRT threshold set as flag icld in
!  routine ECRTATM.  If icld=1 then column is cloudy, otherwise it is
!  clear.  Also, set up flag array, icldlyr, for use in radiative
!  transfer.  Set icldlyr to one for each layer with non-zero cloud
!  fraction.

DO JLON = KIDIA,KFDIA
  DO JLEV = 1, KLEV
    IF (ICLD(JLON) == 1.AND.Z_CLDFRAC(JLON,JLEV) > ZEPSEC) THEN
      ICLDLYR(JLON,JLEV) = 1
    ELSE
      ICLDLYR(JLON,JLEV) = 0
    ENDIF
  ENDDO
ENDDO

!  Clear and cloudy parts of column are treated together in RTRN.
!  Clear radiative transfer is done for clear layers and cloudy radiative
!  transfer is done for cloudy layers as identified by icldlyr.

  CALL RRTM_RTRN1A_140GP (KIDIA,KFDIA,KLEV,ISTART,IEND,ICLDLYR,Z_CLDFRAC,Z_TAUCLD,Z_ATR1,&
   & Z_OD,Z_TF1,Z_CLFNET,Z_CLHTR,Z_FNET,Z_HTR,Z_TOTDFLUC,Z_TOTDFLUX,Z_TOTUFLUC,Z_TOTUFLUX,&
   & Z_TAVEL,Z_PZ,Z_TZ,Z_TBOUND,Z_PFRAC,Z_SEMISS,Z_SEMISLW,IREFLECT)  

! ***   Pass clear sky and total sky up and down flux profiles to ECRT
!       output arrays (zflux, zfluc). Array indexing from bottom to top 
!       is preserved for ECRT.
!       Invert down flux arrays for consistency with ECRT sign conventions.

DO JLON = KIDIA,KFDIA
  PEMIT(JLON) = Z_SEMISLW(JLON)
ENDDO

DO JLON = KIDIA,KFDIA
  DO JI = 0, KLEV
    PFLUC(JLON,1,JI+1) =  Z_TOTUFLUC(JLON,JI)*Z_FLUXFAC
    PFLUC(JLON,2,JI+1) = -Z_TOTDFLUC(JLON,JI)*Z_FLUXFAC
    PFLUX(JLON,1,JI+1) =  Z_TOTUFLUX(JLON,JI)*Z_FLUXFAC
    PFLUX(JLON,2,JI+1) = -Z_TOTDFLUX(JLON,JI)*Z_FLUXFAC
  ENDDO
ENDDO

END SUBROUTINE RRTM_RRTM_140GP
