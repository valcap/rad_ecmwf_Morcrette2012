!!!#ifdef RS6K
!!!@PROCESS HOT NOSTRICT
!!!#endif
SUBROUTINE SRTM_SPCVRT_MCICA &
 & ( KIDIA   , KFDIA   , KLEV    , KMOL    , KSW    , KCOLS  , PONEMINUS, &
 &   PAVEL   , PTAVEL  , PZ     , PTZ    , PTBOUND  , PALBD   , PALBP, &
 &   PFRCL   , PTAUC   , PASYC  , POMGC  , PTAUA    , PASYA   , POMGA , PRMU0, &
 &   PCOLDRY , PWKL, &
 &   KLAYTROP, KLAYSWTCH, KLAYLOW ,&
 &   PCO2MULT, PCOLCH4  , PCOLCO2 , PCOLH2O , PCOLMOL  , PCOLN2O  , PCOLO2 , PCOLO3 ,&
 &   PFORFAC , PFORFRAC , KINDFOR , PSELFFAC, PSELFFRAC, KINDSELF ,&
 &   PFAC00  , PFAC01   , PFAC10  , PFAC11 ,&
 &   KJP     , KJT      , KJT1 ,&
 !-- output arrays
 &   PBBFD, PBBFU, PBBCD, PBBCU, PFUVF, PFUVC, PPARF, PPARCF, PSUDU )


!**** *SRTM_SPCVRT* - SPECTRAL LOOP TO COMPUTE THE SHORTWAVE RADIATION FLUXES.

!     PURPOSE.
!     --------

!          THIS ROUTINE COMPUTES THE TWO-STREAM METHOD OF BARKER

!**   INTERFACE.
!     ----------

!          *SRTM_SPCVRT_MCICA* IS CALLED FROM *SRTM_SRTM_224GP*

!        IMPLICIT ARGUMENTS :
!        --------------------

!     ==== INPUTS ===
!     ==== OUTPUTS ===

!     METHOD.
!     -------

!     EXTERNALS.
!     ----------

!          *SWVRTQDR*

!     REFERENCE.
!     ----------

!        SEE RADIATION'S PART OF THE ECMWF RESEARCH DEPARTMENT
!        DOCUMENTATION
!     AUTHOR.
!     -------
!        from Howard Barker
!        JEAN-JACQUES MORCRETTE  *ECMWF*

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 03-02-27
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        JJMorcrette   20050110 McICA version
!        JJMorcrette   20070614 bug-fix for solar duration
!        JJMorcrette   20070831 UV-B surface flux
!        D.Salmond  31-Oct-2007 Vector version in the style of RRTM from Meteo France & NEC
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE PARSRTM  , ONLY : JPLAY, JPB1, JPB2, JPGPT

USE YOESRTWN , ONLY : NGC
USE YOERDI   , ONLY : REPCLC

!USE YOERAD   , ONLY : NSW
!USE YOERDU   , ONLY : RCDAY
!USE YOESWN   , ONLY : NTBANDS, NBANDS, NGS, NUV, NVS, RWGT, NDBUG

IMPLICIT NONE

!     ------------------------------------------------------------------

!*       0.1   ARGUMENTS
!              ---------

INTEGER(KIND=JPIM),INTENT(IN)    :: KSW
INTEGER(KIND=JPIM),INTENT(IN)    :: KCOLS

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA, KFDIA
INTEGER(KIND=JPIM)               :: KLEV ! UNDETERMINED INTENT
INTEGER(KIND=JPIM)               :: KMOL ! Argument NOT used
!INTEGER(KIND=JPIM)               :: KPT

REAL(KIND=JPRB)                  :: PONEMINUS(KIDIA:KFDIA) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PAVEL(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: PTAVEL(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: PZ(KIDIA:KFDIA,0:JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: PTZ(KIDIA:KFDIA,0:JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: PTBOUND(KIDIA:KFDIA) ! Argument NOT used
REAL(KIND=JPRB)   ,INTENT(IN)    :: PALBD(KIDIA:KFDIA,KSW)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PALBP(KIDIA:KFDIA,KSW)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFRCL(KIDIA:KFDIA,KCOLS,JPLAY)  ! bottom to top
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTAUC(KIDIA:KFDIA,JPLAY,KCOLS)  ! bottom to top
REAL(KIND=JPRB)   ,INTENT(IN)    :: PASYC(KIDIA:KFDIA,JPLAY,KCOLS)  ! bottom to top
REAL(KIND=JPRB)   ,INTENT(IN)    :: POMGC(KIDIA:KFDIA,JPLAY,KCOLS)  ! bottom to top
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTAUA(KIDIA:KFDIA,JPLAY,KSW)    ! bottom to top
REAL(KIND=JPRB)   ,INTENT(IN)    :: PASYA(KIDIA:KFDIA,JPLAY,KSW)    ! bottom to top
REAL(KIND=JPRB)   ,INTENT(IN)    :: POMGA(KIDIA:KFDIA,JPLAY,KSW)    ! bottom to top
REAL(KIND=JPRB)                  :: PRMU0(KIDIA:KFDIA) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PCOLDRY(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: PWKL(KIDIA:KFDIA,35,JPLAY) ! Argument NOT used
INTEGER(KIND=JPIM)               :: KLAYTROP(KIDIA:KFDIA) ! UNDETERMINED INTENT
INTEGER(KIND=JPIM)               :: KLAYSWTCH(KIDIA:KFDIA) ! Argument NOT used
INTEGER(KIND=JPIM)               :: KLAYLOW(KIDIA:KFDIA) ! Argument NOT used
REAL(KIND=JPRB)                  :: PCO2MULT(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: PCOLCH4(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PCOLCO2(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PCOLH2O(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PCOLMOL(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PCOLN2O(KIDIA:KFDIA,JPLAY) ! Argument NOT used
REAL(KIND=JPRB)                  :: PCOLO2(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PCOLO3(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PFORFAC(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PFORFRAC(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
INTEGER(KIND=JPIM)               :: KINDFOR(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PSELFFAC(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PSELFFRAC(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
INTEGER(KIND=JPIM)               :: KINDSELF(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PFAC00(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PFAC01(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PFAC10(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)                  :: PFAC11(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
INTEGER(KIND=JPIM)               :: KJP(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
INTEGER(KIND=JPIM)               :: KJT(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
INTEGER(KIND=JPIM)               :: KJT1(KIDIA:KFDIA,JPLAY) ! UNDETERMINED INTENT
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PBBFD(KIDIA:KFDIA,JPLAY+1)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PBBFU(KIDIA:KFDIA,JPLAY+1)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PBBCD(KIDIA:KFDIA,JPLAY+1)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PBBCU(KIDIA:KFDIA,JPLAY+1)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PFUVF(KIDIA:KFDIA), PFUVC(KIDIA:KFDIA)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PPARF(KIDIA:KFDIA), PPARCF(KIDIA:KFDIA)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSUDU(KIDIA:KFDIA)

!     ------------------------------------------------------------------

!              ------------

LOGICAL :: LLRTCHK(KIDIA:KFDIA,JPLAY)

REAL(KIND=JPRB) :: &
 & ZCLEAR(KIDIA:KFDIA)      , ZCLOUD(KIDIA:KFDIA)       &
 & , ZDBT(KIDIA:KFDIA,JPLAY+1) &
 & , ZGCC(KIDIA:KFDIA,JPLAY)   , ZGCO(KIDIA:KFDIA,JPLAY)     &
 & , ZOMCC(KIDIA:KFDIA,JPLAY)  , ZOMCO(KIDIA:KFDIA,JPLAY)    &
 & , ZRDND(KIDIA:KFDIA,JPLAY+1), ZRDNDC(KIDIA:KFDIA,JPLAY+1)&
 & , ZREF(KIDIA:KFDIA,JPLAY+1) , ZREFC(KIDIA:KFDIA,JPLAY+1) , ZREFO(KIDIA:KFDIA,JPLAY+1)  &
 & , ZREFD(KIDIA:KFDIA,JPLAY+1), ZREFDC(KIDIA:KFDIA,JPLAY+1), ZREFDO(KIDIA:KFDIA,JPLAY+1) &
 & , ZRUP(KIDIA:KFDIA,JPLAY+1) , ZRUPD(KIDIA:KFDIA,JPLAY+1) &
 & , ZRUPC(KIDIA:KFDIA,JPLAY+1), ZRUPDC(KIDIA:KFDIA,JPLAY+1)&
 & , ZTAUC(KIDIA:KFDIA,JPLAY)  , ZTAUO(KIDIA:KFDIA,JPLAY)    &
 & , ZTDBT(KIDIA:KFDIA,JPLAY+1) &
 & , ZTRA(KIDIA:KFDIA,JPLAY+1) , ZTRAC(KIDIA:KFDIA,JPLAY+1) , ZTRAO(KIDIA:KFDIA,JPLAY+1)  &
 & , ZTRAD(KIDIA:KFDIA,JPLAY+1), ZTRADC(KIDIA:KFDIA,JPLAY+1), ZTRADO(KIDIA:KFDIA,JPLAY+1)
REAL(KIND=JPRB) :: &
 & ZDBTC(KIDIA:KFDIA,JPLAY+1), ZTDBTC(KIDIA:KFDIA,JPLAY+1), ZINCFLX(KIDIA:KFDIA,JPGPT)  &
 & ,  ZINCF14(KIDIA:KFDIA,14)   , ZINCTOT(KIDIA:KFDIA)

INTEGER(KIND=JPIM) :: IB1, IB2, IBM, IGT, IKL, IW(KIDIA:KFDIA), JB, JG, JK, I_KMODTS, JL, IC, ICOUNT
INTEGER(KIND=JPIM) :: INDEX(1:2000)

REAL(KIND=JPRB) :: ZARG1(KIDIA:KFDIA), ZARG2(KIDIA:KFDIA), ZDBTMC(KIDIA:KFDIA), ZDBTMO(KIDIA:KFDIA), ZF(KIDIA:KFDIA)
REAL(KIND=JPRB) :: ZINCFLUX(KIDIA:KFDIA), ZWF(KIDIA:KFDIA)
REAL(KIND=JPRB) :: ZCOEFVS

!-- Output of SRTM_TAUMOLn routines

REAL(KIND=JPRB) :: ZTAUG(KIDIA:KFDIA,JPLAY,16), ZTAUR(KIDIA:KFDIA,JPLAY,16), ZSFLXZEN(KIDIA:KFDIA,16)

!-- Output of SRTM_VRTQDR routine
REAL(KIND=JPRB) :: &
 & ZCD(KIDIA:KFDIA,JPLAY+1,JPGPT), ZCU(KIDIA:KFDIA,JPLAY+1,JPGPT) &
 & ,  ZFD(KIDIA:KFDIA,JPLAY+1,JPGPT), ZFU(KIDIA:KFDIA,JPLAY+1,JPGPT)


!     ------------------------------------------------------------------

!-- Two-stream model 1: Eddington, 2: PIFM, Zdunkowski et al., 3: discret ordinates

IB1=JPB1
IB2=JPB2

IC=0
DO JL = KIDIA, KFDIA
  IF (PRMU0(JL) > 0.0_JPRB) THEN
    IC=IC+1
    INDEX(IC)=JL
    IW(JL)=0
    ZINCFLUX(JL)=0.0_JPRB
    ZINCTOT(JL)=0.0_JPRB
    PFUVF(JL) = 0.0_JPRB
    PFUVC(JL) = 0.0_JPRB
    PPARF(JL) = 0.0_JPRB
    PPARCF(JL)= 0.0_JPRB
  ENDIF
ENDDO
ICOUNT=IC
IF(ICOUNT==0)THEN
  RETURN
ENDIF

!-- fraction of visible (to 0.69 um) in interval 0.6250-0.7782 um
ZCOEFVS = 0.42425_JPRB

JB=IB1-1
DO JB = IB1, IB2
  DO IC=1,ICOUNT
    JL=INDEX(IC)
    IBM = JB-15
    IGT = NGC(IBM)
    ZINCF14(JL,IBM)=0.0_JPRB
  ENDDO

  !-- for each band, computes the gaseous and Rayleigh optical thickness
  !  for all g-points within the band

  IF (JB == 16) THEN
    CALL SRTM_TAUMOL16 &
     & ( KIDIA   , KFDIA    , KLEV    ,&
     &   PFAC00  , PFAC01   , PFAC10   , PFAC11   ,&
     &   KJP     , KJT      , KJT1     , PONEMINUS,&
     &   PCOLH2O , PCOLCH4  , PCOLMOL  ,&
     &   KLAYTROP, PSELFFAC , PSELFFRAC, KINDSELF, PFORFAC  , PFORFRAC, KINDFOR ,&
     &   ZSFLXZEN, ZTAUG    , ZTAUR    , PRMU0     &
     & )

  ELSEIF (JB == 17) THEN
    CALL SRTM_TAUMOL17 &
     & ( KIDIA   , KFDIA   , KLEV    ,&
     &   PFAC00  , PFAC01  , PFAC10   , PFAC11 ,&
     &   KJP     , KJT     , KJT1     , PONEMINUS ,&
     &   PCOLH2O , PCOLCO2 , PCOLMOL  ,&
     &   KLAYTROP, PSELFFAC, PSELFFRAC, KINDSELF  , PFORFAC, PFORFRAC, KINDFOR ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR    , PRMU0     &
     & )

  ELSEIF (JB == 18) THEN
    CALL SRTM_TAUMOL18 &
     & ( KIDIA   , KFDIA   , KLEV    ,&
     &   PFAC00  , PFAC01  , PFAC10   , PFAC11 ,&
     &   KJP     , KJT     , KJT1     , PONEMINUS ,&
     &   PCOLH2O , PCOLCH4 , PCOLMOL  ,&
     &   KLAYTROP, PSELFFAC, PSELFFRAC, KINDSELF  , PFORFAC, PFORFRAC, KINDFOR ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR    , PRMU0     &
     & )

  ELSEIF (JB == 19) THEN
    CALL SRTM_TAUMOL19 &
     & ( KIDIA   , KFDIA   , KLEV    ,&
     &   PFAC00  , PFAC01  , PFAC10   , PFAC11 ,&
     &   KJP     , KJT     , KJT1     , PONEMINUS ,&
     &   PCOLH2O , PCOLCO2 , PCOLMOL  ,&
     &   KLAYTROP, PSELFFAC, PSELFFRAC, KINDSELF  , PFORFAC, PFORFRAC, KINDFOR ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR    , PRMU0     &
     & )

  ELSEIF (JB == 20) THEN
    CALL SRTM_TAUMOL20 &
     & ( KIDIA   , KFDIA   , KLEV    ,&
     &   PFAC00  , PFAC01  , PFAC10   , PFAC11 ,&
     &   KJP     , KJT     , KJT1     , PONEMINUS ,&
     &   PCOLH2O , PCOLCH4 , PCOLMOL  ,&
     &   KLAYTROP, PSELFFAC, PSELFFRAC, KINDSELF  , PFORFAC, PFORFRAC, KINDFOR ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR    , PRMU0     &
     & )

  ELSEIF (JB == 21) THEN
    CALL SRTM_TAUMOL21 &
     & ( KIDIA   , KFDIA   , KLEV    ,&
     &   PFAC00  , PFAC01  , PFAC10   , PFAC11 ,&
     &   KJP     , KJT     , KJT1     , PONEMINUS ,&
     &   PCOLH2O , PCOLCO2 , PCOLMOL  ,&
     &   KLAYTROP, PSELFFAC, PSELFFRAC, KINDSELF  , PFORFAC, PFORFRAC, KINDFOR ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR    , PRMU0     &
     & )

  ELSEIF (JB == 22) THEN
    CALL SRTM_TAUMOL22 &
     & ( KIDIA   , KFDIA   , KLEV    ,&
     &   PFAC00  , PFAC01  , PFAC10   , PFAC11 ,&
     &   KJP     , KJT     , KJT1     , PONEMINUS ,&
     &   PCOLH2O , PCOLMOL , PCOLO2   ,&
     &   KLAYTROP, PSELFFAC, PSELFFRAC, KINDSELF  , PFORFAC, PFORFRAC, KINDFOR ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR    , PRMU0     &
     & )

  ELSEIF (JB == 23) THEN
    CALL SRTM_TAUMOL23 &
     & ( KIDIA   , KFDIA   , KLEV    ,&
     &   PFAC00  , PFAC01  , PFAC10   , PFAC11 ,&
     &   KJP     , KJT     , KJT1     , PONEMINUS ,&
     &   PCOLH2O , PCOLMOL ,&
     &   KLAYTROP, PSELFFAC, PSELFFRAC, KINDSELF  , PFORFAC, PFORFRAC, KINDFOR ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR    , PRMU0     &
     & )

  ELSEIF (JB == 24) THEN
    CALL SRTM_TAUMOL24 &
     & ( KIDIA   , KFDIA   , KLEV    ,&
     &   PFAC00  , PFAC01  , PFAC10   , PFAC11 ,&
     &   KJP     , KJT     , KJT1     , PONEMINUS ,&
     &   PCOLH2O , PCOLMOL , PCOLO2   , PCOLO3 ,&
     &   KLAYTROP, PSELFFAC, PSELFFRAC, KINDSELF  , PFORFAC, PFORFRAC, KINDFOR ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR    , PRMU0     &
     & )

  ELSEIF (JB == 25) THEN
    !--- visible 16000-22650 cm-1   0.4415 - 0.6250 um
    CALL SRTM_TAUMOL25 &
     & ( KIDIA    , KFDIA   , KLEV     ,&
     &   PFAC00   , PFAC01  , PFAC10 , PFAC11 ,&
     &   KJP      , KJT     , KJT1   , PONEMINUS ,&
     &   PCOLH2O  , PCOLMOL , PCOLO3 ,&
     &   KLAYTROP ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR   , PRMU0     &
     & )

  ELSEIF (JB == 26) THEN
    !--- UV-A 22650-29000 cm-1   0.3448 - 0.4415 um
    CALL SRTM_TAUMOL26 &
     & ( KIDIA   , KFDIA   , KLEV    ,&
     &   PFAC00  , PFAC01  , PFAC10   , PFAC11 ,&
     &   KJP     , KJT     , KJT1     , PONEMINUS ,&
     &   PCOLH2O , PCOLCO2 , PCOLMOL  ,&
     &   KLAYTROP, PSELFFAC, PSELFFRAC, KINDSELF  , PFORFAC, PFORFRAC, KINDFOR ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR    , PRMU0     &
     & )

  ELSEIF (JB == 27) THEN
    !--- UV-B 29000-38000 cm-1   0.2632 - 0.3448 um
    CALL SRTM_TAUMOL27 &
     & ( KIDIA   , KFDIA   , KLEV    ,&
     &   PFAC00  , PFAC01  , PFAC10   , PFAC11 ,&
     &   KJP     , KJT     , KJT1     , PONEMINUS ,&
     &   PCOLMOL , PCOLO3 ,&
     &   KLAYTROP ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR    , PRMU0     &
     & )

  ELSEIF (JB == 28) THEN
    !--- UV-C 38000-50000 cm-1   0.2000 - 0.2632 um
    CALL SRTM_TAUMOL28 &
     & ( KIDIA   , KFDIA   , KLEV    ,&
     &   PFAC00  , PFAC01  , PFAC10 , PFAC11 ,&
     &   KJP     , KJT     , KJT1   , PONEMINUS ,&
     &   PCOLMOL , PCOLO2  , PCOLO3 ,&
     &   KLAYTROP ,&
     &   ZSFLXZEN, ZTAUG   , ZTAUR  , PRMU0     &
     & )

  ELSEIF (JB == 29) THEN
    CALL SRTM_TAUMOL29 &
     & ( KIDIA    , KFDIA   , KLEV     ,&
     &   PFAC00   , PFAC01  , PFAC10   , PFAC11 ,&
     &   KJP      , KJT     , KJT1     , PONEMINUS ,&
     &   PCOLH2O  , PCOLCO2 , PCOLMOL  ,&
     &   KLAYTROP , PSELFFAC, PSELFFRAC, KINDSELF  , PFORFAC, PFORFRAC, KINDFOR ,&
     &   ZSFLXZEN , ZTAUG   , ZTAUR    , PRMU0     &
     & )

  ENDIF

  DO JG=1,IGT
    DO IC=1,ICOUNT
      JL=INDEX(IC)
      IW(JL)=IW(JL)+1

      ZINCFLX(JL,IW(JL)) =ZSFLXZEN(JL,JG)*PRMU0(JL)
      ZINCFLUX(JL)    =ZINCFLUX(JL)+ZSFLXZEN(JL,JG)*PRMU0(JL)
      ZINCTOT(JL)     =ZINCTOT(JL)+ZSFLXZEN(JL,JG)
      ZINCF14(JL,IBM)=ZINCF14(JL,IBM)+ZSFLXZEN(JL,JG)

      !-- CALL to compute layer reflectances and transmittances for direct
      !  and diffuse sources, first clear then cloudy.
      !   Use direct/parallel albedo for direct radiation and diffuse albedo
      !   otherwise.

      ! ZREFC(JK)  direct albedo for clear
      ! ZREFO(JK)  direct albedo for cloud
      ! ZREFDC(JK) diffuse albedo for clear
      ! ZREFDO(JK) diffuse albedo for cloud
      ! ZTRAC(JK)  direct transmittance for clear
      ! ZTRAO(JK)  direct transmittance for cloudy
      ! ZTRADC(JK) diffuse transmittance for clear
      ! ZTRADO(JK) diffuse transmittance for cloudy

      ! ZREF(JK)   direct reflectance
      ! ZREFD(JK)  diffuse reflectance
      ! ZTRA(JK)   direct transmittance
      ! ZTRAD(JK)  diffuse transmittance

      ! ZDBTC(JK)  clear direct beam transmittance
      ! ZDBTO(JK)  cloudy direct beam transmittance
      ! ZDBT(JK)   layer mean direct beam transmittance
      ! ZTDBT(JK)  total direct beam transmittance at levels

      !-- clear-sky
      !----- TOA direct beam
      ZTDBTC(JL,1)=1._JPRB
      !----- surface values
      ZDBTC(JL,KLEV+1) =0.0_JPRB
      ZTRAC(JL,KLEV+1) =0.0_JPRB
      ZTRADC(JL,KLEV+1)=0.0_JPRB
      ZREFC(JL,KLEV+1) =PALBP(JL,IBM)
      ZREFDC(JL,KLEV+1)=PALBD(JL,IBM)
      ZRUPC(JL,KLEV+1) =PALBP(JL,IBM)
      ZRUPDC(JL,KLEV+1)=PALBD(JL,IBM)

      !-- total sky
      !----- TOA direct beam
      ZTDBT(JL,1)=1._JPRB
      !----- surface values
      ZDBT(JL,KLEV+1) =0.0_JPRB
      ZTRA(JL,KLEV+1) =0.0_JPRB
      ZTRAD(JL,KLEV+1)=0.0_JPRB
      ZREF(JL,KLEV+1) =PALBP(JL,IBM)
      ZREFD(JL,KLEV+1)=PALBD(JL,IBM)
      ZRUP(JL,KLEV+1) =PALBP(JL,IBM)
      ZRUPD(JL,KLEV+1)=PALBD(JL,IBM)
    ENDDO


    !-- NB: a two-stream calculations from top to bottom, but RRTM_SW quantities
    !       are given bottom to top (argh!)
    !       Inputs for clouds and aerosols are bottom to top as inputs

    DO JK=1,KLEV
      IKL=KLEV+1-JK
      DO IC=1,ICOUNT
        JL=INDEX(IC)
        !-- clear-sky optical parameters
        LLRTCHK(JL,JK)=.TRUE.

        !-- clear-sky optical parameters including aerosols
        ZTAUC(JL,JK) = ZTAUR(JL,IKL,JG) + ZTAUG(JL,IKL,JG) + PTAUA(JL,IKL,IBM)
        ZOMCC(JL,JK) = ZTAUR(JL,IKL,JG)*1.0_JPRB + PTAUA(JL,IKL,IBM)*POMGA(JL,IKL,IBM)
        ZGCC(JL,JK) = PASYA(JL,IKL,IBM)*POMGA(JL,IKL,IBM)*PTAUA(JL,IKL,IBM) / ZOMCC(JL,JK)
        ZOMCC(JL,JK) = ZOMCC(JL,JK) / ZTAUC(JL,JK)

      ENDDO
    ENDDO
    DO JK=1,KLEV
      IKL=KLEV+1-JK
      DO IC=1,ICOUNT
        JL=INDEX(IC)
        !-- total sky optical parameters
        ZTAUO(JL,JK) = ZTAUR(JL,IKL,JG) + ZTAUG(JL,IKL,JG) + PTAUA(JL,IKL,IBM) + PTAUC(JL,IKL,IW(JL))
        ZOMCO(JL,JK) = PTAUA(JL,IKL,IBM)*POMGA(JL,IKL,IBM) + PTAUC(JL,IKL,IW(JL))*POMGC(JL,IKL,IW(JL)) &
         & + ZTAUR(JL,IKL,JG)*1.0_JPRB
        ZGCO(JL,JK) = (PTAUC(JL,IKL,IW(JL))*POMGC(JL,IKL,IW(JL))*PASYC(JL,IKL,IW(JL))  &
         & +  PTAUA(JL,IKL,IBM)*POMGA(JL,IKL,IBM)*PASYA(JL,IKL,IBM)) &
         & /  ZOMCO(JL,JK)
        ZOMCO(JL,JK) = ZOMCO(JL,JK) / ZTAUO(JL,JK)

      ENDDO
    ENDDO

    !-- Delta scaling for clear-sky / aerosol optical quantities
    DO  JK=1,KLEV
      DO IC=1,ICOUNT
        JL=INDEX(IC)
        ZF(JL)=ZGCC(JL,JK)*ZGCC(JL,JK)
        ZWF(JL)=ZOMCC(JL,JK)*ZF(JL)
        ZTAUC(JL,JK)=(1._JPRB-ZWF(JL))*ZTAUC(JL,JK)
        ZOMCC(JL,JK)=(ZOMCC(JL,JK)-ZWF(JL))/(1.0_JPRB-ZWF(JL))
        ZGCC(JL,JK)=(ZGCC(JL,JK)-ZF(JL))/(1.0_JPRB-ZF(JL))
      ENDDO
    ENDDO

    CALL SRTM_REFTRA ( KIDIA, KFDIA, KLEV, I_KMODTS ,&
     &   LLRTCHK, ZGCC  , PRMU0, ZTAUC , ZOMCC ,&
     &   ZREFC  , ZREFDC, ZTRAC, ZTRADC )

    !-- Delta scaling for cloudy quantities
    DO JK=1,KLEV
      IKL=KLEV+1-JK
      DO IC=1,ICOUNT
        JL=INDEX(IC)
        LLRTCHK(JL,JK)=.FALSE.
        ZF(JL)=ZGCO(JL,JK)*ZGCO(JL,JK)
        ZWF(JL)=ZOMCO(JL,JK)*ZF(JL)
        ZTAUO(JL,JK)=(1._JPRB-ZWF(JL))*ZTAUO(JL,JK)
        ZOMCO(JL,JK)=(ZOMCO(JL,JK)-ZWF(JL))/(1._JPRB-ZWF(JL))
        ZGCO(JL,JK)=(ZGCO(JL,JK)-ZF(JL))/(1._JPRB-ZF(JL))
        LLRTCHK(JL,JK)=(PFRCL(JL,IW(JL),IKL) > REPCLC)
      ENDDO
    ENDDO

    CALL SRTM_REFTRA ( KIDIA, KFDIA, KLEV, I_KMODTS ,&
     &   LLRTCHK, ZGCO  , PRMU0, ZTAUO , ZOMCO ,&
     &   ZREFO , ZREFDO, ZTRAO, ZTRADO )

    DO JK=1,KLEV
      IKL=KLEV+1-JK
      DO IC=1,ICOUNT
        JL=INDEX(IC)
        !-- combine clear and cloudy contributions for total sky

        ZCLEAR(JL)   = 1.0_JPRB - PFRCL(JL,IW(JL),IKL)
        ZCLOUD(JL)   = PFRCL(JL,IW(JL),IKL)

        ZREF(JL,JK) = ZCLEAR(JL)*ZREFC(JL,JK) + ZCLOUD(JL)*ZREFO(JL,JK)
        ZREFD(JL,JK)= ZCLEAR(JL)*ZREFDC(JL,JK)+ ZCLOUD(JL)*ZREFDO(JL,JK)
        ZTRA(JL,JK) = ZCLEAR(JL)*ZTRAC(JL,JK) + ZCLOUD(JL)*ZTRAO(JL,JK)
        ZTRAD(JL,JK)= ZCLEAR(JL)*ZTRADC(JL,JK)+ ZCLOUD(JL)*ZTRADO(JL,JK)

        !-- direct beam transmittance
        ZARG1(JL)      = MIN( 200._JPRB, ZTAUC(JL,JK)/PRMU0(JL) )
        ZARG2(JL)      = MIN( 200._JPRB, ZTAUO(JL,JK)/PRMU0(JL) )
        ZDBTMC(JL)     = EXP(-ZARG1(JL) )
        ZDBTMO(JL)     = EXP(-ZARG2(JL) )
        ZDBT(JL,JK)   = ZCLEAR(JL)*ZDBTMC(JL)+ZCLOUD(JL)*ZDBTMO(JL)
        ZTDBT(JL,JK+1)= ZDBT(JL,JK)*ZTDBT(JL,JK)

        !-- clear-sky
        ZDBTC(JL,JK)   =ZDBTMC(JL)
        ZTDBTC(JL,JK+1)=ZDBTC(JL,JK)*ZTDBTC(JL,JK)

      ENDDO
    ENDDO

    !-- vertical quadrature producing clear-sky fluxes

    !    print *,'SRTM_SPCVRT after 3 before SRTM_VRTQDR clear'

    CALL SRTM_VRTQDR ( KIDIA, KFDIA, KLEV, IW ,&
     &   ZREFC, ZREFDC, ZTRAC , ZTRADC ,&
     &   ZDBTC, ZRDNDC, ZRUPC , ZRUPDC, ZTDBTC ,&
     &   ZCD  , ZCU   , PRMU0 )

    !-- vertical quadrature producing cloudy fluxes

    CALL SRTM_VRTQDR ( KIDIA, KFDIA, KLEV, IW ,&
     &   ZREF , ZREFD , ZTRA , ZTRAD ,&
     &   ZDBT , ZRDND , ZRUP , ZRUPD , ZTDBT ,&
     &   ZFD  , ZFU   , PRMU0)

    !-- up and down-welling fluxes at levels
    DO JK=1,KLEV+1
      DO IC=1,ICOUNT
        JL=INDEX(IC)
        !-- accumulation of spectral fluxes
        PBBFU(JL,JK) = PBBFU(JL,JK) + ZINCFLX(JL,IW(JL))*ZFU(JL,JK,IW(JL))
        PBBFD(JL,JK) = PBBFD(JL,JK) + ZINCFLX(JL,IW(JL))*ZFD(JL,JK,IW(JL))
        PBBCU(JL,JK) = PBBCU(JL,JK) + ZINCFLX(JL,IW(JL))*ZCU(JL,JK,IW(JL))
        PBBCD(JL,JK) = PBBCD(JL,JK) + ZINCFLX(JL,IW(JL))*ZCD(JL,JK,IW(JL))

      ENDDO
    ENDDO
    DO IC=1,ICOUNT
      JL=INDEX(IC)
      IF ( JB >= 26 .AND. JB <= 28 ) THEN
        PFUVF(JL) = PFUVF(JL) + ZINCFLX(JL,IW(JL))*ZFD(JL,KLEV+1,IW(JL))
        PFUVC(JL) = PFUVC(JL) + ZINCFLX(JL,IW(JL))*ZCD(JL,KLEV+1,IW(JL))
      ENDIF
      IF ( JB == 23) THEN
        PPARF(JL) = PPARF(JL)+ ZINCFLX(JL,IW(JL))*ZFD(JL,KLEV+1,IW(JL))*ZCOEFVS
        PPARCF(JL)=PPARCF(JL)+ ZINCFLX(JL,IW(JL))*ZCD(JL,KLEV+1,IW(JL))*ZCOEFVS
      ENDIF
      IF ( JB == 24) THEN
        PPARF(JL) = PPARF(JL)+ ZINCFLX(JL,IW(JL))*ZFD(JL,KLEV+1,IW(JL))
        PPARCF(JL)=PPARCF(JL)+ ZINCFLX(JL,IW(JL))*ZCD(JL,KLEV+1,IW(JL))
      ENDIF
      PSUDU(JL) = PSUDU(JL)  + ZINCFLX(JL,IW(JL))*ZTDBT(JL,KLEV+1)
    ENDDO

  ENDDO
  !-- end loop on JG

ENDDO
!-- end loop on JB

!     ------------------------------------------------------------------
END SUBROUTINE SRTM_SPCVRT_MCICA
