subroutine setran (KCONSEED,YD_RANDOM_STREAM,KOUTSEED)

!*** *SETRAN* - Sets the seed for a random number stream

!   Purpose.
!   --------

!      To set the seed for a random number stream as a function of NINDAT, NSSSSS and KCONSEED

!   Interface.
!   ----------

!      CALL SETRAN(KCONSEED)

!      INPUT: KCONSEED                   - integer to control the seeding
!      OPTIONAL OUTPUT: YD_RANDOM_STREAM - initialized random number stream
!      OPTIONAL OUTPUT: KOUTSEED         - a seed constructed from KCONSEED and the model time

!   Externals.
!   ----------
!      RANSET

!   Method.
!   -------

!      The seed is set to a function of the initial time of the run,
!      and of an input integer KCONSEED.
!      For dates chosen at random, the seeds are approximately
!      uniformly distributed between 1 and HUGE(0). A highly nonlinear
!      function is used to reduce the possibility of correlations
!      between random sequences generated for different initial dates.

!   Reference.
!   ----------
!      None yet!

!   Author.
!   -------
!      Mike Fisher *ECMWF*

!   Modifications.
!   --------------
!      Original      01/02/94
!      M. Fisher     20/09/95  -  reduce chance of the same seed being
!                                 repeated.
!      M. Fisher     15/01/96  -  fortran 90
!      R. Buizza     15/12/98  -  add input integer KCONSEED
!      Jan Haseler   10/04/02  -  fix for too large numbers on p690
!      M. Fisher     25/09/02  -  adapt for RANDOM_NUMBERS_MIX module
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        M.Hamrud      01-Dec-2003 CY28R1 Cleaning
!      M.Fisher      2004-05-06 - Added argument yd_random_stream

!-----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMCST   , ONLY : RDAY
USE YOMRIP   , ONLY : NINDAT   ,NSSSSS
USE RANDOM_NUMBERS_MIX

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KCONSEED 
TYPE(randomNumberStream), INTENT(INOUT), OPTIONAL :: YD_RANDOM_STREAM
INTEGER(KIND=JPIM),INTENT(OUT), OPTIONAL :: KOUTSEED

INTEGER(KIND=JPIM) :: idigits, iradix, is, jdigit, iscale, iseed

REAL(KIND=JPRB) :: zirr1, zs, zt, ztim

#include "fcttim.h"

iradix  = RADIX(ztim)
idigits = DIGITS(ztim)
zirr1 = 0.5_JPRB*(SQRT(5._JPRB)-1.0_JPRB)

!--- generate a unique number from the date and the input KCONSEED

ztim = RJUDAT(NCCAA(NINDAT),NMM(NINDAT),NDD(NINDAT))&
 & -1720994.5_JPRB + real(NSSSSS,JPRB)/86400._JPRB &
 & -2581470.3_JPRB*KCONSEED  

!--- multiply by an irrational number to randomize the bits and scale
!--- to between 0 and 1.

ztim = FRACTION(zirr1*ABS(ztim))

!--- reverse the bits

zs = 0.0_JPRB
zt = ztim
DO jdigit=1,idigits
  zt = zt*iradix
  is = int(zt)
  zt = zt-is
  zs = (zs+is)/iradix
ENDDO

!--- Scale to an odd number between 0 and HUGE-100000000
!--- (Allow some headroom because some routines use setran to set an initial seed, 
!---  and then generate new seeds by incrementing.)

iscale=(HUGE(iseed)-100000000)/2
iseed = 1 + 2*INT( iscale*zs )

!--- set/output the seed

IF (PRESENT(KOUTSEED)) THEN
  KOUTSEED=ISEED
ENDIF

IF (PRESENT(yd_random_stream)) THEN
  call initialize_random_numbers (iseed,yd_random_stream)
ENDIF

end subroutine setran
