MODULE YOMRANDOM_STREAMS

!**** *YOMRANDOM_STREAMS*  - Globally-available random number streams 

!  Original :      2004-05-06 - Mike Fisher
!  Modifications: 07-09-2006 - Judith Berner
!                              added random number streams for stochastic physics
   
                  

USE RANDOM_NUMBERS_MIX, ONLY: randomNumberStream

IMPLICIT NONE

SAVE

TYPE (randomNumberStream) :: yd_random_stream_scan2mtl
TYPE (randomNumberStream) :: yd_random_stream_stochphys
TYPE (randomNumberStream) :: yd_random_stream_stochphys_CABS
TYPE (randomNumberStream) :: yd_random_stream_stochphys_SPBS

END MODULE YOMRANDOM_STREAMS
