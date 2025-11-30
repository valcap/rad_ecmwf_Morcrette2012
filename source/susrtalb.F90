SUBROUTINE SUSRTALB (KTSW,KSW)

!**** *SUSRTALB*   - INITIALIZE COMMON YOESW

!     PURPOSE.
!     --------
!           INITIALIZE YOESW, THE COMMON THAT CONTAINS COEFFICIENTS
!           NEEDED TO RUN THE SHORTWAVE RADIATION SUBROUTINES

!**   INTERFACE.
!     ----------
!        *CALL* *SUSRTALB

!        EXPLICIT ARGUMENTS :
!        --------------------
!        NONE

!        IMPLICIT ARGUMENTS :
!        --------------------
!        COMMON YOESW

!     METHOD.
!     -------
!        SEE DOCUMENTATION

!     EXTERNALS.
!     ----------

!     REFERENCE.
!     ----------
!        ECMWF RESEARCH DEPARTMENT DOCUMENTATION OF THE IFS

!     AUTHOR.
!     -------
!        JEAN-JACQUES MORCRETTE *ECMWF*

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 03-04-24
!        03-04-24 JJ Morcrette  SRTM with 14 spectral intervals
!        M.Hamrud      01-Oct-2003 CY28 Cleaning

!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE YOERAD   , ONLY : NTSW     
USE YOS_SW   , ONLY : RSUN     ,&
 & RALBICE_AR         ,RALBICE_AN         ,RWEIGHT 
USE YOESW    , ONLY : &
! & RALBICE_AR         ,RALBICE_AN         ,RWEIGHT  ,&
 & RADJUST            ,NTYPS  

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KTSW 
INTEGER(KIND=JPIM),INTENT(IN)    :: KSW 
!     ----------------------------------------------------------------

REAL(KIND=JPRB) :: ZWEIGHT14(14,8), ZALBICE14(12,14), ZSUN14(14)
  
REAL(KIND=JPRB) :: ZTWEIGHT(8)  

INTEGER(KIND=JPIM) :: JW, JTYPS, IM, JM

REAL(KIND=JPRB) :: ZADJUS14


!     ------------------------------------------------------------------

!*          1.1    FOURTEEN SPECTRAL INTERVALS FOR RRTM_SW
!                  ---------------------------------------

! SW : 14 spectral intervals 
!  3.846 -  3.077       6
!  3.077 -  2.500    6
!  2.500 -  2.150    5
!  2.150 -  1.942    5
!  1.942 -  1.626    5
!  1.626 -  1.299    5
!  1.299 -  1.242    5
!  1.242 -  0.7782    4
!  0.7782-  0.6250    4
!  0.6250-  0.4415    3
!  0.4415-  0.3448    2
!  0.3448-  0.2632    2
!  0.2632-  0.2000    1
! 12.195 -  3.846    6

!     ----------------------------------------------------------------
!*    2.1   FRACTION OF INCIDENT SOLAR ENERGY AT TOA IN SPECTRAL INTERVALS

ZSUN14 = (/ 0.00885058_JPRB,  0.01488434_JPRB,  0.01734347_JPRB, 0.01639185_JPRB &
 & , 0.04065612_JPRB, 0.07523013_JPRB, 0.01775560_JPRB, 0.25269468_JPRB, 0.15946759_JPRB &
 & , 0.25375430_JPRB, 0.09464472_JPRB, 0.03665504_JPRB, 0.00225105_JPRB, 0.00942053_JPRB /)  
      
!*    2.2   WEIGHTS FOR SPECTRAL SURFACE ALBEDO
!           Water
!           Sea-Ice (Ebert, Curry, 1993)
!           Wet skin
!           Low vegetation (snow free) (BR, 1982)
!           Snow on low vegetation (Warren, 1982)
!           High vegetation (snow free) (BR, 1982)
!           Snow under high vegetation  (Warren, 1982)
!           Bare soil (Briegleb & Ramanathan, 1982)

NTYPS = 8

ZWEIGHT14(:,1)= (/ 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB &
 & , 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB /)  
ZWEIGHT14(:,2)= (/ 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB &
 & , 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB /)  
ZWEIGHT14(:,3)= (/ 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB &
 & , 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB /)  
ZWEIGHT14(:,4)= (/ 1._JPRB, 1._JPRB, 2._JPRB, 2._JPRB, 2._JPRB, 2._JPRB, 2._JPRB &
 & , 4._JPRB, 4._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB /)  
ZWEIGHT14(:,5)= (/ 0.010_JPRB, 0.010_JPRB, 0.159_JPRB, 0.159_JPRB, 0.159_JPRB, 0.159_JPRB, 0.159_JPRB &
 & , 0.798_JPRB, 0.798_JPRB, 0.920_JPRB, 0.920_JPRB, 0.920_JPRB, 0.920_JPRB, 0.010_JPRB /)  
ZWEIGHT14(:,6)= (/ 1._JPRB, 1._JPRB, 2._JPRB, 2._JPRB, 2._JPRB, 2._JPRB, 2._JPRB &
 & , 4._JPRB, 4._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB /)  
ZWEIGHT14(:,7)= (/ 0.010_JPRB, 0.010_JPRB, 0.092_JPRB, 0.092_JPRB, 0.092_JPRB, 0.092_JPRB, 0.092_JPRB &
 & , 0.664_JPRB, 0.664_JPRB, 0.860_JPRB, 0.860_JPRB, 0.860_JPRB, 0.860_JPRB, 0.010_JPRB /)  
ZWEIGHT14(:,8)= (/ 1._JPRB, 1._JPRB, 2._JPRB, 2._JPRB, 2._JPRB, 2._JPRB, 2._JPRB &
 & , 2._JPRB, 2._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB, 1._JPRB /)  

!*    2.3   SPECTRAL SURFACE ALBEDO FOR SEA-ICE

!*  For sea ice, monthly values are based on Ebert and Curry, 1993, Table 2.
!   We take dry snow albedo as the representative value for non-summer
!   months, and bare sea-ice as the representative value for summer
!   months. The values for Antarctic are shifted six-months.

!- 14-spectral intervals

!*  Sea ice surf. albedo for 2.38-4.00 microns (snow covered; Ebert and Curry, 1993)
ZALBICE14(1:12,1) = (/0.025_JPRB,0.025_JPRB,0.025_JPRB,0.025_JPRB,&
 & 0.025_JPRB,0.030_JPRB,0.036_JPRB,0.036_JPRB,&
 & 0.025_JPRB,0.025_JPRB,0.025_JPRB,0.025_JPRB/)  
ZALBICE14(1:12,2) = (/0.025_JPRB,0.025_JPRB,0.025_JPRB,0.025_JPRB,&
 & 0.025_JPRB,0.030_JPRB,0.036_JPRB,0.036_JPRB,&
 & 0.025_JPRB,0.025_JPRB,0.025_JPRB,0.025_JPRB/)  

!*  Sea ice surf. albedo for 1.19-2.38 micron (snow covered; Ebert and Curry, 1993)
ZALBICE14(1:12,3) = (/0.250_JPRB,0.250_JPRB,0.250_JPRB,0.250_JPRB,&
 & 0.250_JPRB,0.153_JPRB,0.055_JPRB,0.055_JPRB,&
 & 0.250_JPRB,0.250_JPRB,0.250_JPRB,0.250_JPRB/)  
ZALBICE14(1:12,4) = (/0.250_JPRB,0.250_JPRB,0.250_JPRB,0.250_JPRB,&
 & 0.250_JPRB,0.153_JPRB,0.055_JPRB,0.055_JPRB,&
 & 0.250_JPRB,0.250_JPRB,0.250_JPRB,0.250_JPRB/)  
ZALBICE14(1:12,5) = (/0.250_JPRB,0.250_JPRB,0.250_JPRB,0.250_JPRB,&
 & 0.250_JPRB,0.153_JPRB,0.055_JPRB,0.055_JPRB,&
 & 0.250_JPRB,0.250_JPRB,0.250_JPRB,0.250_JPRB/)  
ZALBICE14(1:12,6) = (/0.250_JPRB,0.250_JPRB,0.250_JPRB,0.250_JPRB,&
 & 0.250_JPRB,0.153_JPRB,0.055_JPRB,0.055_JPRB,&
 & 0.250_JPRB,0.250_JPRB,0.250_JPRB,0.250_JPRB/)  
ZALBICE14(1:12,7) = (/0.250_JPRB,0.250_JPRB,0.250_JPRB,0.250_JPRB,&
 & 0.250_JPRB,0.153_JPRB,0.055_JPRB,0.055_JPRB,&
 & 0.250_JPRB,0.250_JPRB,0.250_JPRB,0.250_JPRB/)  

!*  Sea ice surf. albedo for 0.69-1.19 micron (snow covered; Ebert and Curry, 1993)
ZALBICE14(1:12,8) = (/0.832_JPRB,0.832_JPRB,0.832_JPRB,0.832_JPRB,&
 & 0.832_JPRB,0.638_JPRB,0.443_JPRB,0.443_JPRB,&
 & 0.832_JPRB,0.832_JPRB,0.832_JPRB,0.832_JPRB/)  
ZALBICE14(1:12,9) = (/0.832_JPRB,0.832_JPRB,0.832_JPRB,0.832_JPRB,&
 & 0.832_JPRB,0.638_JPRB,0.443_JPRB,0.443_JPRB,&
 & 0.832_JPRB,0.832_JPRB,0.832_JPRB,0.832_JPRB/)  

!*  Sea ice surf. albedo for 0.44-0.69 micron (snow covered; Ebert and Curry, 1993)
ZALBICE14(1:12,10)= (/0.975_JPRB,0.975_JPRB,0.975_JPRB,0.975_JPRB,&
 & 0.975_JPRB,0.876_JPRB,0.778_JPRB,0.778_JPRB,&
 & 0.975_JPRB,0.975_JPRB,0.975_JPRB,0.975_JPRB/)  

!*  Sea ice surf. albedo for 0.25-0.44 micron (snow covered; Ebert and Curry, 1993)
ZALBICE14(1:12,11)= (/0.975_JPRB,0.975_JPRB,0.975_JPRB,0.975_JPRB,&
 & 0.975_JPRB,0.876_JPRB,0.778_JPRB,0.778_JPRB,&
 & 0.975_JPRB,0.975_JPRB,0.975_JPRB,0.975_JPRB/)  
ZALBICE14(1:12,12)= (/0.975_JPRB,0.975_JPRB,0.975_JPRB,0.975_JPRB,&
 & 0.975_JPRB,0.876_JPRB,0.778_JPRB,0.778_JPRB,&
 & 0.975_JPRB,0.975_JPRB,0.975_JPRB,0.975_JPRB/)  

!*  Sea ice surf. albedo for 0.185-0.25 micron (snow covered; Ebert and Curry, 1993)
ZALBICE14(1:12,13)= (/0.975_JPRB,0.975_JPRB,0.975_JPRB,0.975_JPRB,&
 & 0.975_JPRB,0.876_JPRB,0.778_JPRB,0.778_JPRB,&
 & 0.975_JPRB,0.975_JPRB,0.975_JPRB,0.975_JPRB/)  

!*  Sea ice surf. albedo for 2.38-4.00 microns (snow covered; Ebert and Curry, 1993)
ZALBICE14(1:12,14)= (/0.025_JPRB,0.025_JPRB,0.025_JPRB,0.025_JPRB,&
 & 0.025_JPRB,0.030_JPRB,0.036_JPRB,0.036_JPRB,&
 & 0.025_JPRB,0.025_JPRB,0.025_JPRB,0.025_JPRB/)  

ZADJUS14 = 1.0_JPRB
                                  
!=====================================================================

!*       2.    SET VALUES.
!              -----------

DO JW=1,NTSW
  RSUN(JW)=0.0_JPRB
  DO JTYPS=1,NTYPS
    RWEIGHT(JW,JTYPS)=0.0_JPRB
  ENDDO  
ENDDO
  
IF (KTSW == 14 .AND. KSW == 14) THEN
  RADJUST=ZADJUS14
  DO JW=1,KSW
    RSUN (JW)=ZSUN14(JW)
    DO JTYPS=1,NTYPS
      ZTWEIGHT(JTYPS)=ZTWEIGHT(JTYPS)+ZWEIGHT14(JW,JTYPS)*ZSUN14(JW)
    ENDDO
  ENDDO

  DO JW=1,KSW
    DO JTYPS=1,NTYPS
      RWEIGHT(JW,JTYPS)=ZWEIGHT14(JW,JTYPS)/ZTWEIGHT(JTYPS)
    ENDDO
    
    DO JM=1,12
      IM=MOD(JM+5,12)+1
      RALBICE_AR(IM,JW)=ZALBICE14(IM,JW)
      RALBICE_AN(JM,JW)=RALBICE_AR(IM,JW)
    ENDDO  
  ENDDO    
  
ELSE
  PRINT *,'SUSRTALB: NTSW, NSW: ',KTSW,KSW
!  CALL ABOR1('SUSRTALB: WRONG NUMBER OF SW INTERVALS')
  STOP 'SUSRTALB: WRONG NUMBER OF SW INTERVALS'
ENDIF

!     ----------------------------------------------------------------
END SUBROUTINE SUSRTALB

