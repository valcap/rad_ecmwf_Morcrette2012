SUBROUTINE QSAT ( DQ, Q, E, P, T, R)

USE PARKIND1 ,ONLY : JPIM ,JPRB

IMPLICIT NONE

REAL(KIND=JPRB) ,INTENT(IN) :: P, R, T
REAL(KIND=JPRB) ,INTENT(OUT) :: DQ, E, Q

REAL(KIND=JPRB) :: A, B, TF


!      ----------------------------------------------------------------

tf=273.16_JPRB
if (t.gt.tf) then
  a = 17.269_JPRB
  b = 35.86_JPRB
else
  a = 21.874_JPRB
  b = 7.66_JPRB
end if  
e = 6.11_JPRB  * exp(a*(t-tf)/(t-b))
q = 0.622_JPRB * r*e/(p-r*e)
dq=a*p*q*(tf-b)
dq=dq/((p-r*e)*(t-b)*(t-b))

!print 9001, P,T,R,A,B,E,Q
9001 format (1x,'QSAT ',5F10.3,2E12.5)

!----------------------------------------
END SUBROUTINE QSAT
