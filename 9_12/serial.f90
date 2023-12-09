PROGRAM serial

implicit none

DOUBLE PRECISION :: INTEGRAL, A,B,h,X, funct

INTEGER :: N, ii

EXTERNAL funct

print*, 'Enter a, b, and n'

READ*, A,B,N 

h=(B-A)/N 

INTEGRAL=(funct(A)+funct(B))/2.d0

X=A 

do ii=1,N-1
 X=X + h
 INTEGRAL=INTEGRAL+ funct(X)
end do

INTEGRAL=INTEGRAL*h

!print*, 'with N= ', N, ' trapezoids our estimate'

print*, 'of the integral from ', INTEGRAL

 
END PROGRAM serial


DOUBLE PRECISION FUNCTION  funct(Y)
IMPLICIT NONE 
DOUBLE PRECISION, INTENT(IN) :: Y

funct=Y**2
RETURN 
END FUNCTION