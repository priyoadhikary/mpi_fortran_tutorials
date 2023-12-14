PROGRAM parallel_int


use GLOBAL_MPI_VAR
use MPI_SUBROUTINES

IMPLICIT NONE

INCLUDE 'mpif.h'

INTEGER:: my_rank, p, local_N,  source,&
   status(MPI_STATUS_SIZE) ,ierr , total_N

DOUBLE PRECISION :: local_integral,total_integral,trap, &
 time_start,time_end
DOUBLE PRECISION :: local_a, local_b, h
DOUBLE PRECISION :: funct
EXTERNAL funct
INTEGER, PARAMETER :: dest=0, tag=50
DOUBLE PRECISION :: a, b 
!data a, b,total_N,dest,tag /0.d0, 1.d0,40000,0,50/

CALL CPU_TIME(time_start)
CALL MPI_INIT(ierr)
!CALL MPI_COMM_SIZE(MPI_COMM_WORLD, p, ierr)
CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
CALL MPI_COMM_SIZE(MPI_COMM_WORLD, p, ierr)

if (my_rank==0) then
a=0.d0
b=1.d0
total_N =40000
!print*, ' Enter the values of a, b, N: '
!read*, a, b, total_N
print*, 'the values of a ', a , ' b ',  b, ' N are ', total_N 

end if

CALL MPI_BCAST(a,1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST(b,1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST(total_N ,1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

!read*,a,b,total_N

h=(b-a)/total_N
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

local_N=total_N/p 

local_a = a + my_rank*local_N*h 
local_b = local_a + local_N*h 

local_integral=funct(local_a,local_b,local_N,h )


! IF (my_rank == 0) then
! total_integral=local_integral


! ! do source =1,p-1
! ! CALL MPI_RECV(local_integral,1,MPI_DOUBLE_PRECISION, source,tag,MPI_COMM_WORLD,status,ierr)



! ! total_integral=total_integral+ local_integral
! ! end do


! else
! CALL MPI_SEND(local_integral, 1,MPI_DOUBLE_PRECISION,dest, tag,MPI_COMM_WORLD,ierr )
! END IF

CALL MPI_REDUCE(local_integral,total_integral,1, MPI_DOUBLE_PRECISION, MPI_SUM,0, MPI_COMM_WORLD, ierr)

CALL MPI_ALLREDUCE(local_integral,total_integral,1, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)





! IF (my_rank ==0) then
! print*, 'with N = ', total_N, 'Trapezoids estimate of the integral is ', total_integral
! !print*, 'hello from ', my_rank, 'from total', p
! END IF

IF (my_rank ==0) then
print*, 'WITHOUT ALL REDUCE with RANK NO ', my_rank, 'Trapezoids estimate of the integral is ', total_integral
!print*, 'hello from ', my_rank, 'from total', p
END IF

IF (my_rank ==3) then
print*, 'WITH ALL REDUCE with RANK NO ', my_rank, 'Trapezoids estimate of the integral is ', total_integral
!print*, 'hello from ', my_rank, 'from total', p
END IF


CALL MPI_FINALIZE(ierr)

CALL CPU_TIME(time_end)

print*, 'The elapsed time is ', time_end-time_start, 'sec'

END PROGRAM parallel_int




DOUBLE PRECISION FUNCTION  funct(local_a1, local_b1, local_N1,h1)
IMPLICIT NONE 
DOUBLE PRECISION, INTENT(IN) :: local_a1, local_b1, h1
INTEGER, INTENT(IN):: local_N1
INTEGER:: jj
DOUBLE PRECISION:: integral1,XX, YY

DOUBLE PRECISION ::  funct2
EXTERNAL  funct2



integral1=(funct2(local_a1)+funct2(local_b1))/2.d0

XX=local_a1 

do jj=1,local_N1 -1
 XX=XX + h1
 integral1=integral1+ funct2(XX)
end do

integral1=integral1*h1

funct=integral1

!funct=1.d0
!funct=Y**2
RETURN 
END FUNCTION


DOUBLE PRECISION FUNCTION  funct2(Y)
IMPLICIT NONE 
DOUBLE PRECISION, INTENT(IN) :: Y

funct2=Y**5
RETURN 
END FUNCTION