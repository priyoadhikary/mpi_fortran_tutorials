PROGRAM MPI_HELLO

IMPLICIT NONE

INCLUDE 'mpif.h'

integer :: ierr, my_rank, num_proc

CALL MPI_INIT(ierr)

CALL MPI_COMM_SIZE( MPI_COMM_WORLD, num_proc, ierr)

CALL MPI_COMM_RANK( MPI_COMM_WORLD, my_rank , ierr)


print*, "hello from  processor ", my_rank , 'out of total', num_proc, 'processesors'

CALL MPI_FINALIZE(ierr)



end program
