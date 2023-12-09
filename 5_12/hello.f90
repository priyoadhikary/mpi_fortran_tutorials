PROGRAM MPI_HELLO

implicit none

include 'mpif.h'

integer :: ierr, my_rank, num_proc

CALL MPI_INIT(ierr)

CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
CALL MPI_COMM_SIZE(MPI_COMM_WORLD, num_proc, ierr)

print*, 'hello from ', my_rank, 'from total', num_proc 

CALL MPI_FINALIZE(ierr)









END PROGRAM 
