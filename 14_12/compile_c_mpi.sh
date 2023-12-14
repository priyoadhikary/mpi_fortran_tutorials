#!/bin/bash
clear
rm *.o
rm *.x

compiler="mpif90 "
#compiler="mpif90.mpich "

#mpif90 hello_world_mpi.f90 -o hello_world_mpi.exe


libF=" -L/usr/lib/x86_64-linux-gnu -llapack -lblas"


$compiler -c  global_mpi_var.f90\
                      mpi_subroutines.f90\
				  parallel_int.f90

$compiler -o  intg.x\
                     global_mpi_var.o\
				 mpi_subroutines.o\
                     parallel_int.o\
	      $libF

 mpirun -np 4 ./intg.x
#mv a.out  greeting.x



