#!/bin/bash
clear
rm *.o
rm *.x

compiler="mpif90.mpich "
libF=" -L/usr/lib/x86_64-linux-gnu -llapack -lblas"


$compiler -c  helo.f90

$compiler -o  intg.x\
              helo.o\
	      $libF

 #mpirun -np 4 ./intg.x
#mv a.out  greeting.x



