#!/bin/bash

clear
rm -r output
rm chi_*_*.txt
rm chi_trace.txt
rm chi_sum.txt
rm band1.txt band2.txt band3.txt band4.txt band5.txt
rm dos_*
rm kmesh.txt kpath_3d.txt qmesh.txt newk.txt
rm *.o
rm *.x
rm *.mod
mkdir output

compiler="mpif90.mpich "

libF=" -L/usr/lib/x86_64-linux-gnu -llapack -lblas"

$compiler -c global_var.f90 \
          kmesh.f90 \
          main_time.f90 \
          lapack_module.f90 \
          hband_module.f90 \
          greens_func.f90 \
          print_data.f90 \
          MAIN_SPIN_FLUC_MPI.f90

$compiler -o spin_fluc.x \
          global_var.o \
          kmesh.o \
          main_time.o \
          lapack_module.o \
          hband_module.o \
          greens_func.f90 \
          print_data.f90 \
          MAIN_SPIN_FLUC_MPI.f90 \
          $libF

rm *.o
rm *.mod
rm *.mod*

mv a.out spin_fluc.x

#mpirun -np 4 ./spin_fluc.x

#mv chi*.txt output/

