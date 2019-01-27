#!/bin/bash

# compilar mpi : mpifort -show multimatrix_mpi.f95 -o multimatrix_mpi.exe

#gfortran -I/usr/include -c multimatrix_mpi.f95

#gfortran -I/usr/include multimatrix_mpi.o -o multimatrix_mpi.exe -L/usr/lib64 /usr/lib64/libhdf5hl_fortran.a /usr/lib64/libhdf5_hl.a /usr/lib64/libhdf5_fortran.a /usr/lib64/libhdf5.a -lpthread -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib64

# compilar hdf5 : mpifort -show multimatrix_mpi.f95 -o multimatrix_mpi.exe

#gfortran -m64 multimatrix_mpi.f95 -o multimatrix_mpi.exe -I/usr/local/mvapich2/2.3.0/gnu4.8.5/include -I/usr/local/mvapich2/2.3.0/gnu4.8.5/include -L/usr/local/mvapich2/2.3.0/gnu4.8.5/lib -lmpifort -Wl,-rpath -Wl,/usr/local/mvapich2/2.3.0/gnu4.8.5/lib -Wl,--enable-new-dtags -lmpi

# combinando ambos comandos de compilacion

gfortran -m64 -c multimatrix_mpi.f95  -I/usr/include -I/usr/local/mvapich2/2.3.0/gnu4.8.5/include -I/usr/local/mvapich2/2.3.0/gnu4.8.5/include -L/usr/local/mvapich2/2.3.0/gnu4.8.5/lib -lmpifort -Wl,-rpath -Wl,/usr/local/mvapich2/2.3.0/gnu4.8.5/lib -Wl,--enable-new-dtags -lmpi
gfortran -I/usr/include multimatrix_mpi.o -o multimatrix_mpi.exe -L/usr/lib64 /usr/lib64/libhdf5hl_fortran.a /usr/lib64/libhdf5_hl.a /usr/lib64/libhdf5_fortran.a /usr/lib64/libhdf5.a -lpthread -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib64 -I/usr/local/mvapich2/2.3.0/gnu4.8.5/include -I/usr/local/mvapich2/2.3.0/gnu4.8.5/include -L/usr/local/mvapich2/2.3.0/gnu4.8.5/lib -lmpifort -Wl,-rpath -Wl,/usr/local/mvapich2/2.3.0/gnu4.8.5/lib -Wl,--enable-new-dtags -lmpi