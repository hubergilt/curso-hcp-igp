gcc programa_en_c.c -c 
gfortran fun_fortran.f95 -c
gcc -lgfortran programa_en_c.o fun_fortran.o -o programa_en_c.exe


gcc -c cfun.c 
gfortran -c programa_fortran.f95
gfortran programa_fortran.o cfun.o -o programa_fortran.exe
