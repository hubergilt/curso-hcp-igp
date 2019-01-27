gcc -lm calcufunc.c -c
gfortran calculadora.f95 -c
gfortran calculadora.o calcufunc.o -o calculadora.exe
