
program programa_fortran
	INTEGER :: ierr
	DOUBLE PRECISION :: PI,radio
	PI=3.14159
	print *,"Cual es el radio del circulo"
	read *,radio
	print *,"Llamamo a nuestra funcion en C"
	print *,"El radio es ",radio,"y PI ",PI
	ierr= cfun(PI,radio)



end program programa_fortran
