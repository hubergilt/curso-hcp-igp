
subroutine CALC_AREA(radius,pius,areaus)

	implicit none
	DOUBLE PRECISION :: radius,pius,areaus
	print *,"Desde fortran radio",radius,"pi",pius
	areaus= pius*pius*radius
	print *,"El area es",areaus
	



end subroutine CALC_AREA
