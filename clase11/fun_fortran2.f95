
subroutine CALC_CIRCUNFERENCIA(radius,pius,circunferenciaus)

	implicit none
	DOUBLE PRECISION :: radius,pius,circunferenciaus
	print *,"Desde fortran radio",radius,"pi",pius
	circunferenciaus= 2*pius*radius
	print *,"El circulo es", circunferenciaus
	



end subroutine CALC_CIRCUNFERENCIA

