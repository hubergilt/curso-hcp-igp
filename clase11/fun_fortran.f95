
subroutine calc_area(radius,pius,areaus)

    implicit none
    DOUBLE PRECISION :: radius,pius,areaus
    print *,"Desde fortran radio",radius,"pi",pius
    areaus= pius*pius*radius
    print *,"El area es",areaus
    
end subroutine calc_area
