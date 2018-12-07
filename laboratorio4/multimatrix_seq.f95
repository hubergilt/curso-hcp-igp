program multimatrix_seq
    implicit none
    character(len=30) :: arg
    integer, parameter:: MA__NFIL = 4
    integer, parameter:: MA__NCOL = 4
    integer, parameter:: MB__NFIL = 4
    integer, parameter:: MB__NCOL = 4
    real, allocatable :: ma(:,:)
    real, allocatable :: mb(:,:)
    real, allocatable :: mr(:,:)
    integer :: ma_nfil
    integer :: ma_ncol
    integer :: mb_nfil
    integer :: mb_ncol
    integer :: i, j, k

    ma_nfil=MA__NFIL
    ma_ncol=MA__NCOL
    mb_nfil=MB__NFIL
    mb_ncol=MB__NCOL 
    i=1
    j=1
    k=1



    if(iargc().eq.0) then
        print *, "Ingrese orden de las matrices i,j,k"
    else
        CALL getarg(1, arg)
        read(arg, *, IOSTAT=e)x

    end if

end program multimatrix_seq

subroutine print_matrix(arreglo1, nfilas, ncolumnas) 
    integer :: nfilas, ncolumnas, i , j
    real, allocatable :: arreglo1(:,:)

    do i=1, nfilas
        do j=1, ncolumnas
            write (*, "('('I2','I2')=', F10.2, 2x)", advance="no") i, j, arreglo1(i, j)           
        end do
        print *
    end do
end subroutine print_matrix
