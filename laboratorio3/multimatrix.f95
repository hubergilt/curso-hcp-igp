module matrix
    implicit none
contains

    subroutine print_matrix(arreglo1, nfilas, ncolumnas) 
        integer :: nfilas, ncolumnas, i , j
        real, allocatable :: arreglo1(:,:)

        do i=1, nfilas
            do j=1, ncolumnas
                write (*, "('('I2','I2')=', F6.2, 2x)", advance="no") i, j, arreglo1(i, j)           
            end do
            print *
        end do
    end subroutine print_matrix

    subroutine read_matrix(arreglo1, nfilas, ncolumnas) 
        integer :: nfilas, ncolumnas, i , j
        real, allocatable :: arreglo1(:,:)

        do i=1, nfilas
            do j=1, ncolumnas
                print *, "Matrix Cuadrada, ingrese valor en >> (",i,",",j,") "
                read *, arreglo1(i, j)        
            end do
            print *
        end do
    end subroutine read_matrix

end module matrix


program multimatrix
    use matrix
    real, allocatable :: arreglo1(:,:)

    allocate (arreglo1(10,10))  

    call print_matrix(arreglo1,10,2)

end program multimatrix