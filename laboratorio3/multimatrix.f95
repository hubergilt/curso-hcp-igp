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

    subroutine rand_matrix(arreglo1, nfilas, ncolumnas) 
        integer :: nfilas, ncolumnas, i , j
        real, allocatable :: arreglo1(:,:)

        do i=1, nfilas
            do j=1, ncolumnas
                arreglo1(i, j)=rand(0)*123.45    
            end do
            print *
        end do
    end subroutine rand_matrix


    subroutine multi_matrix(arreglo1, arreglo2, nfilas, ncolumnas, resultado) 
        integer :: nfilas, ncolumnas, i , j, k
        real, allocatable :: arreglo1(:,:), arreglo2(:,:), resultado(:,:)

        do i=1, nfilas
            do j=1, ncolumnas
                resultado(i, j)=0    
            end do
        end do

        do i=1, nfilas
            do j=1, ncolumnas
                do k=1, ncolumnas
                    resultado(i, j)=resultado(i, j)+arreglo1(i, k)*arreglo2(k, j)   
                end do  
            end do
        end do

    end subroutine multi_matrix

end module matrix


program multimatrix
    use matrix
    real, allocatable :: arreglo1(:,:)

    allocate (arreglo1(10,10))  

    !call read_matrix(arreglo1,10,2)
    call rand_matrix(arreglo1,10,2)

    call print_matrix(arreglo1,10,2)

end program multimatrix