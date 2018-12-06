module matrix
    implicit none

contains

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
                arreglo1(i, j)=rand(0)*100    
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

    integer :: i, j, ncolumnas, nfilas, err1, err2, err3
    character(len=30) :: arg
    real, allocatable :: arreglo1(:,:), arreglo2(:,:), resultado(:,:)

    err1=1
    err2=1
    err3=1

    if(argc().eq.0) then
        print *, "Multiplicacion de Matrices Cuadradas, ingrese : nfilas,ncolumnas >> "
        read *, ncolumnas, nfilas
        print *,"El numero de filas y columnas seleccionadas son >> I3 filas y I3 columnas", nfilas, ncolumnas
    else
        CALL getarg(1, arg)
        
    end if




    print *, "Multiplicacion de Matrices Cuadradas, ingrese : nfilas,ncolumnas >> "
    read *, ncolumnas, nfilas
    print *,"El numero de filas y columnas seleccionadas son >> I3 filas y I3 columnas", nfilas, ncolumnas

    if(nfilas.eq.ncolumnas) then
        allocate (arreglo1(nfilas,ncolumnas), stat=err1)  
        allocate (arreglo2(nfilas,ncolumnas), stat=err2)  
        allocate (resultado(nfilas,ncolumnas), stat=err3)

        if(err1.gt.0 .and. err2.gt.0 .and. err3.gt.0) then
            print *,"Fracaso"
        end if

        print *,"Ingrese elementos de la PRIMERA matriz cuadrada >>"
        call read_matrix(arreglo1, nfilas, ncolumnas)
        print *,"Ingrese elementos de la SEGUNDA matriz cuadrada >>"
        call read_matrix(arreglo2, nfilas, ncolumnas)

        ! print *,"Ingrese elementos de la PRIMERA matriz cuadrada >>"
        ! call rand_matrix(arreglo1, nfilas, ncolumnas)
        ! print *,"Ingrese elementos de la SEGUNDA matriz cuadrada >>"
        ! call rand_matrix(arreglo2, nfilas, ncolumnas)

        print *

        print *,"PRIMERA matriz cuadrada ingresada >>"
        call print_matrix(arreglo1, nfilas, ncolumnas)
        print *,"SEGUNDA matriz cuadrada ingresada >>"
        call print_matrix(arreglo2, nfilas, ncolumnas)

        call multi_matrix(arreglo1, arreglo2, nfilas, ncolumnas, resultado)

        print *
        print *,"RESULTADO de la Multiplicacion de Matrices >>"
        call print_matrix(resultado, nfilas, ncolumnas)

        deallocate(arreglo1)
        deallocate(arreglo2)
        deallocate(resultado)
    else
        print *,"La dimension de las matrices  no es cuadrada"
    end if

end program multimatrix