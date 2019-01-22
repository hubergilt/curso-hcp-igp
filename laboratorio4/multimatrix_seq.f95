module matrix

    implicit none
    real, parameter:: MA__NFIL = 4
    real, parameter:: MA__NCOL = 4
    real, parameter:: MB__NFIL = 4
    real, parameter:: MB__NCOL = 4
    real(8), allocatable :: ma(:,:)
    real(8), allocatable :: mb(:,:)
    real(8), allocatable :: mr(:,:)
    integer :: ma_nfil
    integer :: ma_ncol
    integer :: mb_nfil
    integer :: mb_ncol
    integer :: i, j, k, e, ei, ej, ek, flag
    character(len=40) :: arg  
    character(len=10), dimension(3) :: args

contains

subroutine print_matrix()

    101 format("El orden seleccionado para la PRIMERA matrix es   >> (",I4,",",I4,") ")
    102 format("El orden seleccionado para la SEGUNDA matrix es   >> (",I4,",",I4,") ")
    103 format("El orden seleccionado para la matrix RESULTADO es >> (",I4,",",I4,") ")

    write(*,101) ma_nfil, ma_ncol
    do i=1, ma_nfil
        do j=1, ma_ncol
            write (*, "(F8.2)", advance="no") ma(i, j)
        end do
        print *
    end do

    write(*,102) mb_nfil, mb_ncol
    do i=1, mb_nfil
        do j=1, mb_ncol
            write (*, "(F8.2)", advance="no") mb(i, j)
        end do
        print *        
    end do

    write(*,103) ma_nfil, mb_ncol
    do i=1, ma_nfil
        do j=1, mb_ncol
            write (*, "(F8.2)", advance="no") mr(i, j)
        end do        
        print *
    end do

end subroutine print_matrix

subroutine allocate_matrix() 

    integer :: err1
    103 format ("Error en la asignacion de memoria para ",A10," matrix con ",I4," filas y con ",I4," columnas")

    allocate (ma(ma_nfil,ma_ncol), stat=err1)  
    if(err1.gt.0) then 
        write (*, 103) "PRIMERA", ma_nfil, ma_ncol
    end if

    allocate (mb(mb_nfil,mb_ncol), stat=err1)  
    if(err1.gt.0) then 
        write (*, 103) "SEGUNDA", mb_nfil,mb_ncol
    end if

    allocate (mr(ma_nfil,mb_ncol), stat=err1)  
    if(err1.gt.0) then 
        write (*, 103) "RESULTADO", ma_nfil,mb_ncol
    end if

end subroutine

subroutine  deallocate_matrix()
    deallocate(ma)
    deallocate(mb)
    deallocate(mr)
end subroutine


subroutine rand_matrix()

    do j=1, ma_ncol
        do i=1, ma_nfil        
            ma(i, j)=rand(0)*10+1   
            ! ma(i, j)=1
        end do
    end do

    do j=1, mb_ncol
        do i=1, mb_nfil
            mb(i, j)=rand(0)*10+1                       
            ! mb(i, j)=1
        end do
    end do

    do j=1, mb_ncol
        do i=1, ma_nfil
            mr(i, j)=0                        
        end do
    end do    

end subroutine rand_matrix


subroutine product_matrix()
    do j=1, mb_ncol
        do i=1, ma_nfil
            do k=1, ma_ncol
                mr(i, j)=mr(i, j)+ma(i, k)*mb(k, j)
            end do
        end do
    end do      
end subroutine

end module matrix

program multimatrix_seq
    use matrix
    implicit none

    real :: start, finish, nb

    104 format("Multiplicacion de matrices terminada en    >> ",F10.3," segundos") 
    105 format("Tamaño del problema, suma memoria asignada >> ",F12.0," bytes")

    call multimatrix_init()

    call allocate_matrix()    
    call rand_matrix()
    call cpu_time(start)
    call product_matrix()
    call cpu_time(finish)
    write(*,104) finish-start

    nb=(ma_nfil*ma_ncol+mb_nfil*mb_ncol+ma_nfil*mb_ncol)*8. ! c_sizeof(real(8))=8
    write(*,105) nb
    print *

    if(flag.ge.1) then
        call print_matrix()        
    end if

    call deallocate_matrix()

contains

    subroutine multimatrix_init()
        integer :: x

        100 format("Ingrese orden de las matrices i,j,k")

        ma_nfil=MA__NFIL
        ma_ncol=MA__NCOL
        mb_nfil=MB__NFIL
        mb_ncol=MB__NCOL

        i=1
        j=1
        k=1
        start = 0
        finish = 0
        nb = 0
        flag = 1

        if(iargc().eq.0) then
            write (*, 100)
            stop 1
        else
            CALL getarg(1, arg)        
            read(arg, *, IOSTAT=e) args

            if (e.eq.0) then 
                read(args(1), *, IOSTAT=ei)i
                if(ei.ne.0) then
                    write (*, 100)
                    stop 1
                end if

                read(args(2), *, IOSTAT=ej)j            
                if(ej.ne.0) then
                    write (*, 100)
                    stop 2
                end if

                read(args(3), *, IOSTAT=ek)k            
                if(ek.ne.0) then
                    write (*, 100)
                    stop 3
                end if
            else
                write (*, 100)
                stop 1 
            end if

            if(iargc().eq.2) then
                CALL getarg(2, arg)  
                read(arg, *, IOSTAT=e)x
                if (e.eq.0) then
                    if(x.ge.1) then 
                        flag = 0              
                    end if
                else
                    write (*, 100)
                    stop 1
                end if           
            end if
        end if

        !print *, i, j, k

        ma_nfil=i
        ma_ncol=j
        mb_nfil=j
        mb_ncol=k
    end subroutine multimatrix_init

end program multimatrix_seq
