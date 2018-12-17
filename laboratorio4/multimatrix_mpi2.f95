module matrix

    implicit none
    real(8), allocatable :: ma(:,:), mb(:,:), mr(:,:)
    real(8), allocatable :: ra(:,:), rb(:,:), rr(:,:), ta(:,:)
    integer :: ma_nfil, ma_ncol
    integer :: mb_nfil, mb_ncol
    integer :: i, j, k

contains
 
subroutine print_matrix()

    101 format("El orden seleccionado para la ",A10," matrix es >> (",I4,",",I4,") ")
    102 format("El resultado de la Multiplicacion de matrices es >> (",I4,",",I4,")")

    write(*,101) "PRIMERA", ma_nfil, ma_ncol
    do i=1, ma_nfil
        print *, (ma(i, j), j=1, ma_ncol)
    end do

    write(*,101) "SEGUNDA", mb_nfil, mb_ncol
    do i=1, mb_nfil
        print *, (mb(i, j), j=1, mb_ncol)
    end do

    write(*,102) ma_nfil, mb_ncol
    do i=1, ma_nfil
        print *, (mr(i, j), j=1, mb_ncol)
    end do

end subroutine print_matrix

subroutine allocate_matrix() 
    allocate(ma(ma_nfil,ma_ncol))  
    allocate(mb(mb_nfil,mb_ncol))  
    allocate(mr(ma_nfil,mb_ncol))
    
    allocate(ra(1,ma_nfil*ma_ncol))  
    allocate(rb(1,mb_nfil*mb_ncol))  
    allocate(rr(1,ma_nfil*mb_ncol))

    allocate(ta(mb_ncol,mb_nfil))  
end subroutine

subroutine  deallocate_matrix()
    deallocate(ma)
    deallocate(mb)
    deallocate(mr)
end subroutine

subroutine rand_ma()
    do j=1, ma_ncol
        do i=1, ma_nfil        
            ma(i, j)=rand(0)*10+1   
        end do
    end do
end subroutine rand_ma

subroutine rand_mb()
    do j=1, mb_ncol
        do i=1, mb_nfil
            mb(i, j)=rand(0)*10+1                       
        end do
    end do
end subroutine rand_mb

subroutine rand_mr()
    do j=1, mb_ncol
        do i=1, ma_nfil
            mr(i, j)=0                        
        end do
    end do    
end subroutine rand_mr

subroutine reshape_matrix()
    ta = transpose(ma)
    ra = reshape(source=ta, shape=[1, ma_nfil*ma_ncol])
    rb = reshape(source=mb, shape=[1, mb_nfil*mb_ncol])
end subroutine reshape_matrix

end module matrix

program main
    use matrix
    use mpi
    implicit none

    integer, parameter :: MASTER=0
    integer :: ierr,proceso,num_procesos, ne, p, h, g, f, flg, resto, pflag
    integer :: status(MPI_STATUS_SIZE)
    real(8) :: start, finish, nb

    real(8), allocatable :: a(:), b(:), r(:)

    104 format("Multiplicacion de matrices terminada en     >> ",F10.3," segundos") 
    105 format("Tamano del problema, suma memoria asignada  >> ",F12.0," bytes")
    106 format("Resevacion dinamica de memoria terminada en >> ",F10.3," segundos") 
    107 format("Asignacion aleatoria de valores terminada en>> ",F10.3," segundos") 
    108 format("Transformacion de dimensiones terminada en  >> ",F10.3," segundos") 

    call config_args()
    flg = 0

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,proceso,ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,num_procesos,ierr)
    
    !calulamos el numero elementos por proceso
    ne = int(ma_nfil*mb_ncol/num_procesos)

    !calulamos si existe el resto de elementos
    resto = mod(ma_nfil*mb_ncol,num_procesos)

    if(ne.lt.1) then
        print *, "Error numero de elementos vacio"
        stop 1
    end if

    allocate(r(ne)) 

    !El proceso 0, reparte trabajos a los demas procesos
    if(proceso.eq.MASTER) then
        call cpu_time(start)
        call allocate_matrix()
        call cpu_time(finish)
        write(*,106) finish-start

        call cpu_time(start)
        call rand_ma()
        call rand_mb()
        call rand_mr()
        call cpu_time(finish)
        write(*,107) finish-start

        call cpu_time(start)
        call reshape_matrix()
        call cpu_time(finish)
        write(*,108) finish-start

        call cpu_time(start)

        !El proceso 0 envia a cada proceso una parte de la matrix proporcional al num_procesos        
        do p=1,num_procesos-1
            call MPI_SEND(ra(1,:),ma_nfil*ma_ncol,MPI_REAL8,p,1000+p,MPI_COMM_WORLD,ierr)
            g=p*ne+1
            call MPI_SEND(rb(1,g:g+(ma_ncol-1)),ma_ncol,MPI_REAL8,p,2000+p,MPI_COMM_WORLD,ierr)
        end do

        !El proceso 0, realiza la multiplicacion correspondiente a su parte
        g=proceso*ne+1
        j=int(ne/ma_ncol)
        do h=1,ne
            f=mod((h-1),j)*ma_ncol+1
            print *, "ra(1,f:f+(ma_ncol-1)) :", ra(1,f:f+(ma_ncol-1)), "f",f,"->", f+(ma_ncol-1)
            ! print *, "rb(1,g:g+(ma_ncol-1)) :", rb(1,g:g+(ma_ncol-1)), "g",g, g+(ma_ncol-1)
            r(h)=dot_product(ra(1,f:f+(ma_ncol-1)),rb(1,g:g+(ma_ncol-1)))
        end do

        !Si hay resto, el proceso 0 envia el resto al ultimo proceso
        if(resto.gt.0) then
            p=num_procesos-1
            g=(p+1)*ne+1
            call MPI_SEND(rb(1,g:g+(resto-1)),resto,MPI_REAL8,p,4000+p,MPI_COMM_WORLD,ierr)
        end if

    else
        !Para cada proceso diferente del proceso 0, se recibe y guarda una parte
        ! de la matrix proporcional al num_procesos
        allocate(a(ma_nfil*ma_ncol))
        allocate(b(ma_ncol))

        call MPI_RECV(a,ma_nfil*ma_ncol,MPI_REAL8,0,1000+proceso,MPI_COMM_WORLD,status,ierr)
        call MPI_RECV(b,ma_ncol,MPI_REAL8,0,2000+proceso,MPI_COMM_WORLD,status,ierr)

        do h=1,ne
            f=(h-1)*ma_ncol+1
            r(h)=dot_product(a(f:f+(ma_ncol-1)),b)
        end do

        ! do p=1,num_procesos-1
        !     do h=1,ne
        !         f=(h-1)*ma_ncol+1
        !         r(h)=dot_product(a(f:f+(ma_ncol-1)),b)
        !     end do
        ! end do

        !devuelve el resultado de cada proceso
        call MPI_SEND(r,ne,MPI_REAL8,0,3000+proceso,MPI_COMM_WORLD,ierr)

        if (resto.eq.0 .and. proceso.ne.(num_procesos-1)) then
            deallocate(a)
        end if    
        deallocate(b)    

    end if

    !Si hay resto, el ultimo proceso termina con la multiplicacion
    if(proceso.eq.(num_procesos-1) .and. resto.gt.0) then

        allocate(b(resto))
        allocate(r(resto))

        call MPI_RECV(b,resto,MPI_REAL8,0,4000+proceso,MPI_COMM_WORLD,status,ierr)

        do h=1,resto
            g=(proceso+1)*ne+h
            ! f=(h-1)*resto+1
            r(h)=dot_product(a(g:g+(resto-1)),b)
        end do

        !El ultimo proceso, envia el resultado al proceso 0
        call MPI_SEND(r,resto,MPI_REAL8,0,5000+proceso,MPI_COMM_WORLD,ierr)

        deallocate(a)
        deallocate(b)
        deallocate(r)
    end if

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)

    !Para el proceso 0, compone los resultados parciales en la matrix resultado
    if(proceso.eq.MASTER) then
        
        !Se guarda el resultado del proceso 0
        g=proceso*ne+1
        rr(1,g:g+(ne-1))=r(1:ne)
        ! rr(1,g:g+(ne-1))=0

        !Se guarda el resultado del demas procesos diferente al proceso 0
        do p=1,num_procesos-1
            call MPI_RECV(r,ne,MPI_REAL8,p,3000+p,MPI_COMM_WORLD,status,ierr)
            g=p*ne+1
            rr(1,g:g+(ne-1))=r(1:ne)
            ! rr(1,g:g+(ne-1))=0
        end do

        !Si hay resto, se recibe y guarda el resultado del ultimo proceso
        if(resto.gt.0) then
            deallocate(r)
            allocate(r(resto))
            p=num_procesos-1
            call MPI_RECV(r,resto,MPI_REAL8,p,5000+p,MPI_COMM_WORLD,status,ierr)
            g=p*ne+1
            rr(1,g:g+(resto-1))=r(1:resto)
            ! rr(1,g:g+(resto-1))=0
        end if     

        mr = reshape(source=rr, shape=[ma_nfil, mb_ncol])

        !calcula el tiempo que toma el proceso y la memoria asignada
        call cpu_time(finish)
        write(*,104) finish-start

        nb=(ma_nfil*ma_ncol+mb_nfil*mb_ncol+ma_nfil*mb_ncol)*8. !c_sizeof(real(8)) = 8
        write(*,105) nb
        print *    

        if(pflag.ge.1) then
            call print_matrix()        
        end if

        call deallocate_matrix()

    end if

    deallocate(r)        

    call MPI_FINALIZE(ierr)

contains

    subroutine config_args()
        integer :: e, ei, ej, ek, x
        character(len=40) :: arg  
        character(len=10), dimension(3) :: args

        100 format("Ingrese orden de las matrices i,j,k")

        i=1
        j=1
        k=1
        start = 0
        finish = 0
        nb = 0
        pflag = 1

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
                        pflag = 0              
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
    end subroutine config_args

end program main