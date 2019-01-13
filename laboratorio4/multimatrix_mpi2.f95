module matrix

    implicit none
    real(8), allocatable :: ma(:,:), mb(:,:), mr(:,:)
    real(8), allocatable :: ra(:,:), rb(:,:), rr(:,:), ta(:,:)
    integer :: ni, nj, nk, i, j, k

contains
 
subroutine print_matrix()

    101 format("El orden seleccionado para la PRIMERA matrix es   >> (",I4,",",I4,") ")
    102 format("El orden seleccionado para la SEGUNDA matrix es   >> (",I4,",",I4,") ")
    103 format("El orden seleccionado para la matrix RESULTADO es >> (",I4,",",I4,") ")

    write(*,101) ni, nj
    do i=1, ni
        do j=1, nj
            write (*, "(F8.2)", advance="no") ma(i, j)
        end do
        print *
    end do

    write(*,102) nj, nk
    do i=1, nj
        do j=1, nk
            write (*, "(F8.2)", advance="no") mb(i, j)
        end do
        print *        
    end do

    write(*,103) ni, nk
    do i=1, ni
        do j=1, nk
            write (*, "(F8.2)", advance="no") mr(i, j)
        end do        
        print *
    end do

end subroutine print_matrix

subroutine allocate_matrix() 
    allocate(ma(ni,nj))  
    allocate(mb(nj,nk))  
    allocate(mr(ni,nk))
    
    allocate(ra(1,ni*nj))  
    allocate(rb(1,nj*nk))  
    allocate(rr(1,ni*nk))

    allocate(ta(nj,ni))  
end subroutine

subroutine  deallocate_matrix()
    deallocate(ma)
    deallocate(mb)
    deallocate(mr)
end subroutine

subroutine rand_ma()
    do j=1, nj
        do i=1, ni        
            ma(i, j)=rand(0)*10+1   
        end do
    end do
end subroutine rand_ma

subroutine rand_mb()
    do j=1, nk
        do i=1, nj
            mb(i, j)=rand(0)*10+1                       
        end do
    end do
end subroutine rand_mb

subroutine rand_mr()
    do j=1, nk
        do i=1, ni
            mr(i, j)=0                        
        end do
    end do    
end subroutine rand_mr

subroutine reshape_matrix()
    ta = transpose(ma)
    ra = reshape(source=ta, shape=[1, ni*nj])
    rb = reshape(source=mb, shape=[1, nj*nk])
end subroutine reshape_matrix

end module matrix

program main
    use matrix
    use mpi
    implicit none

    integer, parameter :: MASTER=0
    integer :: ierr, proceso, np, ne, p, h, g, f, flg, re, pflag, ini, fin, len
    integer :: status(MPI_STATUS_SIZE)
    real(8) :: start, finish, nb

    real(8), allocatable :: a(:), b(:), r(:)

    104 format("Multiplicacion terminada en >> ",F10.3," segundos") 
    105 format("Tamano de  memoria asignada >> ",F12.0," bytes")
    106 format("Resevacion mem terminada en >> ",F10.3," segundos") 
    107 format("Asig aleatoria terminada en >> ",F10.3," segundos") 
    108 format("Transformacion terminada en >> ",F10.3," segundos") 

    call config_args()
    flg = 0

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,proceso,ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,np,ierr)
    
    !calulamos el numero elementos por proceso
    ne = int(ni*nk/np)

    !calulamos si existe el resto de elementos
    re = mod(ni*nk,np)

    if(ne.lt.1) then
        print *, "Error numero de elementos vacio"
        stop 1
    end if

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

        call descompose_matrix()

        call dot_product_matrix(proceso, ne)

        call descompose_resto_matrix()
        
    else

        call assignation_matrix()
   
    end if

    !Si hay resto, el ultimo proceso termina con la multiplicacion
    if(proceso.eq.(np-1) .and. re.gt.0) then
        ! call assignation_matrix()
    end if

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)

    !Para el proceso 0, compone los resultados parciales en la matrix resultado
    if(proceso.eq.MASTER) then
     
        mr = reshape(source=rr, shape=[ni, nk])

        !calcula el tiempo que toma el proceso y la memoria asignada
        call cpu_time(finish)
        write(*,104) finish-start

        nb=(ni*nj+nj*nk+ni*nk)*8. !c_sizeof(real(8)) = 8
        write(*,105) nb
        print *    

        if(pflag.ge.1) then
            call print_matrix()        
        end if

        call deallocate_matrix()

    end if

    call MPI_FINALIZE(ierr)

contains

    !El proceso 0 envia a cada proceso una parte de la matrix proporcional al np
    subroutine descompose_matrix()

        do p=1,np-1           
            call compose_a(p)
            call send_a(p)   

            call compose_b(p)
            call send_b(p)
        end do 

    end subroutine descompose_matrix

    subroutine descompose_resto_matrix()

        if(re.ne.0) then
            call compose_resto_a()
            call send_a(np-1)

            call compose_resto_b()
            call send_b(np-1)
        end if

    end subroutine descompose_resto_matrix

    subroutine compose_a(p)
        integer :: p
        if(ne.gt.ni) then
            ini = 1
            fin = ni
        else if(ne.eq.ni) then
            ini = 1
            fin = ni
        else if(ne.lt.ni) then
            ini = p*ne+1
            fin = (p+1)*ne
        end if
        call allocate_a(ini, fin)
    end subroutine compose_a

     subroutine compose_resto_a()
        if(ne.gt.ni) then
            ini = 1
            fin = ni
        else if(ne.eq.ni) then
            ini = 1
            fin = re
        else if(ne.lt.ni) then
            ini = np*ne+1
            fin = np*ne+re
        end if
        call allocate_a(ini, fin)
    end subroutine compose_resto_a

    subroutine compose_b(p)
        integer :: p
        if(ne.gt.ni) then
            ini = p*int(ne/ni)+1
            fin = (p+1)*int(ne/ni)+mod(ne,ni)
        else if(ne.eq.ni) then
            ini = p+1
            fin = p+1
        else if(ne.lt.ni) then
            ini = int((p*ne)/ni)+1
            fin = int(((p+1)*ne-1)/ni)+1
        end if
        call allocate_b(ini, fin)
    end subroutine compose_b

     subroutine compose_resto_b()
        integer :: p 
            if(ne.gt.ni) then
                ini = np*int(ne/ni)+1
                fin = np*int(ne/ni)+re
            else if(ne.eq.ni) then
                ini = np+1
                fin = np+1
            else if(ne.lt.ni) then
                ini = int((np*ne)/ni)+1
                fin = int((np*ne+re-1)/ni)+1
            end if
            call allocate_b(ini, fin)
    end subroutine compose_resto_b

    !El proceso 0, realiza la multiplicacion correspondiente a su parte
    subroutine dot_product_matrix(p, len)
        integer :: p, len

        compose_a(p)
        compose_b(p)
        call dot_product_ab(len)
        call compose_matrix(p, len)

    end subroutine dot_product_matrix

    subroutine compose_matrix(p, len)
        integer :: p, len
        rr(1,p*len+1:(p+1)*len)=r(1:len)
    end subroutine compose_matrix

    subroutine send_a(p)
        integer :: p, tag

        if(re.eq.0) then
            if(ne.gt.ni) then
                tag = 11000
            else if(ne.eq.ni) then
                tag = 21000
            else if(ne.lt.ni) then
                tag = 31000
            end if
        else
            if(ne.gt.ni) then
                !no se vuelve enviar
            else if(ne.eq.ni) then
                tag = 23000
            else if(ne.lt.ni) then
                tag = 33000
            end if
        end if

        ! print *, "a(ini): ", ini, ", a(fin): ", fin
        call MPI_SEND(a,len,MPI_REAL8,p,tag+p,MPI_COMM_WORLD,ierr)
    end subroutine send_a

    subroutine allocate_a(ini, fin)
        integer :: ini, fin
        ini=nj*(ini-1)+1
        fin=nj*(fin-1)+nj
        len = fin-(ini-1)
        ! print *, "a(ini): ", ini, ", a(fin): ", fin
        if(allocated(a)) then
            deallocate(a)
        end if
        allocate(a(len))
        a=ra(1,ini:fin)
    end subroutine

    subroutine send_b(p)
        integer :: p, tag

        if(re.eq.0) then
            if(ne.gt.ni) then
                tag = 12000
            else if(ne.eq.ni) then
                tag = 22000
            else if(ne.lt.ni) then
                tag = 32000
            end if
        else
            if(ne.gt.ni) then
                tag = 13000
            else if(ne.eq.ni) then
                tag = 24000
            else if(ne.lt.ni) then
                tag = 34000
            end if
        end if

        call MPI_SEND(rb(1,ini:fin),len,MPI_REAL8,p,tag,MPI_COMM_WORLD,ierr)
    end subroutine send_b

    subroutine allocate_b(ini, fin)
        integer :: ini, fin
        ini=nj*(ini-1)+1
        fin=nj*(fin-1)+nj
        len = fin-(ini-1)
        if(allocated(b)) then
            deallocate(b)
        end if
        allocate(b(len))
        b=rb(1,ini:fin)
    end subroutine

    subroutine dot_product_ab(len)
        integer :: len

        if(allocated(r)) then
            deallocate(r)
        end if
        allocate(r(len))

        h=1

        if(ne.gt.ni) then

            do k=1,len
                do i=1,ni
                    r(h)=dot_product(a((i-1)*nj+1:i*nj+1),b((k-1)*nj+1:k*nj+1))
                    h=h+1

                    if(h.gt.len) then
                        exit
                    end if
                end do

                if(h.gt.len) then
                    exit
                end if
            end do

        end if

    end subroutine dot_product_ab

    subroutine assignation_matrix()
        !Para cada proceso diferente del proceso 0, se recibe y guarda una parte
        ! de la matrix proporcional al np
        if(ne.gt.ni) then            
            call recv_ra(ni,MASTER,11000+proceso)
            call recv_rb(ne,MASTER,12000+proceso)

            if(re.ne.0 .and. proceso.eq.(np-1)) then
                call recv_rb(re,MASTER,13000+proceso)
            end if

        else if(ne.eq.ni) then
            call recv_ra(ni,MASTER,21000+proceso)
            call recv_rb(1,MASTER,22000+proceso)

            if(re.ne.0 .and. proceso.eq.(np-1)) then
                call recv_ra(re,MASTER,23000+proceso)
                call recv_rb(1,MASTER,24000+proceso)
            end if

        else if(ne.lt.ni) then
            call recv_ra(ne,MASTER,31000+proceso)
            len = int(((proceso+1)*ne-1)/ni)-int((proceso*ne)/ni)+1
            call recv_rb(len,MASTER,32000+proceso)

            if(re.ne.0 .and. proceso.eq.(np-1)) then
                call recv_ra(re,MASTER,33000+proceso)
                len = int((np*ne+re-1)/ni)-int((np*ne)/ni)+1      
                call recv_rb(len,MASTER,34000+proceso)
            end if

        end if
    end subroutine assignation_matrix


    subroutine recv_ra(lena, rank, tag)
        integer :: lena, rank, tag
        len=lena*nj
        allocate(a(len))
        call MPI_RECV(a,len,MPI_REAL8,rank,tag,MPI_COMM_WORLD,status,ierr)
        deallocate(a)
    end subroutine recv_ra

    subroutine recv_rb(lenb, rank, tag)
        integer :: lenb, rank, tag
        len=lenb*nj
        allocate(b(len))
        call MPI_RECV(b,len,MPI_REAL8,rank,tag,MPI_COMM_WORLD,status,ierr)
        deallocate(b)
    end subroutine recv_rb

    subroutine config_args()
        integer :: e, ei, ej, ek, x
        character(len=40) :: arg  
        character(len=10), dimension(3) :: args

        100 format("Ingrese orden de las matrices i,j,k")

        ni=1
        nj=1
        nk=1
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
                read(args(1), *, IOSTAT=ei)ni
                if(ei.ne.0) then
                    write (*, 100)
                    stop 1
                end if

                read(args(2), *, IOSTAT=ej)nj            
                if(ej.ne.0) then
                    write (*, 100)
                    stop 2
                end if

                read(args(3), *, IOSTAT=ek)nk            
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
    end subroutine config_args

end program main