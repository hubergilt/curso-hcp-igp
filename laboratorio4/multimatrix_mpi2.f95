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
            ! ma(i, j)=rand(0)*10+1
            ma(i, j)=1
        end do
    end do
end subroutine rand_ma

subroutine rand_mb()
    do j=1, nk
        do i=1, nj
            ! mb(i, j)=rand(0)*10+1                     
            mb(i, j)=1
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
    108 format("Conv a arreglo terminada en >> ",F10.3," segundos") 
    109 format("Descomposicion terminada en >> ",F10.3," segundos") 
    110 format("Mult proceso 0 terminada en >> ",F10.3," segundos")     
    111 format("Comp proceso 0 terminada en >> ",F10.3," segundos")     

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
        call cpu_time(finish)
        write(*,109) finish-start

        call cpu_time(start)
        !El proceso 0, realiza la multiplicacion correspondiente a su parte
        call compose_ab(proceso)
        call dot_product_ab(ne)

        ! print *, "proceso", proceso, "a", a
        ! print *, "proceso", proceso, "b", b
        ! print *, "proceso", proceso, "r", r
        
        call cpu_time(finish)
        write(*,110) finish-start

        call cpu_time(start) 
        call recompose_matrix(proceso,ne)
        call cpu_time(finish)
        write(*,111) finish-start

        call cpu_time(start)

    else

        ! print *, "proceso", proceso
        call assignation_ab(proceso)
        ! print *, "a", a
        ! print *, "b", b
        
        call dot_product_ab(ne)


        call send_result(MASTER,ne)
        ! print *, "proceso", proceso, "a", a
        ! print *, "proceso", proceso, "b", b
        ! print *, "proceso", proceso, "r", r

    end if

    !El proceso 0, recompone los resultados parciales
    if(proceso.eq.MASTER) then
        do p=1, np-1
            call recv_result(p, ne)
            call recompose_matrix(p,ne)
        end do
    end if

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)

    ! Si hay resto, el ultimo proceso termina con la multiplicacion
    if(re.gt.0) then
        if(proceso.eq.(MASTER)) then 
            call descompose_residue_matrix()                      
        else if(proceso.eq.(np-1)) then
            call assignation_residue_ab(proceso)
            call dot_product_ab(re)            
            call send_result(MASTER,re)
        end if

        call MPI_BARRIER(MPI_COMM_WORLD,ierr)         

        if(proceso.eq.MASTER) then

            call recv_result(np-1, re)                
            call recompose_residue_matrix(np, ne, re)

        end if
    end if

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)    

    !Para el proceso 0, devuelve la dimension original de la matrix
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
            ! print *, "proceso", proceso,"p",p, "a", a
            call send_a(p)

            call compose_b(p)
            ! print *, "p : ", p, "b : ", b            
            call send_b(p)
        end do 

    end subroutine descompose_matrix

    subroutine descompose_residue_matrix()

        if(re.ne.0) then
            call compose_residue_a()
            call send_residue_a(np-1)

            call compose_residue_b()
            call send_residue_b(np-1)
        end if

    end subroutine descompose_residue_matrix

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
        ! print *, "a(ini): ", ini, ", a(fin): ", fin        
        call allocate_a(ini, fin)
        ! print *, "ini a", a
        ! print *, "proceso", proceso,"p",p, "a", a
    end subroutine compose_a

     subroutine compose_residue_a()
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
    end subroutine compose_residue_a

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
        ! print *, "b(ini): ", ini, ", b(fin): ", fin        
        call allocate_b(ini, fin)
    end subroutine compose_b

     subroutine compose_residue_b()
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
    end subroutine compose_residue_b

    subroutine compose_ab(p)
        integer :: p

        call compose_a(p)
        call compose_b(p)

    end subroutine compose_ab

    subroutine recompose_matrix(p, len)
        integer :: p, len
        rr(1,p*len+1:(p+1)*len)=r(1:len)
    end subroutine recompose_matrix

    subroutine recompose_residue_matrix(p, len, re)
        integer :: p, len, re
        rr(1,p*len+1:p*len+re)=r(1:re)
    end subroutine recompose_residue_matrix

    subroutine send_result(p, len)
        integer :: p, len, tag

        tag = 15000
        call MPI_SEND(r,len,MPI_REAL8,p,tag+p,MPI_COMM_WORLD,ierr)
    end subroutine

    subroutine recv_result(p, len)
        integer :: p, len, tag

        if(allocated(r)) then
            deallocate(r)
        end if
        allocate(r(len))

        tag = 15000
        call MPI_RECV(r,len,MPI_REAL8,p,tag,MPI_COMM_WORLD,status,ierr)        
    end subroutine

    subroutine send_residue_a(p)
        integer :: p, tag

        if(ne.gt.ni) then
            tag = 13000
        else if(ne.eq.ni) then
            tag = 23000
        else if(ne.lt.ni) then
            tag = 33000
        end if

        ! print *, "a(ini): ", ini, ", a(fin): ", fin
        call MPI_SEND(a,len,MPI_REAL8,p,tag+p,MPI_COMM_WORLD,ierr)
    end subroutine send_residue_a

    subroutine send_b(p)
        integer :: p, tag

        if(ne.gt.ni) then
            tag = 12000
        else if(ne.eq.ni) then
            tag = 22000
        else if(ne.lt.ni) then
            tag = 32000
        end if

        call MPI_SEND(rb(1,ini:fin),len,MPI_REAL8,p,tag+p,MPI_COMM_WORLD,ierr)
    end subroutine send_b

    subroutine send_residue_b(p)
        integer :: p, tag

        if(ne.gt.ni) then
            tag = 14000
        else if(ne.eq.ni) then
            tag = 24000
        else if(ne.lt.ni) then
            tag = 34000
        end if

        call MPI_SEND(rb(1,ini:fin),len,MPI_REAL8,p,tag+p,MPI_COMM_WORLD,ierr)
    end subroutine send_residue_b

    subroutine allocate_a(ini, fin)
        integer :: ini, fin
        integer :: ini1, fin1, len1
        integer :: ini2, fin2, len2   

        ini2 = ini
        fin2 = fin

        ini1 = circular(ini2)
        fin1 = circular(fin2)

        if(allocated(a)) then
            deallocate(a)
        end if

        if(ini1.gt.fin1) then

            ini = ini1
            fin = ini1+(ne-fin1)-1

            call expand_nj(ini, fin, len)

            ini1 = ini
            fin1 = fin
            len1 = len

            ini1 = circular(ini2)
            fin1 = circular(fin2)

            ini = 1
            fin = fin1

            call expand_nj(ini, fin, len)

            ini2 = ini
            fin2 = fin
            len2 = len            

            allocate(a(len1+len2))

            a(1:len1)=ra(1,ini1:fin1)
            a(len1+1:len1+len2)=ra(1,ini2:fin2)
            len=len1+len2

        else

            call expand_nj(ini1, fin1, len1)
            allocate(a(len1))
            a=ra(1,ini1:fin1)
            len=len1

        end if 
       
    end subroutine allocate_a

    integer function circular(index)
        integer :: index
        if(mod(index,ni).eq.0) then
            circular=ni
        else
            circular=mod(index,ni)
        end if
    end function circular

    subroutine expand_nj(ini, fin, len)
        integer ini, fin, len
        ini=nj*(ini-1)+1        
        fin=nj*(fin-1)+nj        
        len = fin-(ini-1) 
        ! print *,"valores", ini, fin, len      
    end subroutine expand_nj

    subroutine allocate_b(ini, fin)
        integer :: ini, fin
        ! print *, "b(ini): ", ini, ", b(fin): ", fin        
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

        ! print *,"a", a
        ! print *,"b", b

        h=1

        do k=1,len
            do i=1,ni                           

                r(h)=dot_product(a((i-1)*nj+1:i*nj),b((k-1)*nj+1:k*nj))
                
                if(proceso.eq.3) then

                    ! print *,"h",h,"r(h)", r(h)
                    ! print *,"h",h,"a", a((i-1)*nj+1:i*nj)
                    ! print *,"h",h,"b", b((i-1)*nj+1:i*nj)
                    ! print *,"i",i,"k",k,"h",h,"a(ini):", (i-1)*nj+1,"a(fin):",i*nj

                end if

                h=h+1

                if(h.gt.len) then
                    exit
                end if

            end do

            if(h.gt.len) then
                exit
            end if
        end do

        ! print *,"r", r

    end subroutine dot_product_ab

    !Para cada proceso, se recibe y guarda una parte de la matrix proporcional al np
    subroutine assignation_ab(p)        
        integer :: p

        ! print *,"p", p

        if(ne.gt.ni) then            
            call recv_a(ni,MASTER)
            call recv_b(ne,MASTER)
        else if(ne.eq.ni) then
            call recv_a(ni,MASTER)
            call recv_b(1,MASTER)
        else if(ne.lt.ni) then
            call recv_a(ne,MASTER)
            len = int(((p+1)*ne-1)/ni)-int((p*ne)/ni)+1
            call recv_b(len,MASTER)
        end if
    end subroutine assignation_ab

    subroutine assignation_residue_ab(p)
        integer ::   p        
        if(re.ne.0) then
            if(ne.gt.ni) then
                call recv_residue_a(ni,MASTER)                
                call recv_residue_b(re,MASTER)
            else if(ne.eq.ni) then
                call recv_residue_a(re,MASTER)
                call recv_residue_b(1,MASTER)
            else if(ne.lt.ni) then
                call recv_residue_a(re,MASTER)
                len = int((np*ne+re-1)/ni)-int((np*ne)/ni)+1      
                call recv_residue_b(len,MASTER)
            end if
        end if
    end subroutine assignation_residue_ab

    subroutine send_a(p)
        integer :: p, tag

        if(ne.gt.ni) then
            tag = 11000
        else if(ne.eq.ni) then
            tag = 21000
        else if(ne.lt.ni) then
            tag = 31000
        end if

        ! print *, "send proceso", proceso,"p",p, "a", a,"tag",tag+p, "len", len
        call MPI_SEND(a,len,MPI_REAL8,p,tag+p,MPI_COMM_WORLD,ierr)
        ! print *, "send proceso", proceso,"p",p, "a", a,"tag",tag+p, "len", len        
    end subroutine send_a

    subroutine recv_a(lena, p)
        integer :: lena, p, tag
        if(ne.gt.ni) then
            tag = 11000
        else if(ne.eq.ni) then
            tag = 21000
        else if(ne.lt.ni) then
            tag = 31000
        end if
        len=lena*nj
        if(allocated(a)) then
            deallocate(a)
        end if
        allocate(a(len))
        ! print *, "recv proceso", proceso,"p",p, "a", a,"tag",tag+proceso, "len", len
        call MPI_RECV(a,len,MPI_REAL8,p,tag+proceso,MPI_COMM_WORLD,status,ierr)
        ! print *, "recv proceso", proceso,"p",p, "a", a,"tag",tag+proceso, "len", len
    end subroutine recv_a

    subroutine recv_residue_a(lena, p)
        integer :: lena, p, tag
        if(ne.gt.ni) then
            tag = 13000
        else if(ne.eq.ni) then
            tag = 23000
        else if(ne.lt.ni) then
            tag = 33000
        end if
        len=lena*nj
        if(allocated(a)) then
            deallocate(a)
        end if
        allocate(a(len))
        ! print *, "recv proceso", proceso,"p",p, "a", a,"tag",tag+proceso
        call MPI_RECV(a,len,MPI_REAL8,p,tag+proceso,MPI_COMM_WORLD,status,ierr)
        ! print *, "proceso", proceso,"p",p, "a", a,"tag",tag
    end subroutine recv_residue_a

    subroutine recv_b(lenb, p)
        integer :: lenb, p, tag 
        if(ne.gt.ni) then
            tag = 12000
        else if(ne.eq.ni) then
            tag = 22000
        else if(ne.lt.ni) then
            tag = 32000
        end if               
        len=lenb*nj
        if(allocated(b)) then
            deallocate(b)
        end if
        allocate(b(len))
        ! print *, "recv proceso", proceso,"p",p, "b", b,"tag",tag+proceso
        call MPI_RECV(b,len,MPI_REAL8,p,tag+proceso,MPI_COMM_WORLD,status,ierr)
    end subroutine recv_b

    subroutine recv_residue_b(lenb, p)
        integer :: lenb, p, tag 
        if(ne.gt.ni) then
            tag = 14000
        else if(ne.eq.ni) then
            tag = 24000
        else if(ne.lt.ni) then
            tag = 34000
        end if               
        len=lenb*nj
        if(allocated(b)) then
            deallocate(b)
        end if
        allocate(b(len))
        ! print *, "recv proceso", proceso,"p",p, "b", b,"tag",tag+proceso
        call MPI_RECV(b,len,MPI_REAL8,p,tag+proceso,MPI_COMM_WORLD,status,ierr)
    end subroutine recv_residue_b

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