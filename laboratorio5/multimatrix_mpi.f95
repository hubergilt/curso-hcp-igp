module matrix
    use hdf5
    implicit none
    real(8), allocatable :: ma(:,:), mb(:,:), mr(:,:)
    real(8), allocatable :: ra(:,:), rb(:,:), rr(:,:), ta(:,:)
    integer :: ni=2, nj=2, nk=2, i, j, k

contains
 
subroutine print_matrix()
    call print_ma()
    call print_mb()
    call print_mr()
end subroutine print_matrix

subroutine print_ma()

    101 format("El orden seleccionado para la PRIMERA matrix es   >> (",I4,",",I4,") ")

    write(*,101) ni, nj
    do i=1, ni
        do j=1, nj
            write (*, "(F8.2)", advance="no") ma(i, j)
        end do
        print *
    end do
  
end subroutine print_ma

subroutine print_mb()

    102 format("El orden seleccionado para la SEGUNDA matrix es   >> (",I4,",",I4,") ")

    write(*,102) nj, nk
    do i=1, nj
        do j=1, nk
            write (*, "(F8.2)", advance="no") mb(i, j)
        end do
        print *
    end do
  
end subroutine print_mb

subroutine print_mr()

    103 format("El orden seleccionado para la matrix RESULTADO es >> (",I4,",",I4,") ")

    write(*,103) ni, nk
    do i=1, ni
        do j=1, nk
            write (*, "(F8.2)", advance="no") mr(i, j)
        end do        
        print *
    end do
  
end subroutine print_mr

subroutine allocate_matrix() 
    allocate(ma(ni,nj))  
    allocate(mb(nj,nk))  
    allocate(mr(ni,nk))
    
    allocate(ra(1,ni*nj))  
    allocate(rb(1,nj*nk))  
    allocate(rr(1,ni*nk))

    allocate(ta(nj,ni)) 

end subroutine allocate_matrix

subroutine  deallocate_matrix()
    deallocate(ma)
    deallocate(mb)
    deallocate(mr)
end subroutine deallocate_matrix

subroutine rand_ma()
    do j=1, nj
        do i=1, ni        
            ma(i, j)=rand(0)*10+1
            ! ma(i, j)=1
            ! if (i.eq.j) then
            !     ma(i, j)=1
            ! else 
            !     ma(i, j)=0
            ! end if
        end do
    end do
end subroutine rand_ma

subroutine rand_mb()
    do j=1, nk
        do i=1, nj
            mb(i, j)=rand(0)*10+1                     
            ! mb(i, j)=1
            ! if (i.eq.j) then
            !     mb(i, j)=1
            ! else
            !     mb(i, j)=0
            ! end if
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

subroutine rand_matrix()
    call rand_ma()
    call rand_mb()
    call rand_mr()
end subroutine rand_matrix

subroutine reshape_matrix()
    ta = transpose(ma)
    ra = reshape(source=ta, shape=[1, ni*nj])
    rb = reshape(source=mb, shape=[1, nj*nk])
end subroutine reshape_matrix

subroutine savehdf5()
    character(len=14), parameter :: filename = "multimatrix.h5" ! Nombre del archivo
    character(len=12), parameter :: dsetname1 = "Matrix ma" ! Nombre del Dataset
    character(len=12), parameter :: dsetname2 = "Matrix mb" ! Nombre del Dataset
    character(len=12), parameter :: dsetname3 = "Matrix mr" ! Nombre del Dataset

    integer(hid_t) :: file_id
    integer(hid_t) :: dset1_id, dset2_id, dset3_id
    integer(hid_t) :: dspace1_id, dspace2_id, dspace3_id

    integer (hsize_t) , dimension(2) :: dims
    integer :: rank = 2
    integer :: error

    call h5open_f(error)

        call h5fcreate_f (filename,H5F_ACC_TRUNC_F,file_id,error)

            dims = [ni, nj]
            call h5screate_simple_f(rank,dims,dspace1_id,error)

                call h5dcreate_f(file_id,dsetname1,H5T_NATIVE_DOUBLE,dspace1_id,dset1_id,error)

                call h5dwrite_f(dset1_id,H5T_NATIVE_DOUBLE,ma,dims,error)

                call h5dclose_f(dset1_id,error)

            call h5sclose_f(dspace1_id,error)


            dims = [nj, nk]
            call h5screate_simple_f(rank,dims,dspace2_id,error)

                call h5dcreate_f(file_id,dsetname2,H5T_NATIVE_DOUBLE,dspace2_id,dset2_id,error)

                call h5dwrite_f(dset2_id,H5T_NATIVE_DOUBLE,ma,dims,error)

                call h5dclose_f(dset2_id,error)

            call h5sclose_f(dspace2_id,error)


            dims = [ni, nk]
            call h5screate_simple_f(rank,dims,dspace3_id,error)

                call h5dcreate_f(file_id,dsetname3,H5T_NATIVE_DOUBLE,dspace3_id,dset3_id,error)

                call h5dwrite_f(dset3_id,H5T_NATIVE_DOUBLE,mr,dims,error)

                call h5dclose_f(dset3_id,error)

            call h5sclose_f(dspace3_id,error)


        call h5fclose_f(file_id,error)

    call h5close_f(error)

end subroutine savehdf5

end module matrix

program main
    use matrix
    use mpi
    implicit none

    integer, parameter :: MASTER=0
    integer :: ierr, proceso, np, ne, p, h, pflag, len, ini
    integer :: status(MPI_STATUS_SIZE)
    real(8) :: start, finish, nb, start0

    real(8), allocatable :: a(:), b(:), r(:), be(:), re(:)
    
    106 format("(01) Resevacion mem terminada en >> ",F10.3," segundos") 
    107 format("(02) Asig aleatoria terminada en >> ",F10.3," segundos") 
    108 format("(03) Tran a arreglo terminada en >> ",F10.3," segundos") 
    109 format("(04) Reservar memoria, para proceso ",I4,", terminada en >> ",F10.3," segundos") 
    110 format("(05) Oper. BAR, BRO, SCA para proceso ",I4,", terminada en >> ",F10.3," segundos")     
    111 format("(06) Multiplicacion para proceso ",I4,", terminada >> ",F10.3," segundos")     
    112 format("(07) Oper. GATHER para proceso ",I4,", terminada en >> ",F10.3," segundos")
    113 format("(08) Recomposicion terminada en >> ",F10.3," segundos")
    114 format("(09) Tamano de  memoria asignada >> ",F12.0," bytes")
    115 format("(10) Multiplicacion de matrices terminada en    >> ",F10.3," segundos") 

    call config_args()
    ! print *, "ni, nj, nk", ni, nj, nk

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,proceso,ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,np,ierr)
    
    !calulamos el numero elementos por proceso
    ne = int(ni*nk/np)

    if(ne.lt.1) then
        print *, "Error numero de elementos por proceso no puede ser vacio"
        stop 1
    end if

    if(ne.lt.ni) then
        print *, "Error numero de elementos por proceso no puede ser menor, al numero de filas (ne<ni)"
        stop 1
    end if

    if (mod(ne,ni).ne.0) then
        print *, "Error numero de elementos por proceso, debe ser factor del tamaño de la PRIMERA matrix ( ne%ni==0 )"
        stop 1
    end if

    if (mod(ne,nk).ne.0) then
        print *, "Error numero de elementos por proceso, debe ser factor del tamaño de la SEGUNDA matrix ( ne%nk==0 )"
        stop 1
    end if

    if (mod(ni*nk,np).ne.0) then
        print *, "Error el tamaño de la matrix RESULTADO, debe ser un factor del numero de procesos ( ni*nk%np==0 )"
        stop 1
    end if


    !El proceso 0, reparte trabajos a los demas procesos
    if(proceso.eq.MASTER) then

        call cpu_time(start)
        start0=start
        call allocate_matrix()
        call cpu_time(finish)
        write(*,106) finish-start

        call cpu_time(start)
        call rand_matrix()
        call cpu_time(finish)
        write(*,107) finish-start

        ! call print_ma()
        ! call print_mb()

        call cpu_time(start)
        call reshape_matrix()
        call cpu_time(finish)
        write(*,108) finish-start

        ! print *, "proceso", proceso, "ra", ra

    end if 

    call cpu_time(start)
    call allocateAll()
    call cpu_time(finish)

    if (proceso.eq.MASTER) then
        a = ra(1,:)
        b = rb(1,:)
        ! print *,"proceso", proceso, "a", a
        ! print *,"proceso", proceso, "b", b
    end if
    write(*,109) proceso, finish-start

    call cpu_time(start)
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    call MPI_BCAST(a,ni*nj,MPI_REAL8,MASTER,MPI_COMM_WORLD,ierr)
    call MPI_SCATTER(b,len*nj,MPI_REAL8,be,len*nj,MPI_REAL8,MASTER,MPI_COMM_WORLD,ierr)
    call cpu_time(finish)
    write(*,110) proceso, finish-start

    ! if (proceso.ne.0) then       
    !     print *,"proceso", proceso, "a", a
    !     print *,"proceso", proceso, "be", be
    ! end if

    ! print *,"proceso", proceso, "a", a
    ! print *,"proceso", proceso, "be", be

    call cpu_time(start)
    call dot_product_matrix()
    call cpu_time(finish)
    write(*,111) proceso, finish-start

    call cpu_time(start)
    call MPI_GATHER(re,ne,MPI_REAL8,r,ne,MPI_REAL8,MASTER,MPI_COMM_WORLD,ierr)
    call cpu_time(finish)
    write(*,112) proceso, finish-start

    if (proceso.eq.MASTER) then

        call cpu_time(start)
        call recompose_matrix()
        call cpu_time(finish)
        write(*,113) finish-start

        nb=(ni*nj+nj*nk+ni*nk)*8. !c_sizeof(real(8)) = 8
        write(*,114) nb
        write(*,115) finish-start0

        if(pflag.ge.1) then
            call print_matrix()
        end if
        
        call savehdf5()

        call deallocate_matrix()

    end if

    call deallocateAll()

    call MPI_FINALIZE(ierr)

contains

    subroutine recompose_matrix()
        ! print *,"proceso", proceso, "r", r
        rr(1,:)=r
        mr = reshape(source=rr, shape=[ni, nk])
        ! call print_mr()
    end subroutine recompose_matrix

    subroutine dot_product_matrix()
        h=1
        p=proceso*ne+1
        ini = int((p-1)/nk)+1
        do p=proceso*ne+1, proceso*ne+ne
            i=circular_ni(p)
            k=int((p-1)/nk)+1-(ini-1)
            ! print *,"proceso", proceso, "p", p, "i", i, "k", k
            re(h)=dot_product(a((i-1)*nj+1:i*nj),be((k-1)*nj+1:k*nj))
            h=h+1
        end do

        ! print *,"proceso", proceso, "re", re
    end subroutine dot_product_matrix

    subroutine allocateAll()
        call allocar(a, ni*nj)
        call allocar(b, nk*nj)
        call allocar(r, ni*nk)
        ! len = int((proceso*ne+ne)/nk)-int(proceso*ne/nk)+1
        ! len = int(ne/ni)+mod(ne,ni)
        len = int(nk/np)
        ! print *,"proceso", proceso, "len", len
        call allocar(be, len*nj)
        call allocar(re, ne)  
  
    end subroutine allocateAll

    subroutine deallocateAll()
        call desallocar(a)
        call desallocar(b)
        call desallocar(r)
        call desallocar(be)
        call desallocar(re)
    end subroutine deallocateAll

    integer function circular_ni(index)
        integer :: index
        if(mod(index,ni).eq.0) then
            circular_ni=ni
        else
            circular_ni=mod(index,ni)
        end if
    end function circular_ni

    subroutine allocar(arreglo,num_ele)
        real(8),ALLOCATABLE :: arreglo(:)
        INTEGER ::num_ele
        allocate (arreglo(num_ele))
    end subroutine allocar

    subroutine desallocar(arreglo)
        real(8),ALLOCATABLE:: arreglo(:)
        deallocate(arreglo)
    end subroutine desallocar

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