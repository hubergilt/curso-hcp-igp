module matrix

    implicit none
    real, parameter:: MA__NFIL = 4
    real, parameter:: MA__NCOL = 4
    real, parameter:: MB__NFIL = 4
    real, parameter:: MB__NCOL = 4
    real, allocatable :: ma(:,:)
    real, allocatable :: mb(:,:)
    real, allocatable :: mr(:,:)
    integer :: ma_nfil
    integer :: ma_ncol
    integer :: mb_nfil
    integer :: mb_ncol
    integer :: i, j, k, e, ei, ej, ek, flag
    character(len=40) :: arg  
    character(len=10), dimension(3) :: args

contains
 
subroutine print_matrix() 
    integer :: nfilas, ncolumnas, i , j
    real, allocatable :: arreglo1(:,:)

    101 format("El orden seleccionado para la ",A10," matrix es >> (",I4,",",I4,") ")
    102 format("El resultado de la Multiplicacion de matrices es >> (",I4,",",I4,")")
    106 format("(",I4,",",I4,")=",F16.8," ")

    write(*,101) "PRIMERA", ma_nfil, ma_ncol
    do i=1, ma_nfil
        do j=1, ma_ncol
            write (*, 106, advance="no") i, j, ma(i, j)
        end do
        print *
    end do

    write(*,101) "SEGUNDA", mb_nfil, mb_ncol
    do i=1, mb_nfil
        do j=1, mb_ncol
            write (*, 106, advance="no") i, j, mb(i, j)           
        end do
        print *
    end do

    write(*,102) ma_nfil, mb_ncol
    do i=1, ma_nfil
        do j=1, mb_ncol
            write (*, 106, advance="no") i, j, mr(i, j)           
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
        end do
    end do

    do j=1, mb_ncol
        do i=1, mb_nfil
            mb(i, j)=rand(0)*10+1                       
        end do
    end do

    do j=1, mb_ncol
        do i=1, ma_nfil
            mr(i, j)=0                        
        end do
    end do    

end subroutine rand_matrix

end module matrix

program multimatrix_mpi
    use matrix
    use mpi
    implicit none

    integer, parameter :: MASTER=0
    integer :: ierr,proceso,num_procesos, ne, p, h, g, f, flg, resto
    integer :: status(MPI_STATUS_SIZE)
    real :: start, finish, nb
    real, allocatable :: a(:), b(:), r(:)
    real, allocatable :: ga(:,:), gb(:,:)


    104 format("Multiplicacion de matrices terminada en    >> ",F10.3," segundos") 
    105 format("Tamano del problema, suma memoria asignada >> ",F12.0," bytes")

    call config_args()
    flg = 0

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,proceso,ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,num_procesos,ierr)
    
    !calulamos el numero elementos por proceso
    ne = int(ma_nfil*mb_ncol/num_procesos)

    if(ne.lt.1) then
        print *, "Error numero de elementos vacio"
        stop 1
    end if

    allocate(a(ma_ncol))
    allocate(b(ma_ncol))
    allocate(r(ne)) 

    r=0

    !el proceso 0, reparte trabajos a los demas procesos
    if(proceso.eq.MASTER) then 
        call allocate_matrix()
        call rand_matrix()
        call cpu_time(start)

        !el proceso 0 envia a cada proceso un numero de filas y columnas dado por ne
        do p=1,num_procesos-1
            call MPI_SEND(ne,1,MPI_INTEGER,p,201,MPI_COMM_WORLD,ierr)
            
            f=p*ne+1

            do g=f, f+(ne-1)

                flg = 0
                do i=1,ma_nfil

                    do k=1,mb_ncol
                        if(g.eq.(mb_ncol*(i-1)+k)) then                            
                            call MPI_SEND(ma(i,:),ma_ncol,MPI_REAL,p,10000+g+i+k,MPI_COMM_WORLD,ierr)
                            call MPI_SEND(mb(:,k),ma_ncol,MPI_REAL,p,20000+g+i+k,MPI_COMM_WORLD,ierr)
                            flg=1                            
                            exit
                        end if
                    end do

                    if (flg.eq.1) then
                       exit
                    end if

                end do
            end do

        end do

        !el proceso 0, realiza la multiplicacion de las primeras "ne" filas y columnas 
        do g=1, ne
            flg = 0
            do i=1,ma_nfil
                do k=1,mb_ncol
                    if(g.eq.(mb_ncol*(i-1)+k)) then                            
                        a = ma(i,:)
                        b = mb(:,k)

                        do j=1,ma_ncol
                            r(g)=r(g)+a(j)*b(j)
                        end do

                        flg=1                            
                        exit

                    end if                
                end do

                if (flg.eq.1) then
                   exit
                end if                

            end do
        end do

        !el proceso 0, procesa la multiplicacion del resto de elementos
        resto = mod(ma_nfil*mb_ncol,num_procesos)
        call MPI_SEND(resto,1,MPI_INTEGER,num_procesos-1,202,MPI_COMM_WORLD,ierr)

        if(resto.gt.0) then

            p=num_procesos-1
            f=(p+1)*ne+1

            do g=f, f+resto

                flg = 0
                do i=1,ma_nfil

                    do k=1,mb_ncol
                        if(g.eq.(mb_ncol*(i-1)+k)) then                            
                            call MPI_SEND(ma(i,:),ma_ncol,MPI_REAL,p,40000+g+i+k,MPI_COMM_WORLD,ierr)
                            call MPI_SEND(mb(:,k),ma_ncol,MPI_REAL,p,50000+g+i+k,MPI_COMM_WORLD,ierr)
                            flg=1                            
                            exit
                        end if
                    end do

                    if (flg.eq.1) then
                       exit
                    end if

                end do
            end do

        end if

    else
        call MPI_RECV(ne,1,MPI_INTEGER,0,201,MPI_COMM_WORLD,status,ierr)

        allocate(ga(ne,ma_ncol))
        allocate(gb(ne,ma_ncol))    

        !cada proceso diferente del proceso 0, guarda las filas y columnas en arreglos
        j=1
        f=proceso*ne+1
        do g=f, f+(ne-1)
            flg = 0
            do i=1,ma_nfil
                do k=1,mb_ncol
                    if(g.eq.(mb_ncol*(i-1)+k)) then                     
                        call MPI_RECV(a,ma_ncol,MPI_REAL,0,10000+g+i+k,MPI_COMM_WORLD,status,ierr)
                        call MPI_RECV(b,ma_ncol,MPI_REAL,0,20000+g+i+k,MPI_COMM_WORLD,status,ierr)      

                        ga(j, :) = a
                        gb(j, :) = b
                        j=j+1
                        flg=1                            
                        exit

                    end if
                end do

                if (flg.eq.1) then
                   exit
                end if

            end do
        end do


        !cada proceso diferente al proceso 0, realiza la multiplicacion
        do g=1, ne
            a = ga(g, :)
            b = gb(g, :)
            r(g)=0

            do j=1,ma_ncol
                r(g)=r(g)+a(j)*b(j)
            end do 

        end do

        !devuelve el resultado de cada proceso
        call MPI_SEND(r,ne,MPI_REAL,0,30000+proceso,MPI_COMM_WORLD,ierr)

        deallocate(ga)
        deallocate(gb)        

    end if

    !el ultimo proceso realiza el trabajo con el resto de elementos
    if(proceso.eq.(num_procesos-1)) then
        call MPI_RECV(resto,1,MPI_INTEGER,0,202,MPI_COMM_WORLD,status, ierr)

        if(resto.gt.0) then

            allocate(ga(resto,ma_ncol))
            allocate(gb(resto,ma_ncol))                  
            j=1

            p=num_procesos-1
            do h = 1, resto
                f=(proceso+1)*ne+h
                do g=f, f+(ne-1)
                    flg = 0
                    do i=1,ma_nfil

                        do k=1,mb_ncol
                            if(g.eq.(mb_ncol*(i-1)+k)) then 
                                call MPI_RECV(a,ma_ncol,MPI_REAL,0,40000+g+i+k,MPI_COMM_WORLD,status,ierr)
                                call MPI_RECV(b,ma_ncol,MPI_REAL,0,50000+g+i+k,MPI_COMM_WORLD,status,ierr)      

                                ga(j, :) = a
                                gb(j, :) = b
                                j=j+1
                                flg=1

                                exit
                            end if
                        end do

                        if (flg.eq.1) then
                           exit
                        end if

                    end do            
                end do           
            end do

            do g=1, ne
                a = ga(g, :)
                b = gb(g, :)
                r(g)=0

                do j=1,ma_ncol
                    r(g)=r(g)+a(j)*b(j)
                end do 

            end do

            call MPI_SEND(r,ne,MPI_REAL,0,60000+proceso,MPI_COMM_WORLD,ierr)

            deallocate(ga)
            deallocate(gb)

        end if
      
    end if  

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)

    !el proceso 0, compone todos los resultados en la matrix resultado
    if(proceso.eq.MASTER) then

        !guarda el resultado de proceso 0, en la matrix resultado
        do g=1, ne
            flg = 0    
            do i=1,ma_nfil
                do k=1,mb_ncol
                    if(g.eq.(mb_ncol*(i-1)+k)) then
                        mr(i, k) = r(g)

                        flg=1                            
                        exit

                    end if
                end do

                if (flg.eq.1) then
                   exit
                end if

            end do
        end do

        !guarda el resultado de demas procesos diferente al proceso 0, en la matrix resultado
        do p=1,num_procesos-1
            call MPI_RECV(r,ne,MPI_REAL,p,30000+p,MPI_COMM_WORLD,status,ierr)
            
            do h=1, ne
                g=p*ne+h
                flg = 0
                do i=1,ma_nfil
                    do k=1,mb_ncol
                        if(g.eq.(mb_ncol*(i-1)+k)) then
                            mr(i, k) = r(h)

                            flg=1                            
                            exit

                        end if
                    end do

                    if (flg.eq.1) then
                        exit
                    end if
                end do
            end do
        end do

        !guarda el resultado del ultimo proceso si hay resto, en la matrix resultado
        if(resto.gt.0) then
            p=num_procesos-1
            call MPI_RECV(r,ne,MPI_REAL,p,60000+p,MPI_COMM_WORLD,status,ierr)
            do h=1, resto
                g=(p+1)*ne+h
                flg = 0
                do i=1,ma_nfil
                    do k=1,mb_ncol
                        if(g.eq.(mb_ncol*(i-1)+k)) then
                            mr(i, k) = r(resto)

                            flg=1                            
                            exit

                        end if
                    end do

                    if (flg.eq.1) then
                        exit
                    end if
                end do
            end do
        end if

        !calcula el tiempo que toma el proceso y la memoria asignada
        call cpu_time(finish)
        write(*,104) finish-start

        nb=(ma_nfil*ma_ncol+mb_nfil*mb_ncol+ma_nfil*mb_ncol)*16. ! sizeof(real)=16
        write(*,105) nb
        print *    

        if(flag.ge.1) then
            call print_matrix()        
        end if

        call deallocate_matrix()

    end if

    deallocate(a)
    deallocate(b)
    deallocate(r)        

    call MPI_FINALIZE(ierr)

contains

    subroutine config_args()
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
    end subroutine config_args

end program multimatrix_mpi