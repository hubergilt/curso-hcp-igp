

program mult_matrix
    use mpi
    PARAMETER (MASTER=0)
    INTEGER :: proceso,num_elementos,ierr
    CHARACTER(MPI_MAX_PROCESSOR_NAME) hostname
    INTEGER :: i,j,k,tamano,suma_parcial,mult_parcial,suma_total,mult_total
    INTEGER :: lim_inf,lim_sup,elementos,suma_elementos
    REAL :: var1,var2,var3
    INTEGER, ALLOCATABLE:: arr1(:), arre(:)
    
    call MPI_INIT (ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,num_procesos,ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,proceso,ierr)
    call MPI_GET_PROCESSOR_NAME(hostname,len,ierr)
    tamano =0
    suma_parcial=0
    suma_total=0
    mult_parcial=1
    mult_total=1
    print*,"Hola desde el proc ",proceso,"Existe",num_procesos
    !*******************************MAESTRO *****************************************
    if (proceso.eq.MASTER)then
        print *,"Asumimos una matriz simple"
        print *,"Cual es el tamano de la matrices"
        read *, tamano
        print *,"La matriz va a tener tantos elementos",tamano
        !call MPI_BCAST(tamano,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        call allocar (arr1,tamano)
        call llenar_azar(arr1,tamano)
                
        print *,"Arreglo1"
        call imprimir(arr1,tamano)
        elementos=tamano/num_procesos
        call allocar (arre,elementos)
    else
    ! **********************************TRABAJADORES ********************************
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        !print *,"proc:",proceso,"tamano ",tamano

    end if
    
    call MPI_BCAST(tamano,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
    
    if (proceso.ne.MASTER)then
        !print *,"proc:",proceso,"tamano ",tamano
        elementos = tamano/num_procesos
        call allocar (arre,elementos)
        
    end if

    call MPI_SCATTER(arr1,elementos,MPI_INTEGER,arre,elementos,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
    !print *,"proceso",proceso,"resultado",arre
    

    !do i=1,elementos
    !  arre(i)=arre(i)
    !end do
    !call MPI_GATHER(arre,elementos,MPI_INTEGER,arr1,elementos,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
    
    do i=1,elementos
     mult_parcial=arre(i) * mult_parcial
    end do
    !print *,"proceso",proceso,"resultado",arre

    print *,"Proceso",proceso,"mult_parcial",mult_parcial

    call MPI_REDUCE(mult_parcial,mult_total,1,MPI_INTEGER,MPI_PROD,MASTER,MPI_COMM_WORLD,ierr)


    if (proceso.eq.MASTER) then
        call imprimir (arr1,tamano)
        print *,"El producto total es",mult_total
    end if
    

    call MPI_FINALIZE(ierr)
    !**********************************************FUNCIONES********************
    contains
    
    subroutine allocar (arreglo,num_ele)
        INTEGER,ALLOCATABLE :: arreglo(:)
        INTEGER ::num_ele,i,j
        allocate (arreglo(num_ele))
    end subroutine allocar 

    subroutine desallocar(arreglo,num_ele)
        INTEGER,ALLOCATABLE:: arreglo(:)
        INTEGER :: num_ele,i,j
        deallocate(arreglo)
    end subroutine desallocar   

    
    subroutine imprimir(arreglo,num_ele)
        INTEGER,ALLOCATABLE:: arreglo(:) 
        INTEGER ::num_ele,i,j
        if (allocated(arreglo)) then
            print *,"El numero de elemntos es ",num_ele
            do i=1,num_ele
                print *,arreglo(i)
            end do
        else
            write (*,*) "Error: El arreglo no ha sido alocado"
        end if
    end subroutine imprimir 


    subroutine llenar_azar(arreglo,num_ele)
        INTEGER,ALLOCATABLE:: arreglo(:) 
        INTEGER ::num_ele,i,j
        if (allocated(arreglo)) then
            !print *,"El numero de elemntos es ",num_ele
            do i=1,num_ele
                arreglo(i)=(rand(0)*10)+1
            end do
        else
            write (*,*) "Error: El arreglo no ha sido alocado"
        end if
    end subroutine llenar_azar


end program mult_matrix





