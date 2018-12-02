

program mult_matrix
    use mpi
    PARAMETER (MASTER=0)
    INTEGER :: proceso,num_elementos,ierr
    CHARACTER(MPI_MAX_PROCESSOR_NAME) hostname
    INTEGER :: i,j,k,tamano
    INTEGER :: lim_inf,lim_sup,elementos,suma_elementos
    REAL :: var1,var2,var3
    INTEGER, ALLOCATABLE:: arr1(:)
    
    101 format ("Hola desde el proceso",I3, " de ", I3)

    103 format ("Desde el proceso",I3,", Recibio el tamano",I3)

    call MPI_INIT (ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,num_procesos,ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,proceso,ierr)
    call MPI_GET_PROCESSOR_NAME(hostname,len,ierr)

    tamano =0
    write(*,101) proceso, num_procesos
    !*******************************MAESTRO *****************************************
    if (proceso.eq.MASTER)then
        print *,"Asumimos una matriz simple"
        print *,"Cual es el tamano de la matrices?"
        read *, tamano
        print *,"La matriz va a tener tantos elementos",tamano
        !call MPI_BCAST(tamano,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        call allocar (arr1,tamano)
        call llenar_azar(arr1,tamano)
                
        print *,"Arreglo1"
        call imprimir(arr1,tamano)
        call desallocar(arr1,tamano)
    else
    ! **********************************TRABAJADORES ********************************
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        !print *,"proc:",proceso,"tamano ",tamano

    end if
        
    call MPI_BCAST(tamano,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
        
    if (proceso.ne.MASTER)then
        write (*,103) proceso,tamano

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
        105 format ("",I6)
        if (allocated(arreglo)) then
            print *,"El numero de elemntos es ",num_ele
            do i=1,num_ele
                write(*,105,advance="no") arreglo(i)
            end do
            print *
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
                      arreglo(i)=rand(0)*10
                end do
        else
        write (*,*) "Error: El arreglo no ha sido alocado"
        end if

    end subroutine llenar_azar


end program mult_matrix





