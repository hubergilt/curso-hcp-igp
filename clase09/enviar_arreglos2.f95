program mult_matrix
	use mpi
	PARAMETER (MASTER =0) 
	INTEGER :: i,j,k,tamano,ierr,proceso,num_procesos,tam_rec
	INTEGER :: lim_inf,lim_sup,elementos,suma_elementos
	INTEGER :: status(MPI_STATUS_SIZE)
	REAL :: var1,var2,var3
	INTEGER, ALLOCATABLE:: arr1(:),arr2(:),arr3(:),arr4(:)
	CHARACTER (MPI_MAX_PROCESSOR_NAME)hostname
	
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,proceso,ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,num_procesos,ierr)

	print *,"Hola desde el proceso ",proceso
	if(proceso.eq.MASTER)then ! Solamente el proceso 0 va a hacer esto
	print *,"Asumimos una matriz simple"
	print *,"Cual es el tamano de la matrices"
	read *, tamano
	print *,"La matriz va a tener tantos elementos",tamano
	
	call allocar (arr1,tamano)
	call allocar (arr2,tamano)
	call allocar (arr3,tamano)
	call allocar (arr4,tamano)
	call llenar_azar(arr1,tamano)
	
	do i=1,(num_procesos-1)
	 call MPI_SEND(tamano,1,MPI_INTEGER,i,201,MPI_COMM_WORLD,ierr)
	end do 

	call MPI_RECV(arr2,tamano,MPI_INTEGER,i,201,MPI_COMM_WORLD,status,ierr)	
	call MPI_RECV(arr3,tamano,MPI_INTEGER,i,201,MPI_COMM_WORLD,status,ierr)	
	call MPI_RECV(arr4,tamano,MPI_INTEGER,i,201,MPI_COMM_WORLD,status,ierr)	

	call desallocar (arr1,tamano)
	call desallocar (arr2,tamano)
	call desallocar (arr3,tamano)	
	call desallocar (arr4,tamano)


	else ! Esto van a hacer el resto de los procesos
	call MPI_RECV(tamano,1,MPI_INTEGER,0,201,MPI_COMM_WORLD,status,ierr)	
	print *,"Desde el proceso",proceso,"Recibio el tamano",tamano
    
    if(proceso.eq.1) then 
	    call allocar (arr2,tamano)
	    call MPI_RECV(arr1,tamano,MPI_INTEGER,0,202,MPI_COMM_WORLD,status,ierr)
        call elevar_potencia(arr1, arr2, tamano, 2)
	    call MPI_SEND(arr2,tamano,MPI_INTEGER,0,203,MPI_COMM_WORLD,ierr)
    else if(proceso.eq.2) then 

    else if(proceso.eq.3) then 

    end if 

	call allocar (arr1,tamano)
	call allocar (arr3,tamano)
	call allocar (arr4,tamano)
	call MPI_RECV(arr1,tamano,MPI_INTEGER,0,202,MPI_COMM_WORLD,status,ierr)
	call MPI_RECV(arr2,tamano,MPI_INTEGER,0,203,MPI_COMM_WORLD,status,ierr)
	call MPI_RECV(arr3,tamano,MPI_INTEGER,0,204,MPI_COMM_WORLD,status,ierr)
	call MPI_RECV(arr4,tamano,MPI_INTEGER,0,205,MPI_COMM_WORLD,status,ierr)
	print *,"Imprimiendo los arreglos del proceso",proceso
	print *,"Arreglo1"
	call imprimir(arr1,tamano)
	print *,"Arreglo2"
	call imprimir(arr2,tamano)
	print *,"Arreglo3"
	call imprimir(arr3,tamano)
	print *,"Arreglo4"
	call imprimir(arr4,tamano)
	call desallocar (arr1,tamano)
	call desallocar (arr2,tamano)
	call desallocar (arr3,tamano)	
	call desallocar (arr4,tamano)
	end if 

	call MPI_FINALIZE(ierr)
	!**********************************************FUNCIONES********************
	contains
	

    subroutine elevar_potencia(arreglo1, arreglo2, num_ele, potencia)
	INTEGER,ALLOCATABLE:: arreglo1(:) 
	INTEGER,ALLOCATABLE:: arreglo2(:) 
	INTEGER ::num_ele,i,j,potencia
	if (allocated(arreglo1) .and. allocated(arreglo2)) then
		do i=1,num_ele
            arreglo2(i)=arreglo1(i)**potencia
        end do
    end if
    end subroutine elevar_potencia

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
                  arreglo(i)=rand(0)*10
        	end do
	else
	write (*,*) "Error: El arreglo no ha sido alocado"
	end if

	end subroutine llenar_azar


end program mult_matrix
