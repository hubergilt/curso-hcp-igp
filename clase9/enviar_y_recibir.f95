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
	!**************************************************************************
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
	do i=1,(num_procesos-1)
	call MPI_SEND(arr1,tamano,MPI_INTEGER,i,202,MPI_COMM_WORLD,ierr)
	end do 
	call MPI_RECV(arr2,tamano,MPI_INTEGER,1,500,MPI_COMM_WORLD,status,ierr)	
	call MPI_RECV(arr3,tamano,MPI_INTEGER,2,600,MPI_COMM_WORLD,status,ierr)	
	call MPI_RECV(arr4,tamano,MPI_INTEGER,3,700,MPI_COMM_WORLD,status,ierr)	

	call imprimir(arr1,tamano)
	call imprimir(arr2,tamano)
	call imprimir(arr3,tamano)
	call imprimir(arr4,tamano)
	!*************************************************************************************
	else ! Esto van a hacer el resto de los procesos
	call MPI_RECV(tamano,1,MPI_INTEGER,0,201,MPI_COMM_WORLD,status,ierr)	
	print *,"Desde el proceso",proceso,"Recibio el tamano",tamano
	call allocar (arr1,tamano)
	call MPI_RECV(arr1,tamano,MPI_INTEGER,0,202,MPI_COMM_WORLD,status,ierr)
	
	if (proceso .eq. 1) then
	do i=1,(tamano)
	arr1(i)=arr1(i)**2
	end do
	call MPI_SEND(arr1,tamano,MPI_INTEGER,0,500,MPI_COMM_WORLD,ierr)

	else if (proceso .eq. 2) then
	do i=1,(tamano)
	arr1(i)=arr1(i)**3
	end do
	call MPI_SEND(arr1,tamano,MPI_INTEGER,0,600,MPI_COMM_WORLD,ierr)
	else 
	do i=1, tamano
	arr1(i)=arr1(i)**4
	end do 
	call MPI_SEND(arr1,tamano,MPI_INTEGER,0,700,MPI_COMM_WORLD,ierr)
	end if 
	end if 
	!***********************************************************************
	call desallocar(arr1,tamano)

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
                  arreglo(i)=rand(0)*10
        	end do
	else
	write (*,*) "Error: El arreglo no ha sido alocado"
	end if

	end subroutine llenar_azar


end program mult_matrix





