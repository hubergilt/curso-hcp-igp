

program mult_matrix
	use mpi
	implicit none 
	INTEGER, PARAMETER :: MASTER =0 
	INTEGER :: i,j,k,tamano,ierr,proceso,num_procesos,tam_rec
	INTEGER :: lim_inf,lim_sup,elementos,suma_elementos
	INTEGER :: status(MPI_STATUS_SIZE)
	REAL :: var1,var2,var3
	INTEGER, ALLOCATABLE:: arr1(:),arr2(:),arr3(:)
	CHARACTER (MPI_MAX_PROCESSOR_NAME)hostname
	
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,proceso,ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,num_procesos,ierr)

	print *,"Hola desde el proceso ",proceso
	!***************************************MASTER ******************************************
	if(proceso.eq.MASTER)then ! Solamente el proceso 0 va a hacer esto
	print *,"Asumimos una matriz simple"
	print *,"Cual es el tamano de la matrices"
	read *, tamano
	print *,"La matriz va a tener tantos elementos",tamano
	!lim_inf= ((i-1)*(tamano/(num_procesos-1))+1)
	!lim_sup= (i)*(tamano/(num_procesos-1))
	!elementos=(lim_sup-lim_inf +1)
	call allocar (arr1,tamano)
	call allocar (arr2,tamano)
	call allocar (arr3,tamano)
	call llenar_azar(arr1,tamano)
	call llenar_azar(arr2,tamano)
	!do i=1,tamano
           	
	!	arr3(i)=arr1(i)*arr2(i) 
        !end do
	!print *,"**********ARREGLO 3 *******************"
	!call imprimir(arr3,tamano)
	!print *,"***************************************"	

	do i=1,(num_procesos-1)
	lim_inf= ((i-1)*(tamano/(num_procesos-1))+1)
	lim_sup= (i)*(tamano/(num_procesos-1))
	elementos=(lim_sup-lim_inf +1)
	call MPI_SEND(tamano,1,MPI_INTEGER,i,201,MPI_COMM_WORLD,ierr)
	call MPI_SEND(lim_inf,1,MPI_INTEGER,i,202,MPI_COMM_WORLD,ierr)
	call MPI_SEND(lim_sup,1,MPI_INTEGER,i,203,MPI_COMM_WORLD,ierr)
	call MPI_SEND(elementos,1,MPI_INTEGER,i,204,MPI_COMM_WORLD,ierr)
	call MPI_SEND(arr1(lim_inf),elementos,MPI_INTEGER,i,210,MPI_COMM_WORLD,ierr)
	call MPI_SEND(arr2(lim_inf),elementos,MPI_INTEGER,i,211,MPI_COMM_WORLD,ierr)
	call MPI_SEND(arr3(lim_inf),elementos,MPI_INTEGER,i,212,MPI_COMM_WORLD,ierr)
	end do 
	!Para recibir precisa de otro lazo. 
	do i=1,(num_procesos-1)
	lim_inf= ((i-1)*(tamano/(num_procesos-1))+1)
	call MPI_RECV(arr3(lim_inf),elementos,MPI_INTEGER,i,300,MPI_COMM_WORLD,status,ierr)
	end do 

	print *, "arreglo3 ya rcibido"
	call imprimir(arr3,tamano)

	call desallocar (arr1,tamano)
	call desallocar (arr2,tamano)
	call desallocar (arr3,tamano)	
	else 
	!************************************TRABAJADORES ******************************************
	call MPI_RECV(tamano,1,MPI_INTEGER,0,201,MPI_COMM_WORLD,status,ierr)	
	call MPI_RECV(lim_inf,1,MPI_INTEGER,0,202,MPI_COMM_WORLD,status,ierr)
	call MPI_RECV(lim_sup,1,MPI_INTEGER,0,203,MPI_COMM_WORLD,status,ierr)
	call MPI_RECV(elementos,1,MPI_INTEGER,0,204,MPI_COMM_WORLD,status,ierr)
	print *,"Desde el proceso",proceso,"tam",tamano,"lim_inf",lim_inf,"lim_sup",lim_sup,"elem",elementos
	call allocar (arr1,tamano)
	call allocar (arr2,tamano)
	call allocar (arr3,tamano)
	call llenar_zeros(arr1,tamano)
	call llenar_zeros(arr2,tamano)
	call llenar_zeros(arr3,tamano)
	call MPI_RECV(arr1(lim_inf),elementos,MPI_INTEGER,0,210,MPI_COMM_WORLD,status,ierr)
	call MPI_RECV(arr2(lim_inf),elementos,MPI_INTEGER,0,211,MPI_COMM_WORLD,status,ierr)
	call MPI_RECV(arr3(lim_inf),elementos,MPI_INTEGER,0,212,MPI_COMM_WORLD,status,ierr)
	
	do i= lim_inf,lim_sup
	  arr3(i)=arr1(i)*arr2(i)
	end do 
	
	call MPI_SEND(arr3(lim_inf),elementos,MPI_INTEGER,0,300,MPI_COMM_WORLD,ierr)
	!print *,"Imprimiendo arreglo1,2,3 del proceso",proceso
	!print *,"Arreglo1"
	!call imprimir(arr1,tamano)
	!print *,"Arreglo2"
	!call imprimir(arr2,tamano)
	!print *,"Arreglo3"
	!call imprimir(arr3,tamano)
	call desallocar (arr1,tamano)
	call desallocar (arr2,tamano)
	call desallocar (arr3,tamano)	

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
                  arreglo(i)=rand(0)*10
        	end do
	else
	write (*,*) "Error: El arreglo no ha sido alocado"
	end if

	end subroutine llenar_azar


	subroutine llenar_zeros(arreglo,num_ele)
	INTEGER,ALLOCATABLE:: arreglo(:) 
	INTEGER ::num_ele,i,j
	if (allocated(arreglo)) then
		!print *,"El numero de elemntos es ",num_ele
		do i=1,num_ele
                  arreglo(i)=0
        	end do
	else
	write (*,*) "Error: El arreglo no ha sido alocado"
	end if

	end subroutine llenar_zeros




end program mult_matrix





