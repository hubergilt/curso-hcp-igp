
program holas_completo

	use MPI
	!implicit none
	INTEGER ::i,j,k,proceso,num_procesos,ierr
	INTEGER ::arreglo1(10),edad,edad1,edad2,edad3
	INTEGER :: status (MPI_STATUS_SIZE)
	call MPI_INIT (ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,proceso,ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,num_procesos,ierr)
	print *,"Hola desde ",proceso,"Existen :",num_procesos,"procesos"
	!**********************PROCESO 0**********************
	if (proceso.eq.0) then 
	
	print *,"Cual es tu edad"
	read *,edad
	print *,"Tu edad es ",edad
	  do i=1,num_procesos-1 !Lazo para enviar info a todos los otros procesos
	  call MPI_SEND(edad,1,MPI_INTEGER,i,10,MPI_COMM_WORLD,ierr)
 	  end do
	  call MPI_RECV(edad1,1,MPI_INTEGER,1,11,MPI_COMM_WORLD,status,ierr)
	  call MPI_RECV(edad2,1,MPI_INTEGER,2,11,MPI_COMM_WORLD,status,ierr)
	  call MPI_RECV(edad3,1,MPI_INTEGER,3,11,MPI_COMM_WORLD,status,ierr)
	  
	print *,"Proceso 0 recibio nueva edad BI: ",edad1
	print *,"Proceso 0 recibio nueva edad OLIMP: ",edad2
	print *,"Proceso 0 recibio nueva edad MUNDIAL: ",edad3
	  
	!*********************otros procesos **************
	else 
	
	print *, "No soy el proceso 0, asi que tengo que hacer esto" 	
	call  MPI_RECV(edad,1,MPI_INTEGER,0,10,MPI_COMM_WORLD,status,ierr) 
	print *, "Hola del proceso ",proceso,"Recibi la edad ",edad
		if (proceso.eq.1)then
		edad=edad+3
		else if (proceso.eq.2) then
		edad=edad+2
		else
		edad=edad+36
		end if 
	call MPI_SEND(edad,1,MPI_INTEGER,0,11,MPI_COMM_WORLD,ierr)
	end if 
	call MPI_FINALIZE(ierr)
end program holas_completo
