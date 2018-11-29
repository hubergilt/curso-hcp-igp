
program hola_enviar
	use mpi
	implicit none
	integer,parameter:: MASTER= 0
	integer numtasks,taskid,len,rem,ierr
	integer partner,message,status(MPI_STATUS_SIZE)
	character (MPI_MAX_PROCESSOR_NAME) hostname

	call MPI_INIT(ierr)	
	call MPI_COMM_RANK(MPI_COMM_WORLD,taskid,ierr)
	call MPI_GET_PROCESSOR_NAME(hostname,len,ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,numtasks,ierr)
	write(*,20)taskid,hostname
	if(taskid.eq.MASTER)then
	  print *,"Desde el maestro, existen", numtasks
	  endif
	
	if (taskid.lt.numtasks/2)then
	  partner=numtasks/2 + taskid
	  call MPI_SEND(taskid,1,MPI_INTEGER,partner,100,MPI_COMM_WORLD,ierr)
	  call MPI_RECV(message,1,MPI_INTEGER,partner,200,MPI_COMM_WORLD,status,ierr)
	else if (taskid.ge.numtasks/2) then
   	  partner=taskid - numtasks/2
	  call MPI_RECV(message,1,MPI_INTEGER,partner,100,MPI_COMM_WORLD,status,ierr)
	  call MPI_SEND(taskid,1,MPI_INTEGER,partner,200,MPI_COMM_WORLD,ierr)
	endif 
	print *,"proceso",taskid,"companero",message

	call MPI_FINALIZE(ierr)
	
	20 format ("Hola desde el proceso",I4," en " A48)


end program hola_enviar

