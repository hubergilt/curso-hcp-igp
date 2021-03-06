program hola_simple
    use mpi
    implicit none
    integer, parameter :: MASTER = 0
    integer :: numtasks, taskid, len, rem, ierr
    integer :: partner, message, status(MPI_STATUS_SIZE)
    character (MPI_MAX_PROCESSOR_NAME) hostname

    call MPI_INIT(ierr) 
    call MPI_COMM_RANK(MPI_COMM_WORLD, taskid, ierr)
    call MPI_GET_PROCESSOR_NAME(hostname, len, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
    write(*,20)taskid, numtasks, hostname
    call MPI_FINALIZE(ierr)
    
    20 format ("Hola soy el proceso ",I3," de ",I3," y estoy corriendo en el nodo " A48)
end program hola_simple

