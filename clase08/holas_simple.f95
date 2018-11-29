program hola_enviar
      use mpi

      integer numprocces, rankid, ierr
      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rankid, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocces, ierr)
      print *, "en el proceso ", rankid, "exiten: ", numprocces

      call MPI_FINALIZE(ierr);
end program hola_enviar
      
