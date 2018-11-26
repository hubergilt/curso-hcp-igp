program hola_enviar
      use mpi

      integer numprocces, rankid, ierr
      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rankid, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocces, ierr)
      !print *, "en el proceso ", rankid, "exiten: ", numprocces
       
      if(rankid==0) then
          print *, "proceso", rankid, "HOLA"
      else if (rankid==1) then
          print *, "proceso", rankid, "HELLO"
      else if (rankid==2) then
          print *, "proceso", rankid, "SALUT"
      else if (rankid==2) then
          print *, "proceso", rankid, "SALUT"
      else 
          print *, "proceso", rankid, "CIAO"
      endif

      call MPI_FINALIZE(ierr);
end program hola_enviar
      
