program hola_enviar
      use mpi

      integer numprocces, rankid, ierr, dataA
      integer status(MPI_STATUS_SIZE)
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
      else if (rankid==3) then
          print *, "proceso", rankid, "MIAHAU"
      else if (rankid==4) then
          print *, "proceso", rankid, "OI"
      else 
          print *, "proceso", rankid, "CIAO"
      endif

      if(rankid==0) then
        dataA=15
        call MPI_SEND(dataA, 1, MPI_INTEGER, 1, 3000, MPI_COMM_WORLD, ierr)
      endif
      
      if(rankid==1) then
        print *, "dataA antes es ", dataA
        call MPI_RECV(dataA, 1, MPI_INTEGER, 0, 3000, MPI_COMM_WORLD, status, ierr)
        print *, "dataA depues epepues es ", dataA
      endif

      if(rankid==3) then
        dataA = 33
        print *, "El proceso 3 tiene dataA es ", dataA
      endif

      call MPI_FINALIZE(ierr);
end program hola_enviar
      
