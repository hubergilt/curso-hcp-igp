program hola_enviar
      use mpi

      integer numprocces, rankid, ierr, dataA, numero, numero_final
      integer status(MPI_STATUS_SIZE)
      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rankid, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocces, ierr)
      !print *, "en el proceso ", rankid, "exiten: ", numprocces
       
      numero = 2
      numero_final = 0

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
        call MPI_SEND(numero, 1, MPI_INTEGER, 1, 3000, MPI_COMM_WORLD, ierr)
        print *, "el proceso es: ", rankid, " envia el numero es: ", numero
        call MPI_RECV(numero, 1, MPI_INTEGER, 3, 6000, MPI_COMM_WORLD, status, ierr)
        print *, "imprimo el valor de numero final: ", numero_final
    endif
      
      if(rankid==1) then
        call MPI_RECV(numero, 1, MPI_INTEGER, 0, 3000, MPI_COMM_WORLD, status, ierr)
        print *, "el proceso es: ", rankid, " recibe el numero es: ", numero
        numero = numero ** 2
        call MPI_SEND(numero, 1, MPI_INTEGER, 2, 4000, MPI_COMM_WORLD, ierr)
        print *, "el proceso es: ", rankid, " envia el numero es: ", numero
      endif

      if(rankid==2) then
        call MPI_RECV(numero, 1, MPI_INTEGER, 1, 4000, MPI_COMM_WORLD, status, ierr)
        print *, "el proceso es: ", rankid, " recibe el numero es: ", numero
        numero = numero ** 2
        call MPI_SEND(numero, 1, MPI_INTEGER, 3, 5000, MPI_COMM_WORLD, ierr)
        print *, "el proceso es: ", rankid, " envia el numero es: ", numero
    endif
      
      if(rankid==3) then
        call MPI_RECV(numero, 1, MPI_INTEGER, 2, 5000, MPI_COMM_WORLD, status, ierr)
        print *, "el proceso es: ", rankid, " recibe el numero es: ", numero
        numero = numero ** 2
        call MPI_SEND(numero, 1, MPI_INTEGER, 0, 6000, MPI_COMM_WORLD, ierr)
        print *, "el proceso es: ", rankid, " envia el numero es: ", numero

      endif

      call MPI_FINALIZE(ierr);
end program hola_enviar
      
