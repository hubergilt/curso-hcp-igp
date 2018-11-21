program azararreglos

      INTEGER :: i,j,k
      REAL:: arreglo1(5)
      REAL :: variable1,variable2
      INTEGER,PARAMETER :: seed = 45789
      
        call RANDOM_SEED()
        print *,"Numero azar",rand(0),rand(1)

      do i=1,5

      print *,arreglo1(i),rand(1)*10

      end do



end program azararreglos
