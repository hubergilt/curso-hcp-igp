
program azar

     INTEGER :: i,j,k,factor
     REAL :: numero1,numero2,numero3,seed
     INTEGER :: limite_superior, limite_inferior
     limite_superior=20
     limite_inferior=10
     !seed = call cpu_time()

      factor=10
      numero1=rand(1)

      print *,numero1
      numero1=factor*numero1
      print *,numero1
      numero2=int(numero1)
      print *,numero2
      numero3=limite_inferior+ (limite_superior-limite_inferior)*rand(1)
        !numero3=rand(1)  
      print *,numero3
end program azar
