program numeros
      REAL :: Radio, Area, Circunferencia
      REAL, PARAMETER :: PI = 3.1416
      !El programa nos va permitir calcular numeros
      
      Radio=20.0
      Area=PI*Radio**2
      Circunferencia=2*PI*Radio

      print *,"El area del circulo es:", Area
      print *, "PI es ", PI
      print *, "La Circunferencia es: ", Circunferencia
end program numeros
