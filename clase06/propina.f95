PROGRAM propinap

      IMPLICIT NONE
      real :: valor, porcentaje, propina

      print *, "Ingrese el valor de la cuenta"
      read *, valor

      print *, "Ingrese porcentaje de la propina"
      read *, porcentaje
 
      propina = valor * porcentaje / 100

      print *, "El valor de la propina es ", propina
END PROGRAM propinap
