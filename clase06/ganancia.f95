PROGRAM propinap

      implicit none
      real :: precio_venta, precio_fabrica, ganancia
      integer :: productos

      print *, "Ingrese el numero de productos"
      read *, productos

      print *, "Ingrese el precio de venta"
      read *, precio_venta
 
      print *, "Ingrese el precio de fabrica"
      read *, precio_fabrica
 
      ganancia = productos*(precio_venta-precio_fabrica)

      print *, "El valor de la ganacia es ", ganancia

END PROGRAM propinap
