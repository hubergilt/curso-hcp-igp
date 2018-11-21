program notasp
      implicit none
      real:: suma=0, promedio
      real:: notas(5)
      integer:: naprobados=0,i 

      print *, "Ingrese cinco notas de los alumnos"

      do i=1,5
       print *, "Ingreses a nota numero", i
       read *, notas(i)
      end do

      do i=1,5
       suma = suma+notas(i)
       if(notas(i)>10) then
        naprobados=naprobados+1
    end if
      end do
      promedio = suma/5;
    
      print *, "El promedio es: ", promedio
      print *, "El numero de aprobados es: ", naprobados
      print *, "El numero de desaprobados es: ", 5-naprobados

end program notasp
