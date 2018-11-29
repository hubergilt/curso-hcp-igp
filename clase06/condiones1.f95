program condiciones1
implicit none      
      INTEGER :: edad
      print *, "Cual es su edad?"
      read *,edad

      if (edad < 18) then
      print *,"No puede entrar al club!"
      else
      print *,"Si puede entrar al club!"
      end if

end program condiciones1

