program estructura1
        type Dni
        character(len=20) :: nombre
        character(len=25) :: apellido
        integer:: numero,grupo
        end type Dni
        type (Dni) :: usuario1
        type (Dni) :: usuario2
        

        print*,"Bienvenido a RENIEC"

        !Ingresamos la informacion a nuestra estructura DNI

         usuario1%nombre="Renzo"
         usuario1%apellido="Bustamante"
         usuario1%numero=10234355
         usuario1%grupo=122245
        
         print*,"Cual es tu nombre"
         read*,usuario2%nombre
         print*,"Cual es tu apellido"
         read*,usuario2%apellido
         print*,"Cual es el numero"
         read*,usuario2%numero
         print*,"Cual es el grupo de votacion"
         read*,usuario2%grupo

         print*,usuario1%apellido
         print*,usuario1%numero
         print*,usuario2%apellido
         print*,usuario2%numero

end program estructura1
