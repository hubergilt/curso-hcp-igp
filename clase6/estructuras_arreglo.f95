program estructura1
        type Dni
        character(len=20) :: nombre
        character(len=25) :: apellido
        integer:: numero,grupo
        end type Dni
        type (Dni) :: usuario1
        type (Dni) :: usuario2
        type(Dni),dimension(3)::lista       

        print*,"Bienvenido a RENIEC"

        !Ingresamos la informacion a nuestra estructura DNI

         lista(1)%nombre="Emilio"
         lista(1)%apellido="Bustamante"
         lista(1)%numero=43239
         lista(1)%grupo=342

         print*,"Cual es tu nombre"
         read*,lista(2)%nombre
         print*,"Cual es tu apellido"
         read*,lista(2)%apellido
         print*,"Cual es el numero"
         read*,lista(2)%numero
         print*,"Cual es el grupo de votacion"
         read*,lista(2)%grupo


         lista(3)%nombre="Huber"
         lista(3)%apellido="Gilt"
         lista(3)%numero=42166147
         lista(3)%grupo=1234

         print*,lista(1)%apellido     
         print*,lista(1)%numero
         print*,lista(2)%apellido
         print*,lista(2)%numero
         print*,lista(3)%apellido
         print*,lista(3)%numero

end program estructura1
