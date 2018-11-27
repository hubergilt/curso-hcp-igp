program calculadora
    implicit none
    call menu()

end program calculadora

subroutine menu()
      print *,"**********************************************"
      print *,"*       Bienvenidos a la calculadora         *"
      print *,"**********************************************"
      print *,"*  MENU:                                     *"
      print *,"*  1) suma            9) ln                  *"
      print *,"*  2) resta          10) log                 *"
      print *,"*  3) multiplicion   11) abs                 *"
      print *,"*  4) division       12) sen                 *"
      print *,"*  5) potencia       13) cos                 *"
      print *,"*  6) residuo        14) factorial           *"
      print *,"*  7) modulo         15) primo               *"
      print *,"*  8) argumento      16) perfecto            *"
      print *,"*                    17) SALIR               *"
      print *,"**********************************************"
end subroutine menu