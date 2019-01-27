program main

    implicit none

    integer, parameter:: SUMA = 1
    integer, parameter:: RESTA= 2
    integer, parameter:: MULTI= 3
    integer, parameter:: DIVI = 4
    integer, parameter:: POTE = 5
    integer, parameter:: RESI = 6
    integer, parameter:: MODU = 7
    integer, parameter:: ARG  = 8
    integer, parameter:: LOGN = 9
    integer, parameter:: LOGA =10
    integer, parameter:: ABSO =11
    integer, parameter:: SEN  =12
    integer, parameter:: COSE =13
    integer, parameter:: FACTO =14
    integer, parameter:: PRIM =15
    integer, parameter:: PERF =16
    integer, parameter:: SALIR=17


    integer :: oper
    real :: resultado
    oper = SALIR
    resultado = 0
    do while(1.eq.1)
      call menu()

      oper = get_oper()
      print *,"La operacion seleccionada es >> ", oper
      if (oper.eq.SALIR) then
        exit
      end if
      
      if(oper.gt.0 .or. oper.lt.SALIR) then
        resultado=execute(oper)
        
        if(oper.eq.PRIM) then
          if(resultado.eq.1) then
            print *,"Resultado de la operacion es >> PRIMO"
          else 
            print *,"Resultado de la operacion es >> NO PRIMO"
          end if

        else if(oper.eq.PERF) then
          if(resultado.eq.1) then
            print *,"Resultado de la operacion es >> PERFECTO"
          else 
            print *,"Resultado de la operacion es >> NO PERFECTO"
          end if
        else
            print *,"Resultado de la operacion es >> ", resultado
        end if

        print *,""
      end if

    end do

contains

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

  integer function get_oper()
      integer :: option
      option = SALIR
      print *,""
      print *,"Ingrese una operacion valida [1-16] >> "
      read *, option
      get_oper = option
  end function get_oper

  real function execute(oper)
    integer :: oper
    real :: val1, val2

    select case (oper)
      case(SUMA)
        print *, "Operacion SUMA, ingrese : valor1 enter, y luego .. >> "
        read *, val1
        print *, "Operacion SUMA, ingrese el valor2 y enter >> "
        read *, val2
        execute=c_sumar(val1, val2)
      case(RESTA)
        print *, "Operacion RESTA, ingrese : valor1 enter, y luego .. >> "
        read *, val1
        print *, "Operacion RESTA, ingrese el valor2 y enter >> "
        read *, val2
        execute=c_restar(val1, val2)
      case(MULTI)
        print *, "Operacion MULTI, ingrese : valor1 enter, y luego .. >> "
        read *, val1
        print *, "Operacion MULTI, ingrese el valor2 y enter >> "
        read *, val2
        execute=c_multiplica(val1, val2)
      case(DIVI)
        print *, "Operacion DIVI, ingrese : valor1 enter, y luego .. >> "
        read *, val1
        print *, "Operacion DIVI, ingrese el valor2 y enter >> "
        read *, val2
        execute=c_divide(val1, val2)
      case(POTE)
        print *, "Operacion POTENCIA, ingrese : valor1 enter, y luego .. >> "
        read *, val1
        print *, "Operacion POTENCIA, ingrese el valor2 y enter >> "
        read *, val2
        execute=c_potencia(val1, val2)
      case(RESI)
        print *, "Operacion RESIDUO, ingrese : valor1 enter, y luego .. >> "
        read *, val1
        print *, "Operacion RESIDUO, ingrese el valor2 y enter >> "
        read *, val2
        execute=c_residuo(val1, val2)
      case(MODU)
        print *, "Operacion MODULO, ingrese : valor1 enter, y luego .. >> "
        read *, val1
        print *, "Operacion MODULO, ingrese el valor2 y enter >> "
        read *, val2
        execute=c_modulo(val1, val2)
      case(ARG)
        print *, "Operacion ARGUMENTO, ingrese : valor1 enter, y luego .. >> "
        read *, val1
        print *, "Operacion ARGUMENTO, ingrese el valor2 y enter >> "
        read *, val2        
        execute=c_argumento(val1, val2)
      case(LOGN)
        print *, "Operacion LOG NATURAL, ingrese : valor1 enter, y luego .. >> "
        read *, val1
        print *, "Operacion LOG NATURAL, ingrese el valor2 y enter >> "
        read *, val2  
        execute=c_ln(val1)
      case(LOGA)
        print *, "Operacion LOG BASE 1O, ingrese : valor1 enter, y luego .. >> "
        read *, val1
        print *, "Operacion LOG BASE 1O, ingrese el valor2 y enter >> "
        read *, val2  
        execute=c_ln10(val1)
      case(ABSO)
        print *, "Operacion ABSOLUTO, ingrese : valor1 y enter >> "
        read *, val1
        execute=c_abso(val1)
      case(SEN)
        print *, "Operacion SENO`, ingrese : valor1 y enter >> "
        read *, val1
        execute=c_seno(val1)
      case(COSE)
        print *, "Operacion COSENO, ingrese : valor1 y enter >> "
        read *, val1
        execute=c_cose(val1)
      case(FACTO)
        print *, "Operacion FACTORIAL, ingrese : valor1 y enter >> "
        read *, val1
        execute=c_fact(val1)
      case(PRIM)
        print *, "Operacion NUM. PRIMO, ingrese : valor1 y enter >> "
        read *, val1
        execute=c_prim(val1)
      case(PERF)
        print *, "Operacion NUM. PERFECTO, ingrese : valor1 y enter >> "
        read *, val1
        execute=c_perf(val1)                           
      case default
        execute=0
    end select
  end function execute

end program main
