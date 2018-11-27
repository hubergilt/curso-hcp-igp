module constantes

  implicit none
  integer, parameter:: SUMA = 1
  integer, parameter:: RESTA= 2
  integer, parameter:: MULTI= 3
  integer, parameter:: DIVI = 4
  integer, parameter:: POTE = 5
  integer, parameter:: RESI = 6
  integer, parameter:: MOD  = 7
  integer, parameter:: ARG  = 8
  integer, parameter:: LOGN = 9
  integer, parameter:: LOG  =10
  integer, parameter:: ABS  =11
  integer, parameter:: SEN  =12
  integer, parameter:: COS  =13
  integer, parameter:: FACT =14
  integer, parameter:: PRIM =15
  integer, parameter:: PERF =16
  integer, parameter:: SALIR=17
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

  real function get_oper()
      integer :: option
      option = SALIR
      print *,""
      print *,"Ingrese una operacion valida [1-16] >> "
      read *, get_oper
      get_oper = option
  end function get_oper

  real function sumar(val1,val2)
      real :: val1, val2
      print *, "Operacion SUMAR, ingrese : valor1+valor2 >> "
      read (*,*), val1, val2
      sumar = val1+val2;
  end function sumar

  real function restar(val1,val2)
      real :: val1, val2
      print *, "Operacion RESTAR, ingrese : valor1-valor2 >> "
      read (*,*), val1, val2
      restar = val1-val2;
  end function restar

  real function multiplica(val1,val2)
      real :: val1, val2
      print *, "Operacion MULTIPLICA, ingrese : valor1*valor2 >> "
      read (*,*), val1, val2
      multiplica = val1*val2;
  end function multiplica

  real function divide(val1,val2)
      real :: val1, val2
      print *, "Operacion DIVIDE, ingrese : valor1/valor2 >> "
      read (*,*), val1, val2
      divide = val1/val2;
  end function divide

  real function potencia(val1,val2)
      real :: val1, val2
      print *, "Operacion POTENCIA, ingrese : valor1^valor2 >> "
      read (*,*), val1, val2
      potencia = val1**val2;
  end function potencia

  real function residuo(val1,val2)
      real :: val1, val2
      print *, "Operacion RESIDUO, ingrese : valor1%valor2 >> "
      read (*,*), val1, val2
      residuo = mod(val1, val2)
  end function residuo

  real function modulo(val1,val2)
      real :: val1, val2
      print *, "Operacion MODULO, ingrese : valor1|valor2 >> "
      read (*,*), val1, val2
      modulo = sqrt(val1**2+val2**2)
  end function modulo

  real function argumento(val1,val2)
      real :: val1, val2
      print *, "Operacion ARGUMENTO, ingrese : valor1?valor2 >> "
      read (*,*), val1, val2
      argumento = atan(val1**2+val2**2)
  end function argumento

  real function ln(val1)
      real :: val1
      print *, "Operacion LOG NATURAL, ingrese : valor1 >> "
      read *, val1
      ln = log(val1)
  end function ln

  real function ln10(val1)
      real :: val1
      print *, "Operacion LOG BASE 10, ingrese : valor1 >> "
      read *, val1
      ln10 = log10(val1)
  end function ln10

  real function abso(val1)
      real :: val1
      print *, "Operacion ABS, ingrese : valor1 >> "
      read *, val1
      abso = abs(val1)
  end function abso

  real function seno(val1)
      real :: val1
      print *, "Operacion SIN, ingrese : valor1 >> "
      read *, val1
      seno = sin(val1)
  end function seno

  real function cose(val1)
      real :: val1
      print *, "Operacion COS, ingrese : valor1 >> "
      read *, val1
      cose = cos(val1)
  end function cose

  recursive function factorial(val1) result(facto)
      real :: facto
      facto = 0
      if (val1.eq.0) then
        facto = 1
      else
        facto = val1-1;
        facto = val1*factorial(facto)
      endif
      !factorial = facto
  end function factorial


  real function fact(val1)
      real :: val1
      print *, "Operacion FACT, ingrese : valor1 >> "
      read *, val1
      fact = factorial(val1)
  end function fact

  real function prim(val1)
      real :: val1, i
      integer :: esprimo, cuenta
      print *, "Operacion PRIMO, ingrese : valor1 >> "
      read *, val1
      esprimo = 1
      i = 1
      cuenta = 0
      if (val1.eq.0 .or. val1.eq.1) then
        esprimo  = 1
      else
          do while(i.le.val1)
              if((mod(val1,i)).eq.0) then
                  cuenta=cuenta+1
              endif
              i=i+1
          enddo
          if(cuenta.eq.2) then
              esprimo=1
          endif
      endif
      prim=esprimo
  end function prim

  real function perf(val1)
      real :: val1, i
      integer :: esperfecto, cuenta, suma
      integer, allocatable :: divisores(:)
      print *, "Operacion NUM PERFECTO, ingrese : valor1 >> "
      read *, val1

      allocate (divisores(100))  

      esperfecto=0
      i=1
      cuenta = 0

      do while(i.lt.val1)
          if((mod(val1,i)).eq.0) then
              cuenta=cuenta+1
              divisores(cuenta)=i
          endif
          i=i+1
      enddo

      esprimo = 1
      i = 0

      do while(cuenta.gt.-1)
          suma=suma+divisores(cuenta)
          cuenta=cuenta-1
      enddo

      if(suma.eq.val1) then
          esperfecto=1
      endif

      deallocate(divisores)

      perf=esperfecto
  end function perf


  real function execute(oper)
    real :: oper, val1, val2
    select case (oper)
      case(SUMA)
        execute=sumar(val1, val2)
      case(REST)
        execute=restar(val1, val2)
      case(MULT)
        execute=multiplica(val1, val2)
      case(DIVI)
        execute=divide(val1, val2)
      case(POTE)
        execute=potencia(val1, val2)
      case(RESI)
        execute=residuo(val1, val2)
      case(MOD)
        execute=modulo(val1, val2)
      case(ARG)
        execute=argumento(val1, val2)
      case(LOGN)
        execute=ln(val1)
      case(LOG)
        execute=ln10(val1)
      case(ABS)
        execute=abso(val1)
      case(SEN)
        execute=seno(val1)
      case(COS)
        execute=cose(val1)
      case(FACT)
        execute=fact(val1)
      case(PRIM)
        execute=prim(val1)
      case(PERF)
        execute=perf(val1)                           
      case default
        execute=0
    end select
  end function execute

end module constantes



program calculadora
    use constantes    
    implicit none
    integer :: oper
    real :: resultado
    oper = SALIR
    resultado = 0
    do while(1.eq.1)
      call menu()
      oper = get_oper()
      print *,"La operacion seleccionada es >> ", oper
      if (oper.eq.SALIR) then
        break
      endif
      if(oper.gt.0 .or. oper.lt.SALIR) then
        resultado=execute(oper)
        if(oper.eq.PRIM) then
          if(resultado.eq.1) then
            print *,"Resultado de la operacion es >> PRIMO"
          else 
            print *,"Resultado de la operacion es >> NO PRIMO"
          endif
        else if(oper.eq.PERF) then
          if(resultado.eq.1) then
            print *,"Resultado de la operacion es >> PRIMO"
          else 
            print *,"Resultado de la operacion es >> NO PRIMO"
          endif
        else
            print *,"Resultado de la operacion es >> ", resultado
        endif
      endif
    end do


end program calculadora
