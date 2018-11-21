module constantes

implicit none
      REAL, PARAMETER :: PI=3.1416
      REAL, PARAMETER :: e=2.718281

contains

        subroutine area_circ(radius,area_total)
        REAL::radius,area_total
        area_total=PI * radius**2
                
        end subroutine area_circ

end module constantes


program circulo
use constantes
        REAL::radio,area,circum
        print *,"Cual es el radio del circulo"
        read  *,radio
        call area_circ(radio,area)

        print *,"El area es",area

end program circulo

