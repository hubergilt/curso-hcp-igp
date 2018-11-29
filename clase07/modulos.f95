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

module variables

implicit none
      REAL, PARAMETER :: PI=3.1416
      REAL, PARAMETER :: e=2.718281

contains

        subroutine circum_circ(radius,area_total)
        REAL::radius,area_total
        area_total=PI * radius*2
                
        end subroutine circum_circ

end module variables


program circulo
use constantes
use variables
        REAL::radio,area,circum
        print *,"Cual es el radio del circulo"
        read  *,radio
        call area_circ(radio,area)
        print *,"El area es",area
        call circum_circ(radio,circum)
        print *,"La circunferencia es",circum

end program circulo

