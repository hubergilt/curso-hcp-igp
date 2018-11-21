program circulo 

        INTEGER:: i,j,k
        REAL :: radio,area,circum
        REAL,PARAMETER :: PI =3.1416


        print*,"Cual es el radio del circulo"
        read *,radio
        call area_circ(radio,PI,area)
        print*,"El area es ",area

   contains

        subroutine area_circ(radius,PIus,area_total)
        REAL:: radius, area_total,PIus
        area_total=PIus * radius**2

        end subroutine area_circ


end program circulo


