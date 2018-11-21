program circulo 

        INTEGER:: i,j,k
        REAL :: radio,area,circum
        REAL,PARAMETER :: PI =3.1416


        print*,"Cual es el radio del circulo"
        read *,radio
        print*,"El radio que ingresaste es:",radio
        call area_circ(radio,PI,area)
        print*,"El area es ",area
        print*,"El nuevo radio es ",radio

end program circulo

subroutine area_circ(radius,PIus,area_total)
        REAL:: radius, area_total,PIus
        area_total=PIus * radius**2
        radius=20

end subroutine area_circ

