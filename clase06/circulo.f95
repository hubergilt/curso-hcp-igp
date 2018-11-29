program circulo
    integer :: i,j,k
    real :: radio, area, circum
    real, parameter :: PI = 3.1416

    print*, "Cual es el radio del circulo"
    read *, radio
    area=area_circ(radio,PI)
    print*, "El area es ", area
    circum=circum_circ(radio,PI)
    print*, "El circulo es ", circum
end program circulo

function area_circ(radius, PIus)
      real:: radius, aera_total, PIus
      area_total=PIus * radius**2
      area_circ=area_total
end function area_circ

function circum_circ(radius, PIus)
      real:: radius, circum_total, PIus
      circum_total=2*PIus * radius
      circum_circ=circum_total
end function circum_circ
