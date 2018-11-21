
program datos2
        character (len=20) :: parte
        integer :: codigo
        real :: costo
        parte ="CPUXEON"
        codigo=2343
        precio=510.45654879
        
        print *,"INVENTARIO"
        
        print 100
        100 format (7x,"Nombre:",7x,"COD:",1x,"Precio")
        print 200,parte,codigo,precio 
        200 format (1x,a,2x,i5,2x,f10.4)

end program datos2
