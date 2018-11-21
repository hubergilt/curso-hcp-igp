program arreglo3
        integer:: i,j,k,filas,columnas
        real,allocatable :: arreglo1(:,:)
        print*,"Bienvenidos a arreglos"
        print*,"Cuantas filas tiene el arreglo"
        read*,filas
        print*,"Cuantas columnas tiene el arreglo"
        read*,columnas

        allocate (arreglo1(filas,columnas))

         do i=1,filas
                do j=1,columnas
                arreglo1(i,j)=i*j +j*4 +i
                end do 
        end do
        
        print*,"paso"
        do i=1,filas
                do j=1,columnas
                 print *, arreglo1(i,j)
                end do 
        end do
        
        deallocate(arreglo1)



end program arreglo3
