
program arreglo1
        integer :: i,j,k,tamano
        real :: var1,var2,var3
        real :: arr1(5) 
        real , allocatable::arr2(:) !Declaro un arregloe unidemensional sin dimensione
        integer , allocatable::arr3(:) !Declaro un arregloe unidemensional sin dimensione

        print*,"Bienvenidos a arreglos"
        
        do i=1,5
        arr1(i)=rand()*10
        end do
        
        do i=1,5
        print *,arr1(i)
        end do

        print*,"Cual es la dimension del segundo arreglo"
        read*,tamano ! El tamano del arreglo que voy a alocar

        allocate ( arr2 (tamano) ) !Allocate el arr2
        
        do i=1,tamano
        arr2(i)=rand()*10
        end do
        
        do i=1,tamano
        print *,arr2(i)
        end do
        deallocate(arr2)

        print*,"Cual es la dimension del tercer arreglo"
        read*,tamano ! El tamano del arreglo que voy a alocar

        allocate ( arr3 (tamano) ) !Allocate el arr2
        
        do i=1,tamano
        arr3(i)=rand()*10
        end do
        
        do i=1,tamano
        print *,arr3(i)
        end do

        deallocate(arr3)

end program arreglo1
