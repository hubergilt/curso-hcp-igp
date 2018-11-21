
program arreglo1
        integer :: i,j,k
        real :: var1,var2,var3
        real :: arr1(5) 

        print*,"Bienvenidos a arreglos"
        
        do i=1,5
        arr1(i)=rand()*10

        end do
        
        do i=1,5
        print *,arr1(i)
        end do


end program arreglo1
