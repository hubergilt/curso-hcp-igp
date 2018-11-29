program primosp 
    implicit none

    integer :: i,j,divisores=0

    print *, "Los numeros primos del 1 al 100, son: "
    
    do i=1,100
      divisores = 0 
      do j=1,i
        if (mod(i,j) == 0) then
            divisores=divisores+1
        end if
      end do

      if (divisores==2) then
        print *, i
      end if
    end do

end program primosp
