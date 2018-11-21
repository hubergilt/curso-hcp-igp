program arreglosp
      implicit none
      integer:: i,j,k
      integer:: arreglos1(5)

      print *, "Bienvenidos a Arreglos UNI-Dimensionales"

      do i=1,5
       arreglos1(i)=i*2
      end do

      do i=1,5
       print *, arreglos1(i)
      end do

end program arreglosp
