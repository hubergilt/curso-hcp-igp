program localidad
        integer:: i,j
        integer:: arr1(5,5)
        
        do j =1,5
                do i=1,5
                write (*,fmt="(1x,a,i0)",advance="no")"" ,LOC(arr1(i,j))
                end do 
        print *,""
        end do 



end program localidad
