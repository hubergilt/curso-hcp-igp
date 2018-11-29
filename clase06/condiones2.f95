program condiciones1
implicit none      
      integer :: edad
      print *, "Cual es su edad?"
      read *,edad

      if ((edad>=0).and.(edad<3)) then
        print *,"Eres un bebe"
      else if ((edad>=3).and.(edad<12)) then
        print *,"Eres un niño(a)"
      else if ((edad>=12).and.(edad<18)) then 
        print *,"Eres un adolecente"
      else if ((edad>=18).and.(edad<65)) then
        print *,"Eres un adulto"
      else 
        print *,"Eres un adulto mayor"
      end if

end program condiciones1

