program bmip

    real :: peso,altura,bmi

    print *, "Ingrese su altura (m)"
    read *, altura

    print *, "Ingrese su peso (kg)"
    read *, peso

    bmi = peso/altura**2

    print *, "Su indice BMI es:", bmi

end program bmip
