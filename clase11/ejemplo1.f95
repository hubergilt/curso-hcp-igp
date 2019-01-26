program prueba1
    use HDF5
    implicit none
    character(len=8), PARAMETER :: filename = "archivo1.h5"
    integer(HID_T):: file_id
    integer :: error

    call h5open_f (error)
    call h5fcreate_f (filename,H5F_ACC_TRUNC_F,file_id,error)
    call h5fclose_f (file_id,error)
    call h5close_f(error)


end program prueba1
