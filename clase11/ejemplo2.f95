program prueba2
    USE HDF5
    IMPLICIT NONE
    
    CHARACTER(LEN=12), PARAMETER :: filename = "ejemplo2.h5" ! Nombre del archivo
    CHARACTER(LEN=4), PARAMETER :: dsetname = "dset" ! Nombre del Dataset
    
    INTEGER (HID_T) :: file_id
    INTEGER (HID_T) :: dset_id
    INTEGER (HID_T) :: dspace_id

    INTEGER (HSIZE_T) , DIMENSION(2) :: dims = (/4,6/)
    INTEGER :: RANK = 2

    INTEGER :: error 

    call h5open_f(error)

        call h5fcreate_f (filename,H5F_ACC_TRUNC_F,file_id,error)
    
            call h5screate_simple_f(rank,dims,dspace_id,error)

                call h5dcreate_f(file_id,dsetname,H5T_NATIVE_INTEGER,dspace_id,dset_id,error)

                call h5dclose_f(dset_id,error)

            call h5sclose_f(dspace_id,error)
    
        call h5fclose_f(file_id,error)
    
    call h5close_f(error)

end program prueba2
