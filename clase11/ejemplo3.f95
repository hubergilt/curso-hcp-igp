
program prueba3
	USE HDF5
	IMPLICIT NONE
	
	CHARACTER(LEN=8), PARAMETER :: filename = "dsetf.h5" ! Nombre del archivo
	CHARACTER(LEN=4), PARAMETER :: dsetname = "dset" ! Nombre del Dataset
	CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2" ! Nombre del Dataset

	INTEGER (HID_T) :: file_id
	INTEGER (HID_T) :: dset_id,dset2_id
	INTEGER (HID_T) :: dspace_id,dspace2_id

	INTEGER (HSIZE_T) , DIMENSION(2) :: dims = (/4,6/)
	INTEGER :: RANK = 2

	INTEGER :: error
	INTEGER :: i,j,k
	INTEGER, DIMENSION(4,6) :: dset_data,data_out 
	
	do i=1,4
	  do j=1,6
  		dset_data(i,j)=(i-1)*6 + j
	  end do 
	end do 

	CALL h5open_f(error)

	CALL h5fcreate_f (filename,H5F_ACC_TRUNC_F,file_id,error)
	CALL h5screate_simple_f(rank,dims,dspace_id,error)
	CALL h5dcreate_f(file_id,dsetname,H5T_NATIVE_INTEGER,dspace_id,dset_id,error)
	
	call h5dwrite_f(dset_id,H5T_NATIVE_INTEGER,dset_data,dims,error)
	
	do i=1,4
	  do j=1,6
  		dset_data(i,j)=i+j
	  end do 
	end do 
	CALL h5screate_simple_f(rank,dims,dspace2_id,error)
	CALL h5dcreate_f(file_id,dsetname2,H5T_NATIVE_INTEGER,dspace2_id,dset2_id,error)
	call h5dwrite_f(dset2_id,H5T_NATIVE_INTEGER,dset_data,dims,error)

	call h5dread_f(dset_id,H5T_NATIVE_INTEGER,data_out,dims,error)
	do i=1,4
	  do j=1,6
  		print *, data_out(i,j)
	  end do 
	end do 


	call h5dclose_f(dset_id,error)
	call h5sclose_f(dspace_id,error)
	call h5dclose_f(dset2_id,error)
	call h5sclose_f(dspace2_id,error)

	call h5fclose_f(file_id,error)
	
	call h5close_f(error)

end program prueba3
