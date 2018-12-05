#include<stdio.h>
#include<hdf5.h>
#define FILE "dset.h5"

int main()
{

hid_t file_id, dataset_id, dataspace_id; // Identificadores
hsize_t dims[2];
herr_t status;

file_id = H5Fcreate(FILE,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);

dims[0]=4;
dims[1]=6;
dataspace_id= H5Screate_simple(2,dims,NULL);

dataset_id = H5Dcreate2(file_id,"/dset",H5T_STD_I32BE,dataspace_id,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
// Cerramos el Data set
status = H5Dclose(dataset_id);
// Cerramos el Data space
status = H5Sclose(dataspace_id);
// Cerramos el archivo 
status = H5Fclose(file_id);

return 0;
}
