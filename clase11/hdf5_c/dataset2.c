#include<stdio.h>
#include<hdf5.h>
#define FILE "dset.h5"
#include<stdlib.h>
#include<time.h>
int main()
{

hid_t file_id, dataset_id, dataspace_id; // Identificadores
hsize_t dims[2];
herr_t status;
int i,j,k,dset_data[4][6];
file_id = H5Fcreate(FILE,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);
srand(time(NULL));
dims[0]=4;
dims[1]=6;
dataspace_id= H5Screate_simple(2,dims,NULL);

dataset_id = H5Dcreate2(file_id,"/dset",H5T_STD_I32BE,dataspace_id,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
// Cerramos el Data set
for (i=0;i<4;i++)
{
	for (j=0;j<6;j++)
	{
	dset_data[i][j]=rand()%10;
	}
}

status=H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,dset_data);
status=H5Dread(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,dset_data);

status = H5Dclose(dataset_id);
// Cerramos el Data space
status = H5Sclose(dataspace_id);
// Cerramos el archivo 
status = H5Fclose(file_id);

return 0;
}
