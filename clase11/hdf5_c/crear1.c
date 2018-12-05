#include<stdio.h>
#include<hdf5.h>
#define FILE "archivo1.h5"

int main()
{
  hid_t file_id;    //El indentificador del archivo 
  herr_t status;   //Status

  file_id = H5Fcreate(FILE,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);
  status= H5Fclose(file_id);


return 0;
}
