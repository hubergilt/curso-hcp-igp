
#include<stdio.h>
#include<stdlib.h>
#include<time.h>

int main()
{
int **arr1;
int *extra1;
int i,j,k;
int filas,columnas;

srand(time(NULL));
printf("Numeros de filas\n");
scanf("%d",&filas);
printf("Numero de columnas\n");
scanf("%d",&columnas);

extra1 =(int*)malloc(filas*columnas*sizeof(int));

arr1=(int**)malloc(filas*sizeof(int*));
for(i=0;i<filas;i++)
{
arr1[i]=extra1+columnas*i;	
}


for (i=0;i<filas;i++)
{
 for (j=0;j<columnas;j++)
 {arr1[i][j]=rand()%10;
  printf("%x\t",&arr1[i][j]);
 }

printf("\n");
}

/*
for(i=0;i<filas;i++)
{
 for(j=0;j<columnas;j++)
 {printf("%d\t",arr1[i][j]);
 }
printf("\n");
}
*/


return 0;
}
