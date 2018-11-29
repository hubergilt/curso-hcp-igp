#include<stdio.h>
//#include<string.h>
#include<stdlib.h>
int main()
{
int i,j,k;
int nfilas =4;
int ncolumnas=4;
int **arreglo1;
arreglo1 = malloc(nfilas * sizeof(int *));
	if(arreglo1 == NULL)
		{
		fprintf(stderr, "Fracaso\n");
		
		}
	for(i = 0; i < nfilas; i++)
		{
		arreglo1[i] = malloc(ncolumnas * sizeof(int));
		if(arreglo1[i] == NULL)
			{
			fprintf(stderr, "Fracaso\n");
			
			}
		}
for (i=0;i<nfilas;i++)
{
  for(j=0;j<ncolumnas;j++)
  {arreglo1[i][j]=j*3-4;
  }

}	
for (i=0;i<nfilas;i++)
{
  for(j=0;j<ncolumnas;j++)
  {printf("[%d][%d] = %d \t",i,j,arreglo1[i][j]);
  }
printf("\n");
}	

free(arreglo1);
}
