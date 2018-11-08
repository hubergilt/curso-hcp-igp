#include<stdio.h>
#include<stdlib.h>

int main()
{
int *arreglo1;
int tamano;
int i,j,k;
printf("Cual es el tamano del arreglo\n");
scanf("%d",&tamano);
arreglo1=(int*)malloc(tamano*sizeof(int));
if(arreglo1 != NULL)
{
printf("Se asigno sin ningun problema\n");
for (i=0;i<tamano;i++)
{
printf("ingrese el elemeno %d \n",i);
scanf("%d",&arreglo1[i]);
}
for (i=0;i<tamano;i++)
{
printf("direccion de arreglo[%d]:%x\n",i,&arreglo1[i]);
printf("arreglo[%d]: %d \n",i,arreglo1[i]);
}
free(arreglo1);
}
else
{printf("ERROR \n:");
}






return 0;
}
