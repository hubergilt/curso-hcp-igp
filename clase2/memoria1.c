#include<stdio.h>
//#include<string.h>
//#include<stdarg.h>
#include<stdlib.h>

int main()
{
//char nombre[20];
char *miNombre= NULL;
int tamano=0;
int rc =0;
printf("Cuan largo es tu nombre?\n");
scanf("%d",&tamano);

miNombre=(char*)malloc(tamano*sizeof(char));

if(miNombre !=NULL)
{
printf("Ingresa tu nombre? :\n");
rc=scanf("%s",miNombre);
printf("Hola %s \n",miNombre);
free(miNombre);
}
else
{
	printf("ERROR alocando la memoria\n");
	rc=-1;
}

return rc;
}
