#include<stdio.h>
#include<stdlib.h>
#include<pthread.h>
#include<assert.h>

double **matriz1;
double **matriz2;
double **matriz3;
void allocar_memoria(int filas,int columnas);
void imprimir_matrix(int filas,int columnas);
void llenar_numeros(int filas,int columnas);

int main()
{
int i,j,k;
int m,n;
double var1,var2;
printf("Multiplicacion de Matrices\n");
printf("Cuantas filas tiene la matriz 1 ?\n");
scanf("%d",&m);
printf("Cuantas columnas tiene la matriz 1 ?\n");
scanf("%d",&n);
printf("La Matriz 1 es %d x %d \n",m,n);
printf("La Matriz 2 es %d x %d \n",m,n);
printf("La Mtraiz 3 es %d x %d \n",m,n);

allocar_memoria(m,n);
llenar_numeros(m,n);
//imprimir_matrix(n,n);

for (i=0;i<m;i++)
{
	for (j=0;j<n;j++)
	{
		for (k=0;k<m;k++)
		{
		matriz3[i][j]+=matriz1[i][k]*matriz2[k][j];
		}
			
	}
}

printf("Despues de multiplicar \n");
//imprimir_matrix(m,n);
printf("Fin del programa \n");

}//fin funcion principal

void allocar_memoria(int filas,int columnas)
{
int i,j,k;
matriz1=(double**)malloc(filas*sizeof(double*));
	for (i=0;i<filas;i++)
	{
	matriz1[i]=(double*)malloc(columnas*sizeof(double));
	}

matriz2=(double**)malloc(filas*sizeof(double*));
	for (i=0;i<filas;i++)
	{
	matriz2[i]=(double*)malloc(columnas*sizeof(double));
	}

matriz3=(double**)malloc(filas*sizeof(double*));
	for (i=0;i<filas;i++)
	{
	matriz3[i]=(double*)malloc(columnas*sizeof(double));
	}

} //Fin de Allocar Memoria

void imprimir_matrix(int filas,int columnas)
{
int i,j,k;
printf("MATRIX 1\n");
for (i=0;i<filas;i++)
{
	for (j=0;j<columnas;j++)
	{
	printf("%lf \t",matriz1[i][j]);
	}
printf("\n");
}
printf("MATRIX 2\n");
for (i=0;i<filas;i++)
{
	for (j=0;j<columnas;j++)
	{
	printf("%lf \t",matriz2[i][j]);
	}
printf("\n");
}
printf("MATRIX 3\n");
for (i=0;i<filas;i++)
{
	for (j=0;j<columnas;j++)
	{
	printf("%lf \t",matriz3[i][j]);
	}
printf("\n");
}
}//Fin de Imprimir Arreglos



void llenar_numeros(int filas, int columnas)
{
int i,j,k;
for (i=0;i<filas;i++)
{
	for (j=0;j<columnas;j++)
	{
	matriz1[i][j]=j*3+2*(rand()%15) ;
	matriz2[i][j]=j*3+2*(rand()%15) ;

	}

}

}//Fin de llenar_numeros





