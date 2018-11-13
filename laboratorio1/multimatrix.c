/*
    Created on Sun Nov 11 19:43:10 2018
    @author: hubergilt@hotmail.com    
*/

#include<stdio.h>
#include<stdlib.h>

void printf_matrix(int **arreglo1, int *nfilas, int *ncolumnas){
	int i=0,j=0;
	for (i=0;i<*nfilas;i++)
	{
		for(j=0;j<*ncolumnas;j++)
		{
			printf("[%d][%d] = %d \t",i,j,arreglo1[i][j]);
		}
		printf("\n");
	}
}

void scanf_matrix(int **arreglo1, int *nfilas, int *ncolumnas){
	int i=0,j=0,val=0;
	for (i=0;i<*nfilas;i++)
	{
		for(j=0;j<*ncolumnas;j++)
		{
			printf("Matrix Cuadrada, ingrese valor en [%d][%d] >> ", i, j);
	    	scanf("%d", &val);
			arreglo1[i][j]=val;
		}

	}	
}

void multi_matrix(int **arreglo1, int **arreglo2, int *nfilas, int *ncolumnas, int **resultado){
	int i=0,j=0,k=0,sum=0;
	
	for (i=0;i<*nfilas;i++)
	{
		for(j=0;j<*ncolumnas;j++)
		{
			resultado[i][j]=0;
		}
	}

	for (i=0;i<*nfilas;i++)
	{
		for(j=0;j<*ncolumnas;j++)
		{
			sum = 0;
			for(k=0;k<*ncolumnas;k++)
			{
				sum += arreglo1[i][k]*arreglo2[k][j];
				resultado[i][j]=sum;
			}
		}
	}
}

int main()
{
	int i=0,j=0;
	int nfilas    = 4;
	int ncolumnas = 4;
	int **arreglo1, **arreglo2, **resultado;
	
	printf("Multiplicacion de Matrices Cuadradas, ingrese : nfilas,ncolumnas >> ");
    scanf("%d,%d", &nfilas, &ncolumnas);	
    printf("El numero de filas y columnas seleccionadas son >> %d filas y %d columnas\n", nfilas, ncolumnas);

    if(nfilas==ncolumnas){
		arreglo1 = malloc(nfilas * sizeof(int *));
		arreglo2 = malloc(nfilas * sizeof(int *));
		resultado = malloc(nfilas * sizeof(int *));

		if(arreglo1 == NULL || arreglo2 == NULL || resultado == NULL)
		{
			fprintf(stderr, "Fracaso\n");
		}

		for(i = 0; i < nfilas; i++)
		{
			arreglo1[i] = malloc(ncolumnas * sizeof(int));
			arreglo2[i] = malloc(ncolumnas * sizeof(int));
			resultado[i] = malloc(ncolumnas * sizeof(int));

			if(arreglo1[i] == NULL || arreglo2[i] == NULL || resultado[i] == NULL)
			{
				fprintf(stderr, "Fracaso\n");
			}
		}

		printf("Ingrese elementos de la PRIMERA matriz cuadrada >>\n");
		scanf_matrix(arreglo1, &nfilas, &ncolumnas);
		printf("Ingrese elementos de la SEGUNDA matriz cuadrada >>\n");
		scanf_matrix(arreglo2, &nfilas, &ncolumnas);

		printf("\n");
		
		printf("PRIMERA matriz cuadrada ingresada >>\n");
		printf_matrix(arreglo1, &nfilas, &ncolumnas);
		printf("SEGUNDA matriz cuadrada ingresada >>\n");
		printf_matrix(arreglo2, &nfilas, &ncolumnas);

		multi_matrix(arreglo1, arreglo2, &nfilas, &ncolumnas, resultado);

		printf("\n");
		printf("RESULTADO de la Multiplicacion de Matrices >>\n");
		printf_matrix(resultado, &nfilas, &ncolumnas);

		free(arreglo1);
		free(arreglo2);
		free(resultado);
    }
    else{
		printf("La dimension de las matrices  no es cuadrada\n");
    }

}
