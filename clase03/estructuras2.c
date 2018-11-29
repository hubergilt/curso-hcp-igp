#include<stdio.h>
#include<string.h>
#include<stdlib.h>

struct Libros{
	char autor[50];
	char titulo[50];
	int isbn;
};
struct Libros Coleccion[3];
struct Libros* Biblioteca;

int main()
{
	struct Libros Libro1;
	int cantidad;
	int i,j,k;
	printf ("Cual es la cantidad de libros en la biblioteca\n");
	scanf("%d",&cantidad);

	struct Libros* Biblioteca = malloc(cantidad*sizeof(struct Libros));

	strcpy(Libro1.titulo,"Rayuela");
	strcpy(Libro1.autor,"Cortazar");
	Libro1.isbn=394834;

	printf("Libro 1 titulo  es %s\n",Libro1.titulo);
	printf("Libro 1 autor es %s\n",Libro1.autor);
	printf("Libro 1 ISBN es %d\n",Libro1.isbn);

	strcpy(Coleccion[0].titulo,"Quijote");
	strcpy(Coleccion[0].autor,"Saavedra");
	Coleccion[0].isbn=394434324;

	printf("Coleccion[0] titulo  es %s\n",Coleccion[0].titulo);
	printf("Coleccion[0] autor es %s\n",Coleccion[0].autor);
	printf("Coleccion[0] 1 ISBN es %d\n",Coleccion[0].isbn);


	strcpy(Coleccion[1].titulo,"Thinking in Java");
	strcpy(Coleccion[1].autor,"JP Deitel");
	Coleccion[0].isbn=394434325;

	printf("Coleccion[1] titulo  es %s\n",Coleccion[1].titulo);
	printf("Coleccion[1] autor es %s\n",Coleccion[1].autor);
	printf("Coleccion[1] 1 ISBN es %d\n",Coleccion[1].isbn);


	strcpy(Coleccion[2].titulo,"Grails in action");
	strcpy(Coleccion[2].autor,"Gen Smith");
	Coleccion[2].isbn=394434326;

	printf("Coleccion[2] titulo  es %s\n",Coleccion[2].titulo);
	printf("Coleccion[2] autor es %s\n",Coleccion[2].autor);
	printf("Coleccion[2] 1 ISBN es %d\n",Coleccion[2].isbn);


	printf("Ingresando datos de la Biblioteca \n");

	for (i=0;i<cantidad;i++)
	{
		printf("Cual es el nombre del libro %d ?\n",i);
		scanf("%s",Biblioteca[i].titulo);
		printf("Cual es el nombre del autor %d ?\n",i);
		scanf("%s",Biblioteca[i].autor);
		printf("Cual es el numero isbn de %d ?\n",i);
		scanf("%d",&Biblioteca[i].isbn);
	}


	for (i=0;i<cantidad;i++)
	{
		printf("Coleccion[%d] titulo  es %s\n",i,Biblioteca[i].titulo);
		printf("Coleccion[%d] autor es %s\n",i,Biblioteca[i].autor);
		printf("Coleccion[%d] 1 ISBN es %d\n",i,Biblioteca[i].isbn);
	}
	return 0;
}
