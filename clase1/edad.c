#include<stdio.h>
int main() 
{
 int edad, anio;
 float peso;
 printf("Cual es tu edad?\n");
 scanf("%d",&edad);
 printf("Que año naciste?\n");
 scanf("%d",&anio);
 printf("Cual es tu peso?\n");
 scanf("%f",&peso);
 printf("Tu edad es %d, naciste en %d y tu peso es %f\n", edad, anio, peso);
 return 0;
}
