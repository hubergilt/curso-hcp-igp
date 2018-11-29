#include<stdio.h>
int main() 
{
 float altura, peso, imc;
 printf("Cual es tu altura en m?\n");
 scanf("%f",&altura);
 printf("Cual es tu peso en kg?\n");
 scanf("%f",&peso);
 imc = peso/(altura*altura);
 printf("Tu indice de masa corporal (IMC) es: %f\n", imc);
 return 0;
}
