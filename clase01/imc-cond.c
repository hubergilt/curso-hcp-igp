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
 if (imc>25)
     printf("Estas en sobrepeso\n");
 else
     printf("Estas saludable\n");
 return 0;
}
