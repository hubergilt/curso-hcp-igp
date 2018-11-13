#include<stdio.h>
int main() 
{
 int edad;
 printf("Cual es edad?\n");
 scanf("%d",&edad);
 if (edad>0 && edad<3)
     printf("Eres un bebe\n");
 else if (edad>=3 && edad<6)
     printf("Eres un infante\n");
 else if (edad>=6 && edad<12)
     printf("Eres un niño\n");
 else if (edad>=12 && edad<18)
     printf("Eres un adolecente\n");
 else if (edad>=18 && edad<65)
     printf("Eres un adulto\n");
 else 
     printf("Eres un adulto mayor\n");
 return 0;
}
