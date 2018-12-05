#include<stdio.h>

int cfun2_(double *pi, double *radio)
{

double PI= *pi;
double RADIUS = *radio;


double CIRCUNFERENCIA;
CIRCUNFERENCIA= PI * RADIUS*2;
printf("La circunferencia del circulo es %lf\n",CIRCUNFERENCIA);
return 0;
}


