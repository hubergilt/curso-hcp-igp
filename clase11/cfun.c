#include<stdio.h>

int cfun_(double *pi, double *radio)
{

double PI= *pi;
double RADIUS = *radio;


double AREA;
AREA= PI * RADIUS*RADIUS;
printf("El Area del circulo es %lf\n",AREA);
return 0;
}


