#include<stdio.h>

void calc_area_(double *,double*,double*);

int main()
{
    double radio, PI, area;
    int i,j,k;
    PI=3.1416;

    printf("Cual es el radio del circulo\n");
    scanf("%lf",&radio);
    printf("El radio es %lf y PI es %lf\n",radio,PI);

    calc_area_(&radio,&PI,&area);

    return 0;
}
