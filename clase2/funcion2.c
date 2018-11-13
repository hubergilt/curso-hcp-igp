#include<stdio.h>
#define PI 3.1416
void parametros(float *radius,float *area_calc,float *circ, float *ang, float *arc);

int main ()
{
float radio, area, circunferencia, angulo, arco;
radio=342.34;
angulo=30;
parametros(&radio,&area,&circunferencia,&angulo,&arco);

printf("El radio es %f, el area es %f, circunferencia %f y arco %f\n",radio,area,circunferencia,arco);

}

void parametros(float *radius,float *area_calc,float *circ, float *angulo, float *arco)
{
*area_calc=PI*(*radius)*(*radius);
*circ = 2 * PI * (*radius);
*arco = (*angulo) * (*radius) * PI / 180;
}



