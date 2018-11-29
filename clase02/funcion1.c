#include<stdio.h>
#define PI 3.1416
float area_circ(float radius); 
float circun_circ(float rad);
float arco_circ(float radius, float angulo);
float extra;
int prueba;
int main()

{
float radio,area,circun,angulo,arco;
extra = 4334.3;
prueba =12;
radio=34.3;
angulo=30.0;
printf("Extra es %f \n",extra);
area=area_circ(radio);
circun=circun_circ(radio);
arco=arco_circ(radio, angulo);
printf("Extra es %f \n",extra);
printf("El area del circulo es  %f y la circum %f \n",area,circun);
printf("El arco con angulo %f y radio %f, es:  %f \n",angulo,radio,arco);
printf("%d\n",prueba);
return 0;
}

float area_circ(float radius)
{
float area_calculada;
area_calculada= PI *radius*radius;
extra =3.233443;
printf("Extra es %f \n",extra);
return area_calculada;
}

float circun_circ(float rad)
{
float circunferencia;
circunferencia= 2*rad*PI;
prueba=15;
return circunferencia;

}

float arco_circ(float radio, float angulo){
 float arco_calculado;
 arco_calculado = PI * radio * angulo / 180;
 return arco_calculado;
}
