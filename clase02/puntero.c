#include<stdio.h>

int main(){
 double numero;
 double *ptr;
 int arreglo[3];
 double *ptr1;
 int *ptr2;

 numero=3.1214;

 arreglo[0]=234;
 arreglo[1]=34;
 arreglo[2]=15;

 ptr1=&numero;
 ptr2=&arreglo[0];
 printf("%x\n", ptr1);
 *ptr1=3.20;

 printf("%lf\n",numero);
 printf("%d\n", *ptr2);
 printf("%d\n", *(ptr2+1));
 printf("%d\n", *(ptr2+2));

 *ptr2=300;
 *(ptr2+1)=500;
 *(ptr2+2)=1000;

 printf("%d\n",arreglo[0]);
 printf("%d\n",arreglo[1]);
 printf("%d\n",arreglo[2]);

}
