#include <stdio.h>
int main(){
 int i,j;
 int arreglo1[5];
 double arreglo2[5];
 int arreglo3[5][5];
 printf("posiciones de memoria arreglo int\n");
 for(i=0;i<5;i++){
  printf("%x\n",&arreglo1[i]);
 }
 printf("posiciones de memoria arreglo double\n");
 for(i=0;i<5;i++){
  printf("%x\n",&arreglo2[i]);
 }
 printf("posiciones de memoria arreglo dos dimenciones int\n");
 for(i=0;i<5;i++){
  printf("fila: %d\n",i);
  for(j=0;j<5;j++){
   printf("%x\n",&arreglo3[i][j]);
  }
 }
 return 0;
}
