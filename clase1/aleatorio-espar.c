#include <stdlib.h>
#include <stdio.h>
#include <time.h>

int main(){
    int numero;
    srand(time(0));
    int i;
    for(i=0;i<5;i++){
        numero=rand()%20;
        printf("%d \n",numero);
        if(numero%2==0){ printf("El numero es par\n"); }
    }
}
