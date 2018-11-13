#include <stdlib.h>
#include <stdio.h>
#include <time.h>

int main(){
    int numero;
    srand(time(0));
    int i;
    for(i=0;i<5;i++){
        numero=rand()%20;
        if(numero%2==0){ printf("%d es par\n",numero); }
        else{ printf("%d\n",numero); }
    }
}
