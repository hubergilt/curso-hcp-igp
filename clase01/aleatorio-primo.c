#include <stdlib.h>
#include <stdio.h>
#include <time.h>

int main(){
    int numero;
    srand(time(0));
    int i,j;
    int esprimo=0;
    for(i=0;i<5;i++){
        numero=rand()%20;
        esprimo=0;
        for(j=1;j<=numero;j++){
            if(numero%j==0){ esprimo++; }
        }         
        if(esprimo==2 || numero==1){ 
	  printf("%d es primo\n", numero);
       	}
	else {
          printf("%d \n", numero);
	}
    }
}
