#include<stdio.h>
#include<stdlib.h>
#include<pthread.h>
#define NUM_HILOS 40

void *printHola (void *hilo_id)
{
long t;
t = (long)hilo_id;

printf("Hola desde el hilo %ld \n",t);
pthread_exit(NULL);
}


void *printOI (void *hilo_id)
{
long t;
t = (long)hilo_id;

printf("OI desde el hilo %ld \n",t);
pthread_exit(NULL);
}

void *printHI (void *hilo_id)
{
long t;
t = (long)hilo_id;

printf("HI desde el hilo %ld \n",t);
pthread_exit(NULL);
}

void *printCIAO (void *hilo_id)
{
long t;
t = (long)hilo_id;

printf("CIAO desde el hilo %ld \n",t);
pthread_exit(NULL);
}



int main ()
{
int i,j,k,rc;
long t=0;
pthread_t arreglo_hilos[NUM_HILOS];

while(t<NUM_HILOS){
    if(t%4==0){
        rc=pthread_create(&arreglo_hilos[t],NULL,printHola,(void*)t);
        if (rc){
            printf("ERROR al crear el hilo %ld codigo %d \n",t,rc);
	        exit(-1);
        }
    }
    if(t%4==1){
        rc=pthread_create(&arreglo_hilos[t],NULL,printOI,(void*)t);
        if (rc){
            printf("ERROR al crear el hilo %ld codigo %d \n",t,rc);
	        exit(-1);
        }
    }
    if(t%4==3){
        rc=pthread_create(&arreglo_hilos[t],NULL,printHI,(void*)t);
        if (rc){
            printf("ERROR al crear el hilo %ld codigo %d \n",t,rc);
	        exit(-1);
        }
    }
    if(t%4==3){
        rc=pthread_create(&arreglo_hilos[t],NULL,printCIAO,(void*)t);
        if (rc){
            printf("ERROR al crear el hilo %ld codigo %d \n",t,rc);
	        exit(-1);
        }
    }
    t++;
}


// pthread_exit(NULL);
return 0;
}
