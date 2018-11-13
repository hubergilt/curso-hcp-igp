#include<stdio.h>
#include<stdlib.h>
#include<pthread.h>

void *printHola( void *id_hilo)
{
long hilo_id;
hilo_id = (long)id_hilo;

printf("Hola desde el hilo %ld \n",hilo_id);

pthread_exit(NULL);
}


void *printChau( void *id_hilo)
{
long hilo_id;
hilo_id = (long)id_hilo;

printf("Chau desde el hilo %ld \n",hilo_id);

pthread_exit(NULL);
}

int main()
{
int i,j,k,rc;
long t;
pthread_t hilo0, hilo1, hilo2, hilo3, hilo4, hilo5;
t=0;
rc = pthread_create(&hilo0,NULL,printHola,(void*)t);
if (rc){printf("ERROR creando hilo %ld , codigo %d \n",t,rc);
	exit(-1);}

t=1;
rc = pthread_create(&hilo1,NULL,printHola,(void*)t);
if (rc){printf("ERROR creando hilo %ld , codigo %d \n",t,rc);
	exit(-1);}

t=2;
rc = pthread_create(&hilo2,NULL,printHola,(void*)t);
if (rc){printf("ERROR creando hilo %ld , codigo %d \n",t,rc);
	exit(-1);}

t=3;
rc = pthread_create(&hilo3,NULL,printChau,(void*)t);
if (rc){printf("ERROR creando hilo %ld , codigo %d \n",t,rc);
	exit(-1);}


t=4;
rc = pthread_create(&hilo4,NULL,printChau,(void*)t);
if (rc){printf("ERROR creando hilo %ld , codigo %d \n",t,rc);
	exit(-1);}

t=5;
rc = pthread_create(&hilo5,NULL,printChau,(void*)t);
if (rc){printf("ERROR creando hilo %ld , codigo %d \n",t,rc);
	exit(-1);}



pthread_exit(NULL);
return 0;
}

