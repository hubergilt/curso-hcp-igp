/*
    Created on Sat Nov 17 08:31:10 2018
    @author: hubergilt@hotmail.com    
*/

#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>
#include<string.h>
#include<sys/time.h>
#include<pthread.h>

#define MA_NFIL 4
#define MA_NCOL 4
#define MB_NFIL 4
#define MB_NCOL 4

#define NUM_HILOS 2

#define ERR_MALLOC1 "Error en la asignacion de memoria para %s matrix con %d filas\n"
#define ERR_MALLOC2 "Error en la asignacion de memoria para %s matrix en %d fila y %d columna\n"
#define ERR_OPT1	"Error en opcion requerida '-%c' es desconocida\n"
#define ERR_OPT2	"Error en opcion requerida '-%c' necesita un valor valido\n"
#define ERR_OPT3	"Error en opcion requerida '-%c' con valor %s es invalida\n"
#define ERR_OPT4	"Error en opcion requerida '-h' con valor %s es invalida\n"
#define ERR_DIM		"Error en dimension, el numero de columnas de la primera matrix (%d) es diferente al numero de filas de las segunda fila %d invalida\n"
#define MEN_USO		"\nUso del comando : %s [-m] [-f <numpy>] [-o <i,j,k>] [-r] [-h <num-hilos>]\n\tPor defecto la asignacion de valores es aleatoria\n\tm : Para, ingresar valores de forma (m)anual\n\tf <numpy> : Para, imprimir en (f)ormato numpy para matrix\n\to <i,j,k> : Para, asignar el (o)rden de las matrices a [i][j] y [j][k]\n\tr : Para, mostrar informacion (r)esumida\n\th <num-hilos> : Para h>1, asignar un numero de (h)ilos de ejecución\n"

#define	MEN_TITLE   "\tMultiplicacion de Matrices Sequencial\n"
#define	MEN_PROMT1  "Ingrese orden de la %s matrix: nfilas,ncolumnas >> "
#define	MEN_PROMT2  "El orden seleccionado para la %s matrix es >> [%d x %d] \n"
#define MEN_PROMT3	"Matrix, Ingrese valor en la posicion [%d][%d] >> \n"
#define MEN_PROMT4	"El resultado de la Multiplicacion de matrices es >> [%d x %d] \n"

double **ma={0}, **mb={0}, **mr={0};
int ma_nfil=MA_NFIL;
int ma_ncol=MA_NCOL;
int mb_nfil=MB_NFIL;
int mb_ncol=MB_NCOL;

int num_hilos=NUM_HILOS;

int i=0, j=0, k=0;

void printf_matrix(){
	printf(MEN_PROMT2, "PRIMERA", ma_nfil, ma_ncol);
	for (i=0; i<ma_nfil; i++)
	{
		for(j=0; j<ma_ncol; j++)
		{
			printf("[%d][%d]=%.2lf\t",i,j,ma[i][j]);
		}
		printf("\n");
	}
	printf(MEN_PROMT2, "SEGUNDA", mb_nfil, mb_ncol);
	for (i=0; i<mb_nfil; i++)
	{
		for(j=0; j<mb_ncol; j++)
		{
			printf("[%d][%d]=%.2lf\t",i,j,mb[i][j]);
		}
		printf("\n");
	}
	printf(MEN_PROMT4, ma_nfil, mb_ncol);
	for (i=0; i<ma_nfil; i++)
	{
		for(j=0; j<mb_ncol; j++)
		{
			printf("[%d][%d]=%.2lf\t",i,j,mr[i][j]);
		}
		printf("\n");
	}
}

void numpy_matrix(){
	printf(MEN_PROMT2, "PRIMERA", ma_nfil, ma_ncol);
	printf("[");
	for (i=0; i<ma_nfil; i++)
	{
		printf("[");
		for(j=0; j<ma_ncol; j++)
		{
			printf("%.2lf,",ma[i][j]);
		}
		printf("],\n");
	}
	printf("],\n");

	printf(MEN_PROMT2, "SEGUNDA", mb_nfil, mb_ncol);
	printf("[");
	for (i=0; i<mb_nfil; i++)
	{
		printf("[");
		for(j=0; j<mb_ncol; j++)
		{
			printf("%.2lf,", mb[i][j]);
		}
		printf("],\n");
	}
	printf("],\n");

	printf(MEN_PROMT4, ma_nfil, mb_ncol);
	for (i=0; i<ma_nfil; i++)
	{
		for(j=0; j<mb_ncol; j++)
		{
			printf("[%d][%d]=%.2lf\t",i,j,mr[i][j]);
		}
		printf("\n");
	}
}

void scanf_matrix(){
	printf(MEN_PROMT2, "PRIMERA", ma_nfil, ma_ncol);
	for (i=0; i<ma_nfil; i++)
	{
		for(j=0; j<ma_ncol; j++)
		{
			printf(MEN_PROMT3, i, j);
	    	scanf("%lf", &ma[i][j]);
		}
	}
	printf(MEN_PROMT2, "SEGUNDA", mb_nfil, mb_ncol);
	for (i=0; i<mb_nfil; i++)
	{
		for(j=0; j<mb_ncol; j++)
		{
			printf(MEN_PROMT3, i, j);
	    	scanf("%lf", &mb[i][j]);
		}
	}
}

void malloc_matrix(){
	ma = (double **)calloc(ma_nfil, sizeof(double *));

	if(ma == NULL){
		fprintf(stderr, ERR_MALLOC1, "PRIMERA", ma_nfil);
	}else{
		for (i=0;i<ma_nfil;i++){
			ma[i] = (double *)calloc(ma_ncol, sizeof(double));
			if(ma[i] == NULL){
				fprintf(stderr, ERR_MALLOC2, "PRIMERA", ma_nfil, ma_ncol);
			}
		}		
	}

	mb = (double **)calloc(mb_nfil, sizeof(double *));
	
	if(mb == NULL){
		fprintf(stderr, ERR_MALLOC1, "SEGUNDA", mb_nfil);
	}else{
		for (i=0;i<mb_nfil;i++){
			mb[i] = (double *)calloc(mb_ncol, sizeof(double));
			if(mb[i] == NULL){
				fprintf(stderr, ERR_MALLOC2, "SEGUNDA", mb_nfil, mb_ncol);
			}
		}		
	}

	mr = (double **)calloc(ma_nfil, sizeof(double *));
	
	if(mr == NULL){
		fprintf(stderr, ERR_MALLOC1, "RESULTADO", ma_nfil);
	}else{
		for (i=0;i<ma_nfil;i++){
			mr[i] = (double *)calloc(mb_ncol, sizeof(double));
			if(mr[i] == NULL){
				fprintf(stderr, ERR_MALLOC2, "RESULTADO", ma_nfil, mb_ncol);
			}
		}
	}
}

void free_matrix(){
	free(ma);
	free(mb);
	free(mr);
}

void rand_matrix(){
	for (i=0; i<ma_nfil; i++)
	{
		for(j=0; j<ma_ncol; j++)
		{
	    	ma[i][j]=(double) (rand()%10);
		}
	}
	for (i=0; i<mb_nfil; i++)
	{
		for(j=0; j<mb_ncol; j++)
		{
	    	mb[i][j]=(double) (rand()%10);
		}
	}
	for (i=0; i<ma_nfil; i++)
	{
		for(j=0; j<mb_ncol; j++)
		{
			mr[i][j]= 0;
		}
	}
}

struct datos
{
    int id;
    int filas;
    int filcols;
    int columnas;
};

void *product_matrix(void *parametros){

    int i=0, j=0, k=0, lim_inf=0, lim_sup=0, id=0, filas=0, filcols=0, columnas=0;

    struct datos *mis_datos;
    mis_datos=(struct datos*)parametros;
    id      =mis_datos->id;
    filas   =mis_datos->filas;
    filcols =mis_datos->filcols;
    columnas=mis_datos->columnas;

    lim_inf=id*(filas/num_hilos);
    lim_sup=(id+1)*(filas/num_hilos);

    printf("Hilo %d, LIM inf %d LIM sup %d \n", id, lim_inf, lim_sup);
    
	for (i=lim_inf; i<lim_sup; i++)
	{
		for(j=0; j<mb_ncol; j++)
		{
			for(k=0; k<ma_ncol; k++)
			{
				mr[i][j] += ma[i][k]*mb[k][j];
			}
		}
	}
	
    pthread_exit((void*)id);    	
}

int main(int argc, char *argv[])
{
	char c;
	int mflag = 0, errflag = 0, oflag = 0, rflag = 0, hflag = 0;
	char * format={0}, * order={0}, *shilos={0};
	while((c=getopt(argc, argv, ":mf:o:rh:"))!=-1)
	{
		switch(c){
			case 'm':
				if(mflag){
					errflag++;
				}else{
					mflag++;
				}
				break;
			case 'f':
				format=optarg;
				break;
			case 'o':
				order=optarg;
				oflag++;
				break;
			case 'r':
				if(rflag){
					errflag++;
				}else{
					rflag++;
				}
				break;
			case 'h':
				shilos=optarg;
				hflag++;
				break;				
			case ':':
				fprintf(stderr, ERR_OPT2, optopt);
				errflag++;
				break;
			case '?':
				if(optopt=='?' || optopt=='o' || optopt=='h'){
					fprintf(stderr, MEN_USO	, argv[0]);					
					return 1;
				}else{
					fprintf(stderr, ERR_OPT1, optopt);
					errflag++;
					break;					
				}
			default:
				errflag++;
				abort();
		}
	}

	if(errflag){
		fprintf(stderr, MEN_USO	, argv[0]);
		return 1;
	}

	if(format!=NULL && strcmp(format,"numpy") != 0){
		fprintf(stderr, ERR_OPT3, 'f', format);
		return 2;
	}

	if(oflag>0 && sscanf(order, "%d,%d,%d", &ma_nfil, &ma_ncol, &mb_ncol)!=3){
		fprintf(stderr, ERR_OPT3, 'o', order);
		return 2;
	}

	if(hflag>0 && sscanf(shilos, "%d", &num_hilos)!=1 ){
        if(num_hilos>1)
		printf("num_hilos %d\n", num_hilos);
		fprintf(stderr, ERR_OPT3, 'h',  shilos);
		return 2;
	}
	
	if(hflag>0 && num_hilos<2){
		fprintf(stderr, ERR_OPT4, shilos);
		return 2;
	}	
	
	printf(MEN_TITLE);

	if(mflag){
		printf(MEN_PROMT1, "PRIMERA");
		scanf("%d,%d", &ma_nfil, &ma_ncol);
		printf(MEN_PROMT2, "PRIMERA", ma_nfil, ma_ncol);

		printf(MEN_PROMT1, "SEGUNDA");
		scanf("%d,%d", &mb_nfil, &mb_ncol);
		printf(MEN_PROMT2, "SEGUNDA", mb_nfil, mb_ncol);

		if(ma_ncol!=mb_nfil){
			fprintf(stderr, ERR_DIM, ma_ncol, mb_nfil);
			abort();
		}
	}
	
	if(oflag){
		mb_nfil=ma_ncol;
	}else{
		ma_nfil=MA_NFIL;
		ma_ncol=MA_NCOL;
		mb_nfil=MB_NFIL;
		mb_ncol=MB_NCOL;
	}

	printf("\n");
	malloc_matrix();

	if(mflag){
		scanf_matrix();
	}else{
		rand_matrix();
	}
	printf("\n");
	
	
    struct datos *mis_datos=(struct datos*)parametros;    
    struct datos datos_pasar[num_hilos];
    int t=0, rc=0;	
	
    void *status;    	
    pthread_t hilos[num_hilos]; 
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_JOINABLE);

	struct timeval ini, fin;
	
	gettimeofday(&ini, NULL);
	//product_matrix();
	for (t=0;t<NUM_HILOS;t++)
    {
        datos_pasar[t].id=t;
        datos_pasar[t].filas=ma_nfil;
        datos_pasar[t].filcols=ma_ncol;
        datos_pasar[t].columnas=mb_ncol;
        
        rc=pthread_create(&arreglo_hilos[t],&attr,product_matrix,(void*)&datos_pasar[t]);
        printf("Creando hilo %ld \n",t);
        if (rc){
            printf("ERROR al crear el hilo %ld codigo %d \n",t,rc);
        	exit(-1);
        }
    
    }
    
    pthread_attr_destroy(&attr);

    for (t=0;t<NUM_HILOS;t++){
        rc=pthread_join(arreglo_hilos[t],&status);
        if (rc){
            printf("ERROR,codigo %d \n",rc);
            exit(-1);
        }
        printf("En la funcion main, ya acabo el hilo %ld con status %ld \n",t,(long)status);
    }            
	gettimeofday(&fin, NULL);
	
	double tiempo=0;
	tiempo = (fin.tv_sec-ini.tv_sec)+(fin.tv_usec-ini.tv_usec)/1000000.0;
	printf("Multiplicacion de matrices terminada en    >> %.6lf \tsegundos \n", tiempo);

    long nb=0;
    nb=(ma_nfil*ma_ncol+mb_nfil*mb_ncol+ma_nfil*mb_ncol)*sizeof(double);
	printf("Tamaño del problema, suma memoria asignada >> %ld \tbytes \n", nb);

	printf("\n");

	if(!rflag){
		if(format!=NULL){
			numpy_matrix();
		}else{
			printf_matrix();
		}
	}
	


	free_matrix();
	printf("\n");

	return 0;
}
