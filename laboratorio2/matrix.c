#include<stdio.h>
#include<stdlib.h>

int i=0, j=0, k=0;

typedef struct matrix_s {
	int rows;
	int cols;
	double **data;
} matrix;

matrix *matrix_alloc(int r, int c){
  matrix *ptr=NULL;
  if (r>0 && c>0) {
    matrix m;
    ptr = &m;
    printf("%x\n", ptr);
    ptr->rows=r;
    ptr->cols=c;
  printf("%d\n", ptr->rows);
  printf("%d\n", ptr->cols);
    ptr->data=calloc(ptr->rows, sizeof(double));
    for(i=0; i<ptr->rows; i++)
      ptr->data[i]=calloc(ptr->cols, sizeof(double));
  }
  return ptr;
}

void matrix_init(matrix *ptr){
  printf("%x\n", ptr);
  printf("%d\n", ptr->rows);
  printf("%d\n", ptr->cols);
  if (ptr->rows>0 &&  ptr->cols>0)
  	for(i=0; i<ptr->rows; i++)
  		for(j=0; j<ptr->cols; j++)
  			ptr->data[i][j]=rand()%10+(double)(rand()%10)/10+(double)(rand()%100);
}

void matrix_printf(matrix *m){
	for(i=0; i<m->rows; i++){
		for(j=0; j<m->cols; j++)
			printf("[%d][%d]=%5.2lf ", i, j, m->data[i][j]);
    printf("\n");
  }
  printf("\n");
}


int main(){
  matrix *a = matrix_alloc(1, 2);
  matrix *b = a;
  printf("%d\n", b->rows);
  printf("%d\n", b->cols);

  matrix_init(b);  

  return 0;
}