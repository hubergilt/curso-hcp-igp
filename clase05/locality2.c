#include<stdio.h>
#include<stdlib.h>
#include<time.h>

int main()
{
int i,j,k;
int arr1[5][5];
srand(time(NULL));

for (j=0;j<5;j++)
 {
  for(i=0;i<5;i++)
   {
    arr1[i][j]=rand()%10;
    printf("%x \t",&arr1[i][j]);
   }
  printf("\n");
 }



return 0;
}
