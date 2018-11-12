/*
    Created on Sun Nov 11 19:43:10 2018
    @author: hubergilt@hotmail.com    
*/

#include<stdio.h>
#include<stdbool.h>
#include<math.h>
#include<stdlib.h>

#define SUMA   1
#define RESTA  2
#define MULTI  3
#define DIVI   4
#define POTE   5
#define RESI   6
#define MOD    7
#define ARG    8
#define LN     9
#define LOG   10
#define ABS   11
#define SEN   12
#define COS   13
#define FACT  14
#define PRIM  15
#define PERF  16
#define SALIR 17

int menu(){
    printf("\t*************************************************\n");
    printf("\t*\t  Bienvenidos a la calculadora    \t*\n");
    printf("\t*************************************************\n");
    printf("\t*  MENU:                             \t\t*\n");
    printf("\t*\t 1) suma           9) ln          \t*\n");
    printf("\t*\t 2) resta         10) log         \t*\n");
    printf("\t*\t 3) multiplicion  11) abs         \t*\n");
    printf("\t*\t 4) division      12) sen         \t*\n");
    printf("\t*\t 5) potencia      13) cos         \t*\n");
    printf("\t*\t 6) residuo       14) factorial   \t*\n");
    printf("\t*\t 7) modulo        15) primo       \t*\n");
    printf("\t*\t 8) argumento     16) perfecto    \t*\n");
    printf("\t*\t                  17) SALIR       \t*\n");
    printf("\t*************************************************\n");
    return 0;    
}

int get_oper(){
    int option=SALIR;
    printf("\n");
    printf("Ingrese una operacion valida [1-16] >> ");
    scanf("%d", &option);
    return option;
}

float sumar(float *val1, float *val2){
    printf("Operacion SUMAR, ingrese : valor1+valor2 >> ");
    scanf("%f+%f", val1, val2);
    return *val1+*val2;
}

float restar(float *val1, float *val2){
    printf("Operacion RESTAR, ingrese : valor1-valor2 >> ");
    scanf("%f-%f", val1, val2);
    return *val1-*val2;
}

float multiplica(float *val1, float *val2){
    printf("Operacion MULTIPLICA, ingrese : valor1*valor2 >> ");
    scanf("%f*%f", val1, val2);
    return (*val1)*(*val2);
}

float divide(float *val1, float *val2){
    printf("Operacion DIVIDE, ingrese : valor1/valor2 >> ");
    scanf("%f/%f", val1, val2);
    return (*val1)/(*val2);
}

float potencia(float *val1, float *val2){
    printf("Operacion POTENCIA, ingrese : valor1^valor2 >> ");
    scanf("%f^%f", val1, val2);
    return powf(*val1, *val2);
}

float residuo(float *val1, float *val2){
    printf("Operacion RESIDUO, ingrese : valor1//valor2 >> ");
    scanf("%f//%f", val1, val2);
    return fmodf(*val1, *val2);
}

float modulo(float *val1, float *val2){
    printf("Operacion MODULO, ingrese : valor1|valor2 >> ");
    scanf("%f|%f", val1, val2);
    return sqrtf(powf(*val1, 2)+powf(*val2, 2));
}

float argumento(float *val1, float *val2){
    printf("Operacion ARGUMENTO, ingrese : valor1?valor2 >> ");
    scanf("%f?%f", val1, val2);
    return atan2f(*val1, *val2);
}

float ln(float *val1){
    printf("Operacion LOG NATURAL, ingrese : valor1 >> ");
    scanf("%f", val1);
    return logf(*val1);    
}

float ln10(float *val1){
    printf("Operacion LOG BASE 10, ingrese : valor1 >> ");
    scanf("%f", val1);
    return log10f(*val1);    
}

float abso(float *val1){
    printf("Operacion ABS, ingrese : valor1 >> ");
    scanf("%f", val1);
    return fabsf(*val1);    
}

float seno(float *val1){
    printf("Operacion SIN, ingrese : valor1 >> ");
    scanf("%f", val1);
    return sinf(*val1);    
}

float cose(float *val1){
    printf("Operacion COS, ingrese : valor1 >> ");
    scanf("%f", val1);
    return cosf(*val1);    
}

float factorial(float *val1){
    float facto=0;
    if(*val1==0){
        facto = 1;
    }
    else{
        facto = (*val1)-1;
        facto = (*val1)*(factorial(&facto)); 
    }
    return facto;   
}

float fact(float *val1){
    printf("Operacion FACT, ingrese : valor1 >> ");
    scanf("%f", val1);
    return factorial(val1);
}

float prim(float *val1){
    printf("Operacion PRIMO, ingrese : valor1 >> ");
    scanf("%f", val1);
    float esprimo=0, i=1;
    int cuenta=0;
    if(*val1==0 || *val1==1){
        esprimo=1;
    }else{
        while(i<=(*val1)){
            if(fmodf(*val1,i)==0){
                cuenta++;
            }
            i++;
        }
        if(cuenta==2){
            esprimo=1;
        }
    }
    return esprimo;    
}

float perf(float *val1){
    printf("Operacion NUM PERFECTO, ingrese : valor1 >> ");
    scanf("%f", val1);
    float esperfecto=0, i=1, suma=0;
    float *divisores;
    int cuenta = 0;

    divisores = (float *)malloc(100*sizeof(float)); 

    while(i<*val1){
        if(fmodf(*val1,i)==0){
            *(divisores+cuenta) = i;
            cuenta++;
        }
        i++;
    }

    while(cuenta>-1){
        suma+=*(divisores+cuenta);
        cuenta--;
    }

    if(suma==*val1){
        esperfecto=1;
    }

    free(divisores);
    return esperfecto;    
}

float execute(int *oper){
    float val1=0, val2=0;
    switch(*oper){
        case SUMA:
            return sumar(&val1, &val2);
        case RESTA:
            return restar(&val1, &val2);
        case MULTI:
            return multiplica(&val1, &val2);
        case DIVI:
            return divide(&val1, &val2);
        case POTE:
            return potencia(&val1, &val2);
        case RESI:
            return residuo(&val1, &val2);
        case MOD:
            return modulo(&val1, &val2);
        case ARG:
            return argumento(&val1, &val2);
        case LN:
            return ln(&val1);
        case LOG:
            return ln10(&val1);
        case ABS:
            return abso(&val1);
        case SEN:
            return seno(&val1);
        case COS:
            return cose(&val1);
        case FACT:
            return fact(&val1);
        case PRIM:
            return prim(&val1);
        case PERF:
            return perf(&val1);                          
        default:
            return 0;
    }
    return 0;
}

int main(){
    int oper=SALIR;
    float resultado=0;
    while(true){
        menu();
        oper = get_oper();
        printf("La operacion seleccionada es >> %d\n", oper);
        if(oper==SALIR){
            break;
        }
        if(oper>0 && oper<SALIR){
            resultado=execute(&oper);
            if(oper==PRIM){
                if(resultado==1){
                    printf("Resultado de la operacion es >> PRIMO\n\n");
                }else{
                    printf("Resultado de la operacion es >> NO PRIMO\n\n");
                }
            }if(oper==PERF){
                if(resultado==1){
                    printf("Resultado de la operacion es >> PERFECTO\n\n");
                }else{
                    printf("Resultado de la operacion es >> NO PERFECTO\n\n");
                }
            }
            else{
                printf("Resultado de la operacion es >> %f\n\n", resultado);                
            }
        }
    }
    return 0;
}
