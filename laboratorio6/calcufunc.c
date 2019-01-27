/*
    Created on Sun Nov 11 19:43:10 2018
    @author: hubergilt@hotmail.com    
*/

#include<stdio.h>
#include<stdbool.h>
#include<math.h>
#include<stdlib.h>

float c_sumar_(float *val1, float *val2){
    return *val1+*val2;
}

float c_restar_(float *val1, float *val2){
    return *val1-*val2;
}

float c_multiplica_(float *val1, float *val2){
    return (*val1)*(*val2);
}

float c_divide_(float *val1, float *val2){
    return (*val1)/(*val2);
}

float c_potencia_(float *val1, float *val2){
    return powf(*val1, *val2);
}

float c_residuo_(float *val1, float *val2){
    return fmodf(*val1, *val2);
}

float c_modulo_(float *val1, float *val2){
    return sqrtf(powf(*val1, 2)+powf(*val2, 2));
}

float c_argumento_(float *val1, float *val2){
    return atan2f(*val1, *val2);
}

float c_ln_(float *val1){
    return logf(*val1);    
}

float c_ln10_(float *val1){
    return log10f(*val1);    
}

float c_abso_(float *val1){
    return fabsf(*val1);    
}

float c_seno_(float *val1){
    return sinf(*val1);    
}

float c_cose_(float *val1){
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

float c_fact_(float *val1){
    return factorial(val1);
}

float c_prim_(float *val1){
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

float c_perf_(float *val1){
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