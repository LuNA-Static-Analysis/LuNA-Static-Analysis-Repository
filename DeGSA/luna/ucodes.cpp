#include <cstdio>
#include "ucenv.h"

extern "C" {

        void printVal(int val){
                printf("%d\n", val);
        }

        void printstr(char* s) {
                        printf("%s\n", s);
        }

        void printfl(double d){
                        printf("%.14f\n", d);
        }

        void printnamedfl(char* s, double d){
                        printf("%s%.14f\n", s, d);
        }

        void printnamedint(char* s, int val){
                        printf("%s%d\n", s, val);
        }

        void printindval(int index, char* name, double value){
                        printf("%s[%d] = %.14f\n", name, index, value);
        }

        void printdoubleindval(int i, int j, char* name, double value){
                        printf("%s[%d][%d] = %f\n", name, i, j, value);
        }

        void printint(int val){
                printf("%d\n", val);
        }

        void c_init_int(int val, OutputDF &df){
                        df.setValue(double(val));
        }

        void c_init_float(double val, OutputDF &df){
                        df.setValue(val);
        }

        void c_init_intint(int val, OutputDF &df){
                        df.setValue(val);
        }

}