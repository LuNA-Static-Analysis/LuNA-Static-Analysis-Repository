#include <cstdio>
#include "ucenv.h"

extern "C" {
void c_init_int(int val, OutputDF &df){
    df.setValue(val);
}

void c_print_int(int val){
    printf("%d\n", val);
}
}


