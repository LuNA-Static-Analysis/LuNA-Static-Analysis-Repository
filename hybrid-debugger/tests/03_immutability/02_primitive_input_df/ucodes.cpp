#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include "ucenv.h"

using namespace luna::ucenv;

extern "C" {
    void c_int_set(OutputDF &x, int v) {
        x = v;
    }

    void c_int_print_as_value(const InputDF &x) {
        printf("%d\n", x.get_int());
    }
}