#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include "ucenv.h"

using namespace luna::ucenv;

double rand_zero_to_one() {
    return (double)rand() / (double)RAND_MAX;
}

struct f4 {
    double v1, v2, v3, v4;
};

#pragma adapt serialized_size(f4)
size_t f4_serialized_size(f4 x) {
    return sizeof(x.v1) + sizeof(x.v2) + sizeof(x.v3) + sizeof(x.v4);
}

#pragma adapt serialize(f4)
void f4_serialize(f4 x, void *buf) {
    auto d_buf = (double *) buf;
    *d_buf++ = x.v1;
    *d_buf++ = x.v2;
    *d_buf++ = x.v3;
    *d_buf++ = x.v4;
}

#pragma adapt deserialize(f4)
void f4_deserialize(f4 &x, const void *buf) {
    auto d_buf = (double *) buf;
    x.v1 = *d_buf++;
    x.v2 = *d_buf++;
    x.v3 = *d_buf++;
    x.v3 = *d_buf++;
}

extern "C" {
    void c_init_f4(OutputDF &xx, double v1, double v2, double v3, double v4) {
        auto x = f4{v1, v2, v3, v4};
        f4_serialize(x, xx.create(f4_serialized_size(x)));
    }

    void c_axpy_f4(double a, const InputDF &xx, const InputDF &yy, OutputDF &zz) {
        f4 x, y, z;

        f4_deserialize(x, xx.get_data());
        f4_deserialize(y, yy.get_data());

        c_init_f4(zz, a*x.v1+y.v1, a*x.v2+y.v2, a*x.v3+y.v3, a*x.v4+y.v4);
    }

    void c_print_f4(const InputDF &xx) {
        f4 x, y, z;

        f4_deserialize(x, xx.get_data());

        printf("f4{%.2lf, %.2lf, %.2lf, %.2lf}\n", x.v1, x.v2, x.v3, x.v4);
    }
}