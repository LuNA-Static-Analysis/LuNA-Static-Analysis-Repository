#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include "ucenv.h"

using namespace luna::ucenv;

extern "C" {
#pragma adapt user_type(x, vector)
    void c_init_f4(OutputDF &x, double v1, double v2, double v3, double v4) {
        auto data = x.create<double>(4);
        data[0] = v1;
        data[1] = v2;
        data[2] = v3;
        data[3] = v4;
    }

#pragma adapt user_type(x, matrix)
    void c_init_f4x4_identity(OutputDF &x) {
        auto data = x.create<double>(4*4);

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                if (i == j) {
                    data[4*i+j] = 1;
                } else {
                    data[4*i+j] = 0;
                }
            }
        }
    }

#pragma adapt user_type(x, matrix)
#pragma adapt user_type(y, matrix)
    void c_scale_f4x4(const InputDF &x, double factor, OutputDF &y) {
        auto m = x.getData<double>();
        auto result = y.create<double>(4*4);

        for (int i = 0; i < 4*4; i++) {
            result[i] = factor * m[i];
        }
    }

#pragma adapt user_type(x, matrix)
#pragma adapt user_type(y, vector)
#pragma adapt user_type(z, vector)
    void c_gemv_f4x4_f4(const InputDF &x, const InputDF &y, OutputDF &z) {
        auto m = x.getData<double>();
        auto v = y.getData<double>();
        auto result = z.create<double>(4);

        for (int i = 0; i < 4; i++) {
            result[i] = 0;
            for (int j = 0; j < 4; j++) {
                result[i] += m[4*i+j]*v[j];
            }
        }
    }

#pragma adapt user_type(x, vector)
    void c_print_f4(const InputDF &x) {
        auto data = x.getData<double>();

        printf("f4{%.2lf, %.2lf, %.2lf, %.2lf}\n", data[0], data[1], data[2], data[3]);
    }
}