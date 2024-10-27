#include <stdlib.h>
#include <unistd.h>
#include <ucenv.h>


using namespace luna::ucenv;


extern "C" {
    void c_print(int v) { 
        printf("%d\n", v);
    }

    void c_init(OutputDF &result, int v) {
        result = v; 
    }
}
