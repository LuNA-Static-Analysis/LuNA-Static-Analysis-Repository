#include <stdlib.h>
#include <unistd.h>
#include <ucenv.h>


using namespace luna::ucenv;


extern "C" {
    void c_print(int v) { 
        printf("%lf\n", v); 
    }

    void c_init(OutputDF &result, int v) { 
        if (v == 0) {
            // pass
        } else {
            sleep(1);
        }
        result = v; 
    }
}
