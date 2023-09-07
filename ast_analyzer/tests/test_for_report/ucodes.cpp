#include <cstdio>
#include "ucenv.h"

extern "C" {

void c_init(int val, OutputDF &df) {
	df.setValue(val);
	printf("c_init: %d, size: %d\n", val, (int)df.getSize());
}

void c_iprint(int df) {
	printf("c_iprint %d\n", df);
}

void c_print(InputDF& df) {
	printf("c_print %d\n", df.getValue<int>());
}

}
