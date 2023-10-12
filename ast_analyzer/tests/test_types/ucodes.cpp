#include <cstdio>
#include "ucenv.h"

extern "C" {

void c_init_int(int val, OutputDF &df) {
	df.setValue(val);
	printf("c_init: %d, size: %d\n", val, (int)df.getSize());
}

void c_init_real(double val, OutputDF &df) {
	df.setValue(val);
	printf("c_init: %f, size: %d\n", val, (int)df.getSize());
}


void c_init_string(char* val, OutputDF &df) {
	df.setValue(val);
	printf("c_init: %s \n", val);
}

void c_int_to_real(double df) {
	printf("c_int_to_real %f\n", df);
}

void c_real_to_int(int df) {
	printf("c_real_to_int %d\n", df);
}

void c_int_to_value(InputDF& df) {
	printf("c_int_to_value %d\n", df.getValue<int>());
}

void c_real_to_value(InputDF& df) {
	printf("c_real_to_value %f\n", df.getValue<double>());
}

void c_string_to_value(InputDF& df) {
	printf("c_string_to_value %s\n", df.getValue<char*>());
}

void c_real_to_string(char* df) {
	printf("c_real_to_string %s\n", df);
}

void c_value_to_int(int df) {
	printf("c_value_to_int %d\n", df);
}

void c_value_to_real(double df) {
	printf("c_value_to_real %f\n", df);
}

void c_value_to_string(char* df) {
	printf("c_value_to_string %s\n", df);
}

void c_int_to_string(char* df) {
	printf("c_int_to_string %s\n", df);
}

void c_string_to_real(double df) {
	printf("c_string_to_real %f\n", df);
}

void c_string_to_int(int df) {
	// printf("c_string_to_int %d\n", df);
}


void c_iprint(int df) {
	printf("c_iprint %d\n", df);
}

void c_print(InputDF& df) {
	printf("c_print %d\n", df.getValue<int>());
}

}

