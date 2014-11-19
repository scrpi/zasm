


const int const_int_e = 123;
int global_int_a;
int global_int_b = 99;
static int static_int_c;
static int static_int_d = 66;


#include <stdlib.h>

int mult(int a, int b)
{
	return a * b;
}

static int div(int a, int b)
{
	return a / b;
}

extern int rem(int a, int b);

void main()
{
	int a;
	char b;
	global_int_a = 33;
	static_int_c = mult(global_int_a,static_int_d);
	static_int_d = rem(global_int_a,static_int_c);

	global_int_a = rand();

	a = global_int_a;
	if(a==static_int_c) { a *= static_int_c; }

	b = static_int_d / global_int_b;
	if(a==b) a *= (unsigned char)a/(unsigned char)b;

	global_int_a = a/b;
}



