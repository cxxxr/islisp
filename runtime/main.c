#define _IS_MAIN_

#include "lisp.h"
#include "OUTPUT.C"

int main(void)
{
	is_gc_disable();
	is_heap_init();
	is_stack_init();
	is_symbol_init();
	is_number_init();
	is_stream_init();
	is_cons_init();
	is_vector_init();
	is_call_init();
	is_shelter_init();
	is_env_init();
	is_print_init();

	loader();

	return 0;
}
