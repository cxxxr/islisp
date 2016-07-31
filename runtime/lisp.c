#define _IS_MAIN_

#include "lisp.h"

void is_init(void)
{
	is_gc_disable();
	is_heap_init();
	is_shelter_init();
	is_symbol_init();
	is_stack_init();
	is_dynamic_init();
	is_character_init();
	is_number_init();
	is_stream_init();
	is_cons_init();
	is_vector_init();
	is_string_init();
	is_convert_init();
	is_call_init();
	is_env_init();
	is_print_init();
	is_error_init();
}
