#include "lisp.h"

void is_create_vector_2f(void)
{
	ISObject *num = is_stack_peek_ptr(2);
	ISObject *v = is_stack_peek_ptr(1);

	int len;
	if (IS_INTEGER_P(num)) {
		len = IS_INTEGER(num);
	} else if (IS_FLOAT_P(num)) {
		len = IS_FLOAT_VALUE(num);
	} else {
		is_type_error(*num, IS_NUMBER_TYPE);
	}
	is_stack_push(is_make_vector(len, v));
}

void is_create_vector_f(int argc)
{
	if (argc == 1)
		is_stack_push(is_nil);
	is_create_vector_2f();
}

void is_vector_init(void)
{
	is_add_builtin_function("CREATE-VECTOR", is_create_vector_f, 1, 2);
}
