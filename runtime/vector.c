#include "lisp.h"

void is_create_vector_2f(void)
{
	ISObject len = is_stack_peek(2);
	ISObject *v = is_stack_peek_ptr(1);
	if (!IS_INTEGER_P(len)) {
		is_type_error(len, IS_NUMBER_TYPE);
	}
	is_stack_drop_push(2, is_make_vector(IS_INTEGER(len), v));
}

void is_create_vector_f(int argc)
{
	if (argc == 1)
		is_stack_push(is_nil);
	is_create_vector_2f();
}

void is_vector_ref_f(int IS_UNUSED(argc))
{
	ISObject vec = is_stack_peek(2);
	ISObject index = is_stack_peek(1);

	if (!IS_VECTOR_P(vec)) is_type_error(vec, IS_VECTOR_TYPE);
	if (!IS_INTEGER_P(index)) is_type_error(index, IS_INTEGER_TYPE);

	int i = IS_INTEGER(index);

	if (i < 0 || IS_VECTOR_LENGTH(vec) <= i)
		is_index_out_of_range(vec, index);

	is_stack_drop_push(2, IS_VECTOR_DATA(vec)[i]);
}

void is_vector_set_f(int IS_UNUSED(argc))
{
	ISObject val = is_stack_peek(3);
	ISObject vec = is_stack_peek(2);
	ISObject index = is_stack_peek(1);

	if (!IS_VECTOR_P(vec)) is_type_error(vec, IS_VECTOR_TYPE);
	if (!IS_INTEGER_P(index)) is_type_error(index, IS_INTEGER_TYPE);

	int i = IS_INTEGER(index);

	if (i < 0 || IS_VECTOR_LENGTH(vec) <= i)
		is_index_out_of_range(vec, index);

	// write_barrior
	IS_VECTOR_DATA(vec)[i] = val;
	is_stack_drop(2);
}

void is_vector_init(void)
{
	is_add_builtin_function("CREATE-VECTOR", is_create_vector_f, 1, 2);
	is_add_builtin_function("IS:VECTOR-REF", is_vector_ref_f, 2, 2);
	is_add_builtin_function("IS:VECTOR-SET", is_vector_set_f, 3, 3);
}
