#include "lisp.h"

void is_create_string_f(int argc)
{
	ISObject len;
	char c;
	if (argc == 1) {
		len = is_stack_peek(1);
		c = ' ';
	} else {
		len = is_stack_peek(2);
		ISObject charobj = is_stack_peek(1);
		if (!IS_CHARACTER_P(charobj))
			is_type_error(charobj, IS_CHARACTER_TYPE);
		c = IS_CHARACTER(charobj);
	}

	if (!IS_INTEGER_P(len))
		is_type_error(len, IS_INTEGER_TYPE);

	is_stack_drop_push(argc, is_make_string_fill(IS_INTEGER(len), c));
}

void is_string_ref_f(int IS_UNUSED(argc))
{
	ISObject string = is_stack_peek(2);
	ISObject index = is_stack_peek(1);

	if (!IS_STRING_P(string)) is_type_error(string, IS_STRING_TYPE);
	if (!IS_INTEGER_P(index)) is_type_error(index, IS_INTEGER_TYPE);

	int i = IS_INTEGER(index);

	if (i < 0 || IS_STRING_LENGTH(string) <= i)
		is_index_out_of_range(string, index);

	is_stack_drop_push(2, is_make_character(IS_STRING_DATA(string)[i]));
}

void is_string_set_f(int IS_UNUSED(argc))
{
	ISObject character = is_stack_peek(3);
	ISObject string = is_stack_peek(2);
	ISObject index = is_stack_peek(1);

	if (!IS_CHARACTER_P(character)) is_type_error(character, IS_CHARACTER_TYPE);
	if (!IS_STRING_P(string)) is_type_error(string, IS_STRING_TYPE);
	if (!IS_INTEGER_P(index)) is_type_error(index, IS_INTEGER_TYPE);

	int i = IS_INTEGER(index);

	if (i < 0 || IS_STRING_LENGTH(string) <= i)
		is_index_out_of_range(string, index);

	IS_STRING_DATA(string)[i] = IS_CHARACTER(character);
	is_stack_drop(2);
}

void is_string_length_f(int IS_UNUSED(argc))
{
	ISObject string = is_stack_peek(1);
	if (!IS_STRING_P(string)) is_type_error(string, IS_STRING_TYPE);
	is_stack_change_tos(is_make_integer(IS_STRING_LENGTH(string)));
}

void is_string_init(void)
{
	is_add_builtin_function("CREATE-STRING", is_create_string_f, 1, 2);
	is_add_builtin_function("IS:STRING-REF", is_string_ref_f, 2, 2);
	is_add_builtin_function("IS:STRING-SET", is_string_set_f, 3, 3);
	is_add_builtin_function("IS:STRING-LENGTH", is_string_length_f, 1, 1);
}
