#include "lisp.h"

void is_convert_char_to_integer_f(int IS_UNUSED(argc))
{
	ISObject obj = is_stack_peek(1);
	if (!IS_CHARACTER_P(obj))
		is_type_error(obj, IS_CHARACTER_TYPE);
	is_stack_change_tos(is_make_integer(IS_CHARACTER(obj)));
}

void is_convert_integer_to_char_f(int IS_UNUSED(argc))
{
	ISObject obj = is_stack_peek(1);
	if (!IS_INTEGER_P(obj))
		is_type_error(obj, IS_INTEGER_TYPE);
	is_stack_change_tos(is_make_character(IS_INTEGER(obj)));
}

void is_convert_string_to_symbol_f(int IS_UNUSED(argc))
{
	ISObject *string = is_stack_peek_ptr(1);
	if (!IS_STRING_P(*string))
		is_type_error(*string, IS_STRING_TYPE);
	is_stack_change_tos(is_intern(string));
}

void is_convert_init(void)
{
	is_add_builtin_function("IS:CONVERT-CHAR-TO-INTEGER", is_convert_char_to_integer_f, 1, 1);
	is_add_builtin_function("IS:CONVERT-INTEGER-TO-CHAR", is_convert_integer_to_char_f, 1, 1);
	is_add_builtin_function("IS:CONVERT-STRING-TO-SYMBOL", is_convert_string_to_symbol_f, 1, 1);
}
