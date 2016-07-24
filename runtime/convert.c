#include "lisp.h"

void is_convert_char_to_integer_f(int IS_UNUSED(argc))
{
	ISObject obj = is_stack_peek(1);
	if (!IS_CHARACTER_P(obj))
		is_type_error(obj, IS_CHARACTER_TYPE);
	is_stack_change_tos(is_make_integer(IS_CHARACTER(obj)));
}

void is_convert_string_to_symbol_f(int IS_UNUSED(argc))
{
	ISObject obj = is_stack_peek(1);
	if (!IS_STRING_P(obj))
		is_type_error(obj, IS_STRING_TYPE);
	is_stack_change_tos(IS_SYMBOL_NAME(obj));
}

void is_convert_init(void)
{
	is_add_builtin_function("IS:CONVERT-CHAR-TO-INTEGER", is_convert_char_to_integer_f, 1, 1);
	is_add_builtin_function("IS:CONVERT-STRING-TO-SYMBOL", is_convert_string_to_symbol_f, 1, 1);
}
