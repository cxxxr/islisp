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

void is_string_init(void)
{
	is_add_builtin_function("CREATE-STRING", is_create_string_f, 1, 2);
}
