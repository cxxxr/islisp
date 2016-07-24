#include "lisp.h"

void is_characterp_f(int IS_UNUSED(argc))
{
	is_stack_change_tos(is_bool_to_object(IS_CHARACTER_P(is_stack_peek(1))));
}

#define DEF_CMP_F(name, op) \
void name(int IS_UNUSED(argc)) \
{ \
	ISObject x = is_stack_peek(2); \
	ISObject y = is_stack_peek(1); \
	if (!IS_CHARACTER_P(x)) is_type_error(x, IS_CHARACTER_TYPE); \
	if (!IS_CHARACTER_P(y)) is_type_error(y, IS_CHARACTER_TYPE); \
	is_stack_change_tos(is_bool_to_object(IS_CHARACTER(x) op IS_CHARACTER(y))); \
}

DEF_CMP_F(is_char_eq_f, ==);
DEF_CMP_F(is_char_ne_f, !=);
DEF_CMP_F(is_char_lt_f, <);
DEF_CMP_F(is_char_gt_f, >);
DEF_CMP_F(is_char_le_f, <);
DEF_CMP_F(is_char_ge_f, >=);

void is_character_init(void)
{
	is_add_builtin_function("CHARACTERP", is_characterp_f, 1, 1);
	is_add_builtin_function("CHAR=", is_char_eq_f, 2, 2);
	is_add_builtin_function("CHAR/=", is_char_ne_f, 2, 2);
	is_add_builtin_function("CHAR<", is_char_lt_f, 2, 2);
	is_add_builtin_function("CHAR>", is_char_gt_f, 2, 2);
	is_add_builtin_function("CHAR<=", is_char_le_f, 2, 2);
	is_add_builtin_function("CHAR>=", is_char_ge_f, 2, 2);
}
