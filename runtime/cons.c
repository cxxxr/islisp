#include "lisp.h"

void is_cons_f(int IS_UNUSED(argc))
{
	is_stack_drop_push(2,
			   is_make_cons(is_stack_peek_ptr(2),
					is_stack_peek_ptr(1)));
}

void is_car_f(int IS_UNUSED(argc))
{
	ISObject *obj = is_stack_peek_ptr(1);
	if (!IS_CONS_P(obj))
		is_type_error(*obj, IS_CONS_TYPE);
	is_stack_change_tos(IS_CONS_CAR(*obj));
}

void is_cdr_f(int IS_UNUSED(argc))
{
	ISObject *obj = is_stack_peek_ptr(1);
	if (!IS_CONS_P(obj))
		is_type_error(*obj, IS_CONS_TYPE);
	is_stack_change_tos(IS_CONS_CDR(*obj));
}

void is_cons_init(void)
{
	is_add_builtin_function("CONS", is_cons_f, 2, 2);
	is_add_builtin_function("CAR", is_car_f, 1, 1);
	is_add_builtin_function("CDR", is_cdr_f, 1, 1);
}
