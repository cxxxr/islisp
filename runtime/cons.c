#include "lisp.h"

void is_consp_f(int IS_UNUSED(argc))
{
	is_stack_change_tos(is_bool_to_object(IS_CONS_P(is_stack_peek(1))));
}

void is_cons_f(int IS_UNUSED(argc))
{
	is_stack_drop_push(2,
			   is_make_cons(is_stack_peek_ptr(2),
					is_stack_peek_ptr(1)));
}

void is_car_f(int IS_UNUSED(argc))
{
	ISObject obj = is_stack_peek(1);
	if (!IS_CONS_P(obj))
		is_type_error(obj, IS_CONS_TYPE);
	is_stack_change_tos(IS_CONS_CAR(obj));
}

void is_cdr_f(int IS_UNUSED(argc))
{
	ISObject obj = is_stack_peek(1);
	if (!IS_CONS_P(obj))
		is_type_error(obj, IS_CONS_TYPE);
	is_stack_change_tos(IS_CONS_CDR(obj));
}

void is_set_car(ISObject obj, ISObject cons)
{
	if (!IS_CONS_P(cons))
		is_type_error(cons, IS_CONS_TYPE);
	// write barrior
	IS_CONS_CAR(cons) = obj;
}

void is_set_cdr(ISObject obj, ISObject cons)
{
	if (!IS_CONS_P(cons))
		is_type_error(cons, IS_CONS_TYPE);
	// write barrior
	IS_CONS_CDR(cons) = obj;
}

void is_set_car_f(int IS_UNUSED(argc))
{
	ISObject obj = is_stack_peek(2);
	ISObject cons = is_stack_peek(1);
	is_set_car(obj, cons);
	is_stack_pop();
}

void is_set_cdr_f(int IS_UNUSED(argc))
{
	ISObject obj = is_stack_peek(2);
	ISObject cons = is_stack_peek(1);
	is_set_cdr(obj, cons);
	is_stack_pop();
}

void is_nreverse_f(int IS_UNUSED(argc))
{
	ISObject list = is_stack_peek(1);
	if (list == is_nil) return;
	if (!IS_CONS_P(list)) is_type_error(list, IS_LIST_TYPE);

	ISObject prev = is_nil;
	ISObject tail = list;
	ISObject next;

	while (tail != is_nil) {
		next = IS_CONS_CDR(tail);
		is_set_cdr(prev, tail);
		prev = tail;
		tail = next;
	}

	is_stack_change_tos(prev);
}

void is_cons_init(void)
{
	is_add_builtin_function("CONSP", is_consp_f, 1, 1);
	is_add_builtin_function("CONS", is_cons_f, 2, 2);
	is_add_builtin_function("CAR", is_car_f, 1, 1);
	is_add_builtin_function("CDR", is_cdr_f, 1, 1);
	is_add_builtin_function("SET-CAR", is_set_car_f, 2, 2);
	is_add_builtin_function("SET-CDR", is_set_cdr_f, 2, 2);
	is_add_builtin_function("NREVERSE", is_nreverse_f, 1, 1);
}
