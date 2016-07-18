#include "lisp.h"

static double to_double(ISObject v)
{
	if (IS_INTEGER_P(v))
		return (double) (IS_INTEGER(v));
	if (IS_FLOAT_P(v))
		return IS_FLOAT_VALUE(v);
	is_type_error(v, IS_NUMBER_TYPE);
	return 0.0;
}

#define DEF_ADD_2F(name, op)\
void name(void)\
{\
	ISObject *a = is_stack_peek_ptr(2);\
	ISObject *b = is_stack_peek_ptr(1);\
	if (IS_INTEGER_P(*a) && IS_INTEGER_P(*b)) {\
		is_stack_drop_push(2, is_make_integer(IS_INTEGER(*a) op IS_INTEGER(*b)));\
	} else {\
		is_stack_drop_push(2, is_make_float(to_double(*a) op to_double(*b)));\
	}\
}

DEF_ADD_2F(is_add_2f, +);
DEF_ADD_2F(is_mul_2f, *);

#define DEF_ADD_F(name, init_val, fun)\
void name(int argc)\
{\
	switch (argc) {\
		case 0:\
			is_stack_push(is_make_integer(init_val));\
			break;\
		case 1:\
			break;\
		case 2:\
			is_add_2f();\
			break;\
		default:\
			while (--argc > 0) fun();\
			break;\
	}\
}

DEF_ADD_F(is_add_f, 0, is_add_2f);
DEF_ADD_F(is_mul_f, 1, is_mul_2f);

static ISObject sub2(ISObject *a, ISObject *b)
{
	if (IS_INTEGER_P(*a) && IS_INTEGER_P(*b)) {
		int v = IS_INTEGER(*a) - IS_INTEGER(*b);
		return is_make_integer(v);
	} else {
		return is_make_float(IS_INTEGER(*a) - IS_INTEGER(*b));
	}
}

void is_sub_2f(void)
{
	is_stack_drop_push(2,
			   sub2(is_stack_peek_ptr(2),
				is_stack_peek_ptr(1)));
}

static ISObject minus(ISObject *v)
{
	if (IS_INTEGER_P(*v))
		return is_make_integer(-IS_INTEGER(*v));
	else
		return is_make_float(-to_double(*v));
}

void is_sub_f(int argc)
{
	switch (argc) {
	case 1:
		is_stack_change_tos(minus(is_stack_peek_ptr(1)));
		break;
	case 2:
		is_sub_2f();
		break;
	default:
		{
			ISObject *v = is_stack_peek_ptr(argc);
			int n = argc;
			while (--n > 0)
				*v = sub2(v, is_stack_peek_ptr(n));
			is_stack_drop_push(argc, *v);
			break;
		}
	}
}

#define DEF_CMP_F(name, op)\
void name(int IS_UNUSED(_argc))\
{\
	ISObject *a = is_stack_peek_ptr(2);\
	ISObject *b = is_stack_peek_ptr(1);\
	if (IS_INTEGER_P(*a) && IS_INTEGER_P(*b))\
		is_stack_drop_push(2, is_bool_to_object(IS_INTEGER(*a) op IS_INTEGER(*b)));\
	else\
		is_stack_drop_push(2, is_bool_to_object(to_double(*a) op to_double(*b)));\
}

DEF_CMP_F(is_eq_number_f, ==);
DEF_CMP_F(is_ne_number_f, !=);
DEF_CMP_F(is_lt_number_f, <);
DEF_CMP_F(is_le_number_f, <=);
DEF_CMP_F(is_gt_number_f, >);
DEF_CMP_F(is_ge_number_f, >=);

void is_number_init(void)
{
	is_add_builtin_function("+", is_add_f, 0, -1);
	is_add_builtin_function("*", is_mul_f, 0, -1);
	is_add_builtin_function("-", is_sub_f, 1, -1);
	is_add_builtin_function("=", is_eq_number_f, 2, 2);
	is_add_builtin_function("/=", is_ne_number_f, 2, 2);
	is_add_builtin_function("<", is_lt_number_f, 2, 2);
	is_add_builtin_function("<=", is_le_number_f, 2, 2);
	is_add_builtin_function(">", is_gt_number_f, 2, 2);
	is_add_builtin_function(">=", is_ge_number_f, 2, 2);

}
