#include "lisp.h"

ISObject is_current_env;

void is_env_init(void)
{
	is_current_env = is_nil;
	is_shelter_add(&is_current_env);
}

void is_env_extend(int n, ...)
{
	va_list args;
	va_start(args, n);
	while (n-- > 0) {
		int offset = va_arg(args, int);
		is_current_env =
		    is_make_cons(is_stack_peek_ptr(offset),
				 &is_current_env);
	}
	va_end(args);
}

ISObject *is_env_get(int i)
{
	ISObject env = is_current_env;
	while (i-- > 0)
		env = IS_CONS_CDR(env);
	return &IS_CONS_CAR(env);
}
