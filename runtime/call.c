#include "lisp.h"

static void call(ISObject v, int argc)
{
	if (!IS_POINTER_P(v))
		is_type_error(v, IS_FUNCTION_TYPE);
	switch (IS_HEAP_OBJECT_TYPE(v)) {
	case IS_BUILTIN_FUNCTION_TYPE:
		if (!(IS_BUILTIN_FUNCTION_MIN(v) <= argc &&
		      (IS_BUILTIN_FUNCTION_MAX(v) == -1 ||
		       argc <= IS_BUILTIN_FUNCTION_MAX(v))))
			is_argc_error();
		IS_BUILTIN_FUNCTION_PTR(v) (argc);
		break;
	case IS_USER_FUNCTION_TYPE:
		IS_USER_FUNCTION_PTR(v) (argc);
		break;
	case IS_CLOSURE_TYPE:{
			ISObject *last_env = &is_current_env;
			int shidx = is_shelter_add(last_env);
			is_current_env = IS_CLOSURE_ENV(v);
			IS_CLOSURE_PTR(v) (argc);
			is_current_env = *last_env;
			is_shelter_set_index(shidx);
			break;
		}
	default:
		is_type_error(v, IS_FUNCTION_TYPE);
		break;
	}
}

void is_call(ISObject symbol, int argc)
{
	ISObject v = IS_SYMBOL_FUNCTION(symbol);
	if (IS_NULL(v))
		is_undefined_function(symbol);
	call(v, argc);
}

void is_funcall_f(int argc)
{
	call(is_stack_peek(argc), argc - 1);
	is_stack_nip(1);
}

void is_call_init(void)
{
	is_add_builtin_function("FUNCALL", is_funcall_f, 1, -1);
}
