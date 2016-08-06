#include "lisp.h"

static ISObject **call_stack = NULL;
static int call_stack_size;
static int call_sp;

static void call_stack_resize(int size)
{
	call_stack_size = size;
	call_stack = is_xrealloc(call_stack, sizeof(ISObject*) * size);
}

void is_call_stack_print(void)
{
	for (int i = call_sp - 1; i >= 0; i--) {
		is_println(*call_stack[i]);
	}
}

static void call(ISObject *v, int argc)
{
	if (!IS_POINTER_P(*v))
		is_type_error(*v, IS_FUNCTION_TYPE);

	if (call_sp >= call_stack_size)
		call_stack_resize(call_stack_size * 2);
	call_stack[call_sp++] = v;

	IS_Stack_Pointer prev_sp = is_stack_get_pointer() - argc;

	switch (IS_HEAP_OBJECT_TYPE(*v)) {
	case IS_BUILTIN_FUNCTION_TYPE:
		if (!(IS_BUILTIN_FUNCTION_MIN(*v) <= argc &&
		      (IS_BUILTIN_FUNCTION_MAX(*v) == -1 ||
		       argc <= IS_BUILTIN_FUNCTION_MAX(*v))))
			is_argc_error(*v, argc);
		IS_BUILTIN_FUNCTION_PTR(*v) (argc);
		break;
	case IS_USER_FUNCTION_TYPE:
		IS_USER_FUNCTION_PTR(*v) (argc);
		break;
	case IS_CLOSURE_TYPE:{
			ISObject last_env = is_current_env;
			int shidx = is_shelter_add(&last_env);
			is_current_env = IS_CLOSURE_ENV(*v);
			IS_CLOSURE_PTR(*v) (argc);
			is_current_env = last_env;
			is_shelter_set_index(shidx);
			break;
		}
	default:
		is_type_error(*v, IS_FUNCTION_TYPE);
		break;
	}

	IS_Stack_Pointer sp = is_stack_get_pointer() - 1;

	if (prev_sp != sp) {
		puts("*******************");
		printf("%d %d\n", prev_sp, sp);
		is_println(*v);
		is_stack_print();
		abort();
	}

	call_sp--;
}

void is_call(ISObject *symbol, int argc)
{
	ISObject *v = &IS_SYMBOL_FUNCTION(*symbol);
	if (IS_NULL(*v))
		is_undefined_function(*symbol);
	call(v, argc);
}

void is_funcall_f(int argc)
{
	call(is_stack_peek_ptr(argc), argc - 1);
	is_stack_nip(1);
}

void is_apply_f(int argc)
{
	ISObject *func = is_stack_peek_ptr(argc);
	ISObject *list = is_stack_pop_ptr();
	if (!IS_FUNCTION_P(*func))
		is_type_error(*func, IS_FUNCTION_TYPE);
	if (!IS_LIST_P(*list))
		is_type_error(*list, IS_LIST_TYPE);

	int len = 0;
	for (ISObject rest = *list; rest != is_nil; rest = IS_CONS_CDR(rest)) {
		is_stack_push(IS_CONS_CAR(rest));
		len++;
	}

	call(func, argc-2+len);
	is_stack_nip(1);
}

void is_call_init(void)
{
	call_sp = 0;
	call_stack_resize(100);

	is_add_builtin_function("FUNCALL", is_funcall_f, 1, -1);
	is_add_builtin_function("APPLY", is_apply_f, 2, -1);
}
