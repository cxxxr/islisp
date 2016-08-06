#include "lisp.h"
#include "stack.h"

void is_stack_init(void)
{
	is_stack_low = is_stack_top = &is_stack[0];
	is_stack_high = is_stack_top + IS_STACK_SIZE;
	is_add_builtin_function("IS:STACK-PRINT", is_stack_print_f, 0, 0);
}

IS_Stack_Pointer is_stack_get_pointer(void)
{
	return is_stack_top;
}

void is_stack_set_pointer(IS_Stack_Pointer stack_pointer)
{
	is_stack_top = stack_pointer;
}

void is_stack_push(ISObject x)
{
	if (is_stack_top >= is_stack_high)
		is_stackoverflow();
	*(is_stack_top++) = x;
}

ISObject *is_stack_pop_ptr(void)
{
	assert(is_stack_top > is_stack_low);
	return --is_stack_top;
}

ISObject is_stack_pop(void)
{
	assert(is_stack_top > is_stack_low);
	return *(--is_stack_top);
}

ISObject *is_stack_peek_ptr(int offset)
{
	return is_stack_top - offset;
}

ISObject is_stack_peek(int offset)
{
	return *(is_stack_top - offset);
}

void is_stack_nip(int n)
{
	if (n > 0) {
		*(is_stack_top - n - 1) = *(is_stack_top - 1);
		is_stack_top -= n;
	}
}

bool is_stack_top_null(void)
{
	return is_stack_pop() == is_nil;
}

void is_stack_drop(int n)
{
	is_stack_top -= n;
}

void is_stack_drop_push(int n, ISObject v)
{
	is_stack_top -= n;
	*(is_stack_top++) = v;
}

void is_stack_change_tos(ISObject v)
{
	*(is_stack_top - 1) = v;
}

void is_stack_build_list(int n)
{
	if (n == 0) {
		is_stack_push(is_nil);
		return;
	}

	ISObject acc = is_nil;
	int shidx = is_shelter_add(&acc);

	for (int i = n; i > 0; i--) {
		acc = is_make_cons(is_stack_peek_ptr(i), &acc);
		is_shelter_add(&acc);
	}

	is_stack_drop_push(n, acc);
	is_nreverse_f(1);

	is_shelter_set_index(shidx);
}

void is_stack_print(void)
{
	puts("\n*** DUMP STACK ***");

	if (is_stack_top < is_stack_low) {
		fputs("stack error\n", stderr);
		abort();
	}

	int i;
	ISObject *p;
	for (p = is_stack_top - 1, i = 1; p >= is_stack_low; p--, i++) {
		fprintf(stdout, "%d: ", i);
		is_print(*p, stdout);
		puts("");
	}

	puts("*** DUMP STACK END ***");
}

void is_stack_print_f(int IS_UNUSED(argc))
{
	is_stack_print();
	is_stack_push(is_nil);
}
