#include "lisp.h"
#include "stack.h"

void is_stack_init(void)
{
	is_stack_low = is_stack_top = &is_stack[0];
	is_stack_high = is_stack_top + IS_STACK_SIZE;
	is_add_builtin_function("IS:STACK-PRINT", is_stack_print_f, 0, 0);
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

	ISObject head = is_nil;
	ISObject tail = is_nil;

	int drop_n = n;
	int shidx = -1;

	while (n > 0) {
		if (head == is_nil) {
			tail = head = is_make_cons(is_stack_peek_ptr(n--), &is_nil);
			shidx = is_shelter_add(&head);
		} else {
			IS_CONS_CDR(tail) = is_make_cons(is_stack_peek_ptr(n--), &is_nil);
			tail = IS_CONS_CDR(tail);
		}
	}

	if (shidx != -1)
		is_shelter_set_index(shidx);

	is_stack_drop_push(drop_n, head);
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
	is_stack_push(is_make_integer(is_stack_get_sp()));
}

int is_stack_get_sp(void)
{
	return (is_stack_top - is_stack_low) / sizeof(ISObject);
}
