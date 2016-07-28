#include "lisp.h"
#include "stack.h"

void is_stack_init(void)
{
	is_stack_low = is_stack_top = &is_stack[0];
	is_stack_high = is_stack_top + IS_STACK_SIZE;
}

void is_stack_push(ISObject x)
{
	if (is_stack_top >= is_stack_high)
		is_stackoverflow();
	*(is_stack_top++) = (x);
}

ISObject *is_stack_pop_ptr(void)
{
	return --is_stack_top;
}

ISObject is_stack_pop(void)
{
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
	return *(--is_stack_top) == is_nil;
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

void is_stack_print(int n)
{
	int i;
	if (n < 0) {
		for (i = 1; (is_stack_top - i) != is_stack_low; i++) {
			is_print(*(is_stack_top - i), stdout);
			puts("");
		}
	} else {
		for (i = 1; i <= n; i++) {
			is_print(*(is_stack_top - i), stdout);
			puts("");
		}
	}
}
