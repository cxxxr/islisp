#include "lisp.h"

static int is_dynamic_stack_size;
struct dynamic_stack_element *is_dynamic_stack;
int is_dynamic_sp;

static void is_dynamic_resize(int size)
{
	is_dynamic_stack = is_xrealloc(is_dynamic_stack, sizeof(struct dynamic_stack_element) * size);
	is_dynamic_stack_size = size;
}

void is_dynamic_init(void)
{
	is_dynamic_resize(8);
}

ISObject is_dynamic_get(ISObject symbol)
{
	for (int i = is_dynamic_sp - 1; i >= 0; i--) {
		if (symbol == is_dynamic_stack[i].symbol) {
			return is_dynamic_stack[i].value;
		}
	}
	is_error("Unbound Dynamic Variable: %", symbol);
	return is_nil;
}

void is_dynamic_set(ISObject symbol, ISObject value)
{
	for (int i = is_dynamic_sp - 1; i >= 0; i--) {
		if (symbol == is_dynamic_stack[i].symbol) {
			// write barrior ???
			is_dynamic_stack[i].value = value;
		}
	}
	is_error("Unbound Dynamic Variable: %", symbol);
}

void is_dynamic_push(ISObject symbol, ISObject value)
{
	if (is_dynamic_sp >= is_dynamic_stack_size)
		is_dynamic_resize(is_dynamic_stack_size * 2);
	is_dynamic_stack[is_dynamic_sp].symbol = symbol;
	is_dynamic_stack[is_dynamic_sp].value = value;
	is_dynamic_sp++;
}

void is_dynamic_pop(int n)
{
	is_dynamic_sp -= n;
}
