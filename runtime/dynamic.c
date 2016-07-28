#include "lisp.h"

struct dynamic_stack_element {
	ISObject symbol;
	ISObject value;
};

static struct dynamic_stack_element *stack;
static int stack_size;
static int sp;

static void is_dynamic_resize(int size)
{
	stack = is_xrealloc(stack, sizeof(struct dynamic_stack_element) * size);
	stack_size = size;
}

void is_dynamic_init(void)
{
	is_dynamic_resize(8);
}

ISObject is_dynamic_get(ISObject symbol)
{
	for (int i = sp - 1; i >= 0; i--) {
		if (symbol == stack[i].symbol) {
			return stack[i].value;
		}
	}
	is_error("Unbound Dynamic Variable: %", symbol);
	return is_nil;
}

void is_dynamic_set(ISObject symbol, ISObject value)
{
	for (int i = sp - 1; i >= 0; i--) {
		if (symbol == stack[i].symbol) {
			stack[i].value = value;
		}
	}
	is_error("Unbound Dynamic Variable: %", symbol);
}

void is_dynamic_push(ISObject symbol, ISObject value)
{
	if (sp >= stack_size)
		is_dynamic_resize(stack_size * 2);
	stack[sp].symbol = symbol;
	stack[sp].value = value;
	sp++;
}

void is_dynamic_pop(int n)
{
	sp -= n;
}
