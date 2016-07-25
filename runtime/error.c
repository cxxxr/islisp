#include "lisp.h"

void is_error(const char *msg, ...)
{
	va_list ap;
	va_start(ap, msg);

	for (int i = 0; msg[i] != '\0'; i++) {
		if (msg[i] == '%') {
			ISObject v = va_arg(ap, ISObject);
			is_print(v, stdout);
		} else {
			putchar(msg[i]);
		}
	}

	puts("");
	va_end(ap);

	is_call_stack_print();

	abort();
}

void is_argc_error(void)
{
	is_error("wrong Number of Arguments");
}

void is_stackoverflow(void)
{
	is_error("stack over flow");
}

void is_type_error(ISObject obj, enum ISType type)
{
	switch (type) {
		case IS_INTEGER_TYPE:
			is_error("not integer %", obj);
			break;
		case IS_CHARACTER_TYPE:
			is_error("not character %", obj);
			break;
		case IS_NUMBER_TYPE:
			is_error("not number: %", obj);
			break;
		case IS_FLOAT_TYPE:
			is_error("not float: %", obj);
			break;
		case IS_LIST_TYPE:
			is_error("not list: %", obj);
			break;
		case IS_CONS_TYPE:
			is_error("not cons: %", obj);
			break;
		case IS_SYMBOL_TYPE:
			is_error("not symbol: %", obj);
			break;
		case IS_STRING_TYPE:
			is_error("not string: %", obj);
			break;
		case IS_USER_FUNCTION_TYPE:
		case IS_BUILTIN_FUNCTION_TYPE:
		case IS_CLOSURE_TYPE:
		case IS_FUNCTION_TYPE:
			is_error("not function: %", obj);
			break;
		case IS_STREAM_TYPE:
			is_error("not stream: %", obj);
			break;
		case IS_INPUT_STREAM_TYPE:
		case IS_INPUT_STRING_STREAM_TYPE:
			is_error("not input stream: %", obj);
			break;
		case IS_OUTPUT_STREAM_TYPE:
		case IS_OUTPUT_STRING_STREAM_TYPE:
			is_error("not output stream: %", obj);
			break;
		case IS_VECTOR_TYPE:
			is_error("not vector: %", obj);
			break;
	}
}

void is_undefined_function(ISObject symbol)
{
	is_error("undefined function: %", symbol);
}

void is_unbound_variable(ISObject symbol)
{
	is_error("unbound variable: %", symbol);
}

void is_not_an_input_stream(void)
{
	is_error("not an input stream");
}

void is_index_out_of_range(ISObject vec, ISObject index)
{
	is_error("index out of range: % %", vec, index);
}

void is_error_f(int IS_UNUSED(argc))
{
	ISObject string = is_stack_peek(1);
	if (!IS_STRING_P(string)) is_type_error(string, IS_STRING_TYPE);
	is_stack_pop(); // ???
	is_error(IS_STRING_DATA(string));
}

void is_error_init(void)
{
	is_add_builtin_function("IS:ERROR", is_error_f, 1, 1);
}
