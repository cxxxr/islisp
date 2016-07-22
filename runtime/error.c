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

void is_type_error(ISObject IS_UNUSED(obj), enum ISType IS_UNUSED(type))
{
	is_error("type error");
}

void is_undefined_function(ISObject symbol)
{
	is_error("undefined function: %", symbol);
}

void is_unbound_variable(ISObject symbol)
{
	is_error("unbound variable: %", symbol);
}
