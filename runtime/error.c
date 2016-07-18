#include "lisp.h"

void is_error(const char *msg)
{
	fprintf(stderr, "%s\n", msg);
	exit(EXIT_FAILURE);
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

void is_undefined_function(ISObject IS_UNUSED(symbol))
{
	is_error("undefined function");
}
