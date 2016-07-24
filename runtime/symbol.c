#include "lisp.h"

ISObject *is_symbol_table;
int is_symbol_table_size;

ISObject is_symbol_t;
ISObject is_symbol_nil;

static int hashval(const char *str)
{
	int val = 0;
	for (int i = 0; str[i] != '\0'; i++) {
		val += str[i];
	}
	return val % is_symbol_table_size;
}

static void init_symbol_table(void)
{
	is_symbol_table_size = 1009;
	is_symbol_table = is_xmalloc(sizeof(ISObject) * is_symbol_table_size);
	int i;
	for (i = 0; i < is_symbol_table_size; i++) {
		is_symbol_table[i] = (ISObject) NULL;
	}
}

ISObject is_intern(ISObject *string)
{
	const char *p = IS_STRING_DATA(*string);
	int val = hashval(p);
	int k = 1;

	for (;;) {
		if (IS_NULL(is_symbol_table[val])) {
			is_symbol_table[val] = is_make_symbol(string);
			return is_symbol_table[val];
		} else if (!strcmp(IS_STRING_DATA(IS_SYMBOL_NAME(is_symbol_table[val])), p)) {
			return is_symbol_table[val];
		} else {
			val = (val + k * k) % is_symbol_table_size;
			k++;
		}
	}
}

ISObject is_bool_to_object(bool b)
{
	return b ? is_symbol_t : is_symbol_nil;
}

ISObject is_symbol_function(ISObject obj)
{
	return IS_SYMBOL_FUNCTION(obj);
}

void is_symbol_set_function(ISObject symbol, ISObject function)
{
	IS_SYMBOL_FUNCTION(symbol) = function;
}

ISObject is_symbol_global(ISObject symbol)
{
	if (IS_NULL(IS_SYMBOL_GLOBAL(symbol)))
		is_unbound_variable(symbol);
	return IS_SYMBOL_GLOBAL(symbol);
}

void is_symbol_set_global(ISObject symbol, ISObject value)
{
	// write barrior
	IS_SYMBOL_GLOBAL(symbol) = value;
}

ISObject is_gensym(void)
{
	return is_make_symbol(NULL);
}

void is_add_builtin_function(const char *name, ISFuncPtr ptr, int min, int max)
{
	ISObject string = is_make_string(name);
	ISObject symbol = is_intern(&string);
	is_symbol_set_function(symbol,
			       is_make_builtin_function(ptr, name, min,
							max));
}

void is_symbol_init(void)
{
	init_symbol_table();

	ISObject nil_string = is_make_string("NIL");
	is_symbol_nil = is_intern(&nil_string);

	ISObject t_string = is_make_string("T");
	is_symbol_t = is_intern(&t_string);

	is_shelter_add(&is_symbol_nil);
	is_shelter_add(&is_symbol_t);
}
