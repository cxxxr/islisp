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
	ISObject func = IS_SYMBOL_FUNCTION(obj);
	if (IS_NULL(func)) is_undefined_function(obj);
	return func;
}

void is_symbol_set_function(ISObject symbol, ISObject function)
{
	// write barrior ???
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
	static int counter = -1;
	counter++;
	return is_make_symbol((ISObject*)is_make_integer(counter));
}

void is_symbolp_f(int IS_UNUSED(argc))
{
	is_stack_change_tos(is_bool_to_object(IS_SYMBOL_P(is_stack_peek(1))));
}

void is_gensym_f(int IS_UNUSED(argc))
{
	is_stack_push(is_gensym());
}

void is_eql_f(int IS_UNUSED(argc))
{
	is_stack_drop_push(2, is_bool_to_object(is_stack_peek(1) == is_stack_peek(2)));
}

void is_property_f(int argc)
{
	ISObject symbol;
	ISObject propname;
	ISObject default_;

	if (argc == 2) {
		symbol = is_stack_peek(2);
		propname = is_stack_peek(1);
		default_ = is_nil;
	} else {
		symbol = is_stack_peek(3);
		propname = is_stack_peek(2);
		default_ = is_stack_peek(1);
	}

	if (!IS_SYMBOL_P(symbol))
		is_type_error(symbol, IS_SYMBOL_TYPE);

	for (ISObject plist = IS_SYMBOL_PROPERTY(symbol);
	     plist != is_nil;
	     plist = IS_CONS_CDR(IS_CONS_CDR(plist))) {
		if (IS_CONS_CAR(plist) == propname) {
			is_stack_drop_push(argc, IS_CONS_CAR(IS_CONS_CDR(plist)));
			return;
		}
		plist = IS_CONS_CDR(IS_CONS_CDR(plist));
	}

	is_stack_drop_push(argc, default_);
}

void is_set_property_f(int IS_UNUSED(argc))
{
	ISObject *value = is_stack_peek_ptr(3);
	ISObject *symbol = is_stack_peek_ptr(2);
	ISObject *propname = is_stack_peek_ptr(1);

	if (!IS_SYMBOL_P(*symbol))
		is_type_error(*symbol, IS_SYMBOL_TYPE);

	for (ISObject plist = IS_SYMBOL_PROPERTY(*symbol);
	     plist != is_nil;
	     plist = IS_CONS_CDR(IS_CONS_CDR(plist))) {
		if (IS_CONS_CAR(plist) == *propname) {
			is_set_car(*value, IS_CONS_CDR(plist));
			goto END;
		}
		plist = IS_CONS_CDR(IS_CONS_CDR(plist));
	}

	ISObject plist = IS_SYMBOL_PROPERTY(*symbol);
	int shidx = is_shelter_add(&plist);
	ISObject plist2 = is_make_cons(value, &plist);
	is_shelter_add(&plist2);
	ISObject plist3 = is_make_cons(propname, &plist2);
	is_shelter_add(&plist3);

	// write barrior
	IS_SYMBOL_PROPERTY(*symbol) = plist3;

	is_shelter_set_index(shidx);

 END:
	is_stack_drop_push(3, *value);
}

void is_symbol_function_f(int IS_UNUSED(argc))
{
	ISObject symbol = is_stack_peek(1);
	if (!IS_SYMBOL_P(symbol))
		is_type_error(symbol, IS_SYMBOL_TYPE);
	is_stack_change_tos(is_symbol_function(symbol));
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

	is_add_builtin_function("GENSYM", is_gensym_f, 0, 0);
	is_add_builtin_function("SYMBOLP", is_symbolp_f, 1, 1);
	is_add_builtin_function("EQ", is_eql_f, 2, 2);
	is_add_builtin_function("EQL", is_eql_f, 2, 2);
	is_add_builtin_function("PROPERTY", is_property_f, 2, 3);
	is_add_builtin_function("SET-PROPERTY", is_set_property_f, 3, 3);
	is_add_builtin_function("IS:SYMBOL-FUNCTION", is_symbol_function_f, 1, 1);
}
