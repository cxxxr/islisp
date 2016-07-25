#include "lisp.h"

static bool gc_flag;

static const int NEWSPACE_SIZE = (1024 * 1024);

static ISObject *from_space_start;
static ISObject *to_space_start;
static ISObject *free_space;

static void is_assert(bool b)
{
	if (!b) abort();
}

void *is_xrealloc(void *p, size_t size)
{
	p = realloc(p, size);
	if (p == NULL) {
		fputs("mallocに失敗\n", stderr);
		exit(EXIT_FAILURE);
	}
	return p;
}

void *is_xmalloc(size_t size)
{
	return is_xrealloc(NULL, size);
}



void is_gc_enable(void)
{
	gc_flag = true;
}

void is_gc_disable(void)
{
	gc_flag = false;
}

static size_t obj_size(ISObject obj)
{
	switch (IS_HEAP_OBJECT_TYPE(obj)) {
		case IS_FLOAT_TYPE:
			return sizeof(ISFloat);
		case IS_CONS_TYPE:
			return sizeof(ISCons);
		case IS_SYMBOL_TYPE:
			return sizeof(ISSymbol);
		case IS_STRING_TYPE:
			return sizeof(ISString) + sizeof(char) * (IS_STRING_LENGTH(obj));
		case IS_USER_FUNCTION_TYPE:
			return sizeof(ISUserFunction);
		case IS_BUILTIN_FUNCTION_TYPE:
			return sizeof(ISBuiltinFunction);
		case IS_CLOSURE_TYPE:
			return sizeof(ISClosure);
		case IS_INPUT_STREAM_TYPE:
		case IS_OUTPUT_STREAM_TYPE:
		case IS_INPUT_STRING_STREAM_TYPE:
		case IS_OUTPUT_STRING_STREAM_TYPE:
			return sizeof(ISStream);
		case IS_VECTOR_TYPE:
			return sizeof(ISVector) + sizeof(ISObject) * IS_VECTOR_LENGTH(obj);
		default:
			printf("unknwon type: %d\n", IS_HEAP_OBJECT_TYPE(obj));
			exit(EXIT_FAILURE);
	}
}

static bool from_space_p(ISObject obj)
{
	return (ISObject)from_space_start <= obj && obj <= (ISObject)from_space_start + NEWSPACE_SIZE;
}

static size_t alignment(size_t size)
{
	return (size + IS_OBJECT_MASK) & ~IS_OBJECT_MASK;
}

static ISObject copy(ISObject obj)
{
	if (!IS_POINTER_P(obj)) return obj;

	if (from_space_p(IS_HEAP_OBJECT_FORWARDING(obj))) {
		size_t size = alignment(obj_size(obj));
		memcpy(free_space, IS_OBJECT_PTR(obj), size);
		IS_HEAP_OBJECT_FORWARDING(obj) = (ISObject)free_space;
		free_space += size / sizeof(ISObject);
	}

	return IS_HEAP_OBJECT_FORWARDING(obj);
}

static void copy_obj_children(ISObject obj)
{
	if (!IS_POINTER_P(obj)) return;

	switch (IS_HEAP_OBJECT_TYPE(obj)) {
		case IS_CONS_TYPE:
			IS_CONS_CAR(obj) = copy(IS_CONS_CAR(obj));
			IS_CONS_CDR(obj) = copy(IS_CONS_CDR(obj));
			break;
		case IS_SYMBOL_TYPE:
			if (!IS_NULL(IS_SYMBOL_NAME(obj)))
				IS_SYMBOL_NAME(obj) = copy(IS_SYMBOL_NAME(obj));
			if (!IS_NULL(IS_SYMBOL_GLOBAL(obj)))
				IS_SYMBOL_GLOBAL(obj) = copy(IS_SYMBOL_GLOBAL(obj));
			if (!IS_NULL(IS_SYMBOL_DYNAMIC(obj)))
				IS_SYMBOL_DYNAMIC(obj) = copy(IS_SYMBOL_DYNAMIC(obj));
			if (!IS_NULL(IS_SYMBOL_PROPERTY(obj)))
				IS_SYMBOL_PROPERTY(obj) = copy(IS_SYMBOL_PROPERTY(obj));
			if (!IS_NULL(IS_SYMBOL_FUNCTION(obj)))
				IS_SYMBOL_FUNCTION(obj) = copy(IS_SYMBOL_FUNCTION(obj));
			break;
		case IS_CLOSURE_TYPE:
			IS_CLOSURE_ENV(obj) = copy(IS_CLOSURE_ENV(obj));
			break;
		case IS_VECTOR_TYPE:
		{
			ISVector *vec = (ISVector*)(obj);
			for (int i = 0; i < vec->len; i++) {
				vec->data[i] = copy(vec->data[i]);
			}
			break;
		}
	}
}

static void copy_gc(void)
{
	if (!gc_flag) return;

	ISObject *scan = to_space_start;
	free_space = to_space_start;

	for (int i = 0; i < is_symbol_table_size; i++) {
		if (!IS_NULL(is_symbol_table[i])) {
			is_symbol_table[i] = copy(is_symbol_table[i]);
		}
	}

	for (int i = 0; i < is_shelter_used; i++) {
		*is_shelter[i] = copy(*is_shelter[i]);
	}

	for (ISObject *p = is_stack_top - 1; p >= is_stack_low; p--) {
		*p = copy(*p);
	}

	while (scan < free_space) {
		copy_obj_children((ISObject)scan);
		size_t bytes = alignment(obj_size((ISObject)scan));
		is_assert(bytes > 0);
		scan += bytes / sizeof(ISObject);
	}

	ISObject *tmp = from_space_start;
	from_space_start = to_space_start;
	to_space_start = tmp;
}

static bool require_gc(size_t size)
{
	return (free_space + size / sizeof(ISObject) >= from_space_start + NEWSPACE_SIZE / sizeof(ISObject));
}

static ISObject* alloc(size_t size)
{
	if (256 <= size) {
		return is_xmalloc(size);
	}

	size = alignment(size);
#if 1
	if (require_gc(size)) {
		copy_gc();
		if (require_gc(size)) {
			is_error("allocation fail");
		}
	}
#else
	copy_gc();
#endif
	ISObject *result = free_space;
	free_space += (size / sizeof(ISObject));
	return (ISObject*)result;
}



ISObject is_make_float(double v)
{
	ISFloat *flt = (ISFloat*)alloc(sizeof(ISFloat));
	flt->forwarding = 0;
	flt->type = IS_FLOAT_TYPE;
	flt->value = v;
	return (ISObject) flt;
}

ISObject is_make_cons(ISObject *car, ISObject *cdr)
{
	ISCons *cons = (ISCons*)alloc(sizeof(ISCons));
	cons->forwarding = 0;
	cons->type = IS_CONS_TYPE;
	cons->car = *car;
	cons->cdr = *cdr;
	return (ISObject) cons;
}

ISObject is_make_symbol(ISObject *name)
{
	ISSymbol *symbol = (ISSymbol*)alloc(sizeof(ISSymbol));
	symbol->forwarding = 0;
	symbol->type = IS_SYMBOL_TYPE;
	symbol->name = (IS_INTEGER_P(name) ? (ISObject)name : *name);
	symbol->global = (ISObject) NULL;
	symbol->dynamic = (ISObject) NULL;
	symbol->property = is_nil;
	symbol->function = (ISObject) NULL;
	return (ISObject) symbol;
}

ISObject is_make_string(const char *data)
{
	int len = strlen(data);
	ISString *string = (ISString*)alloc(sizeof(ISString) + sizeof(char) * len);
	string->forwarding = 0;
	string->type = IS_STRING_TYPE;
	string->len = len;
	strcpy(string->data, data);
	return (ISObject) string;
}

ISObject is_make_string_fill(int len, char c)
{
	ISString *string = (ISString*)alloc(sizeof(ISString) + sizeof(char) * len);
	string->forwarding = 0;
	string->type = IS_STRING_TYPE;
	string->len = len;
	memset(string->data, c, len);
	string->data[len] = '\0';
	return (ISObject) string;
}

ISObject is_make_user_function(ISObject *name, ISFuncPtr ptr)
{
	ISUserFunction *func = (ISUserFunction*)alloc(sizeof(ISUserFunction));
	func->forwarding = 0;
	func->type = IS_USER_FUNCTION_TYPE;
	func->name = *name;
	func->ptr = ptr;
	return (ISObject) func;
}

ISObject is_make_builtin_function(ISFuncPtr ptr, const char *name, int min, int max)
{
	ISBuiltinFunction *func = (ISBuiltinFunction*)alloc(sizeof(ISBuiltinFunction));
	func->forwarding = 0;
	func->type = IS_BUILTIN_FUNCTION_TYPE;
	func->ptr = ptr;
	func->name = name;
	func->min = min;
	func->max = max;
	return (ISObject) func;
}

ISObject is_make_closure(ISFuncPtr ptr)
{
	ISClosure *func = (ISClosure*)alloc(sizeof(ISClosure));
	func->forwarding = 0;
	func->type = IS_CLOSURE_TYPE;
	func->env = is_current_env;
	func->ptr = ptr;
	return (ISObject) func;
}

ISObject is_make_input_stream(FILE *ptr)
{
	ISStream *stream = (ISStream*)alloc(sizeof(ISStream));
	stream->forwarding = 0;
	stream->type = IS_INPUT_STREAM_TYPE;
	stream->ptr = ptr;
	return (ISObject) stream;
}

ISObject is_make_output_stream(FILE *ptr)
{
	ISStream *stream = (ISStream*)alloc(sizeof(ISStream));
	stream->forwarding = 0;
	stream->type = IS_OUTPUT_STREAM_TYPE;
	stream->ptr = ptr;
	return (ISObject) stream;
}

ISObject is_make_vector(int len, ISObject *v)
{
	ISVector *vector = (ISVector*)alloc(sizeof(ISVector) + sizeof(ISObject) * len);
	vector->forwarding = 0;
	vector->type = IS_VECTOR_TYPE;
	vector->len = len;
	for (int i = 0; i < len; i++) vector->data[i] = *v;
	return (ISObject) vector;
}



void is_heap_init(void)
{
	from_space_start = is_xmalloc(NEWSPACE_SIZE);
	to_space_start = is_xmalloc(NEWSPACE_SIZE);
	free_space = from_space_start;
}
