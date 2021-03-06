#ifndef LISP_H
#define LISP_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>

#ifdef _IS_MAIN_
#define EXTERN
#else
#define EXTERN extern
#endif

typedef uintptr_t ISObject;

typedef void (*ISFuncPtr) (int);

enum ISType {
	IS_NUMBER_TYPE,
	IS_INTEGER_TYPE,
	IS_CHARACTER_TYPE,

	IS_FLOAT_TYPE,

	IS_LIST_TYPE,
	IS_CONS_TYPE,

	IS_SYMBOL_TYPE,

	IS_STRING_TYPE,

	IS_FUNCTION_TYPE,
	IS_USER_FUNCTION_TYPE,
	IS_BUILTIN_FUNCTION_TYPE,
	IS_CLOSURE_TYPE,

	IS_STREAM_TYPE,
	IS_INPUT_STREAM_TYPE,
	IS_OUTPUT_STREAM_TYPE,
	IS_INPUT_STRING_STREAM_TYPE,
	IS_OUTPUT_STRING_STREAM_TYPE,

	IS_VECTOR_TYPE,
};

#define IS_OBJECT_MASK 0x7

#define IS_INTEGER_TAG 0x1
#define IS_CHARACTER_TAG 0x2

#define IS_POINTER_P(obj) ((((intptr_t)obj) & IS_OBJECT_MASK) == 0)

#define IS_INTEGER(obj) (((intptr_t)obj) >> 3)
#define IS_INTEGER_P(obj) ((((intptr_t)obj) & IS_INTEGER_TAG) == IS_INTEGER_TAG)

#define IS_CHARACTER(obj) (((intptr_t)obj) >> 3)
#define IS_CHARACTER_P(obj) ((((intptr_t)obj) & IS_CHARACTER_TAG) == IS_CHARACTER_TAG)

#define IS_NULL(obj) (obj == (ISObject)NULL)
#define IS_OBJECT_PTR(obj) ((void*)obj)

#define IS_FLOAT_P(obj) (IS_POINTER_P(obj) && IS_HEAP_OBJECT_TYPE(obj) == IS_FLOAT_TYPE)
#define IS_CONS_P(obj) (IS_POINTER_P(obj) && IS_HEAP_OBJECT_TYPE(obj) == IS_CONS_TYPE)
#define IS_LIST_P(obj) (((obj) == is_nil) || IS_CONS_P(obj))
#define IS_SYMBOL_P(obj) (IS_POINTER_P(obj) && IS_HEAP_OBJECT_TYPE(obj) == IS_SYMBOL_TYPE)
#define IS_STRING_P(obj) (IS_POINTER_P(obj) && IS_HEAP_OBJECT_TYPE(obj) == IS_STRING_TYPE)
#define IS_FUNCTION_P(obj) (IS_POINTER_P(obj) && \
				(IS_HEAP_OBJECT_TYPE(obj) == IS_USER_FUNCTION_TYPE || \
				 IS_HEAP_OBJECT_TYPE(obj) == IS_BUILTIN_FUNCTION_TYPE || \
				 IS_HEAP_OBJECT_TYPE(obj) == IS_CLOSURE_TYPE))
#define IS_INPUT_STREAM_P(obj) \
	(IS_POINTER_P(obj) && \
	 (IS_HEAP_OBJECT_TYPE(obj) == IS_INPUT_STREAM_TYPE || \
	  IS_HEAP_OBJECT_TYPE(obj) == IS_INPUT_STRING_STREAM_TYPE))
#define IS_OUTPUT_STREAM_P(obj) \
	(IS_POINTER_P(obj) && \
	 (IS_HEAP_OBJECT_TYPE(obj) == IS_OUTPUT_STREAM_TYPE || \
	  IS_HEAP_OBJECT_TYPE(obj) == IS_OUTPUT_STRING_STREAM_TYPE))
#define IS_VECTOR_P(obj) (IS_POINTER_P(obj) && IS_HEAP_OBJECT_TYPE(obj) == IS_VECTOR_TYPE)

#define is_make_integer(v) ((ISObject)(((v) << 3) | IS_INTEGER_TAG))
#define is_make_character(c) (((ISObject)((c) << 3)) | IS_CHARACTER_TAG)

#define IS_OBJECT_HEADER \
	uint8_t type; \
	ISObject forwarding;

#define IS_HEAP_OBJECT_TYPE(obj) (((ISValue*)obj)->type)
#define IS_HEAP_OBJECT_FORWARDING(obj) (((ISValue*)obj)->forwarding)

typedef struct {
	IS_OBJECT_HEADER;
} ISValue;

typedef struct {
	IS_OBJECT_HEADER;
	double value;
} ISFloat;

#define IS_FLOAT_VALUE(obj) (((ISFloat*)obj)->value)

typedef struct {
	IS_OBJECT_HEADER;
	ISObject car;
	ISObject cdr;
} ISCons;

#define IS_CONS_CAR(obj) (((ISCons*)obj)->car)
#define IS_CONS_CDR(obj) (((ISCons*)obj)->cdr)

typedef struct {
	IS_OBJECT_HEADER;
	ISObject name;
	ISObject global;
	ISObject property;
	ISObject function;
} ISSymbol;

#define IS_SYMBOL_NAME(obj) (((ISSymbol*)obj)->name)
#define IS_SYMBOL_GLOBAL(obj) (((ISSymbol*)obj)->global)
#define IS_SYMBOL_PROPERTY(obj) (((ISSymbol*)obj)->property)
#define IS_SYMBOL_FUNCTION(obj) (((ISSymbol*)obj)->function)

typedef struct {
	IS_OBJECT_HEADER;
	int len;
	char data[1];
} ISString;

#define IS_STRING_DATA(obj) (((ISString*)obj)->data)
#define IS_STRING_LENGTH(obj) (((ISString*)obj)->len)

typedef struct {
	IS_OBJECT_HEADER;
	ISObject name;
	ISFuncPtr ptr;
} ISUserFunction;

#define IS_USER_FUNCTION_NAME(obj) (((ISUserFunction*)obj)->name)
#define IS_USER_FUNCTION_PTR(obj) (((ISUserFunction*)obj)->ptr)

typedef struct {
	IS_OBJECT_HEADER;
	ISFuncPtr ptr;
	const char *name;
	int min;
	int max;
} ISBuiltinFunction;

#define IS_BUILTIN_FUNCTION_MIN(obj) (((ISBuiltinFunction*)obj)->min)
#define IS_BUILTIN_FUNCTION_MAX(obj) (((ISBuiltinFunction*)obj)->max)
#define IS_BUILTIN_FUNCTION_NAME(obj) (((ISBuiltinFunction*)obj)->name)
#define IS_BUILTIN_FUNCTION_PTR(obj) (((ISBuiltinFunction*)obj)->ptr)

typedef struct {
	IS_OBJECT_HEADER;
	ISObject env;
	ISFuncPtr ptr;
} ISClosure;

#define IS_CLOSURE_ENV(obj) (((ISClosure*)obj)->env)
#define IS_CLOSURE_PTR(obj) (((ISClosure*)obj)->ptr)

typedef struct {
	IS_OBJECT_HEADER;
	union {
		FILE *ptr;
	};
} ISStream;

#define IS_STREAM_PTR(obj) (((ISStream*)obj)->ptr)

typedef struct {
	IS_OBJECT_HEADER;
	int len;
	ISObject data[1];
} ISVector;

#define IS_VECTOR_LENGTH(obj) (((ISVector*)obj)->len)
#define IS_VECTOR_DATA(obj) (((ISVector*)obj)->data)

extern ISObject **is_shelter;
extern int is_shelter_used;
extern int is_shelter_size;

extern ISObject *is_symbol_table;
extern int is_symbol_table_size;

extern ISObject is_current_env;

extern ISObject is_symbol_t;
extern ISObject is_symbol_nil;
#define is_nil is_symbol_nil

struct dynamic_stack_element {
	ISObject symbol;
	ISObject value;
};

extern struct dynamic_stack_element *is_dynamic_stack;
extern int is_dynamic_sp;

enum is_gc_state {
	IS_GC_DISABLE,
	IS_GC_ENABLE,
	IS_GC_PROHIBITION,
};

// error.c
void is_error(const char *, ...);
void is_argc_error(ISObject, int);
void is_stackoverflow(void);
void is_type_error(ISObject, enum ISType);
void is_undefined_function(ISObject);
void is_unbound_variable(ISObject);
void is_not_an_input_stream(void);
void is_index_out_of_range(ISObject, ISObject);
void is_error_f(int);
void is_error_init(void);

// symbol.c
ISObject is_intern(ISObject *);
ISObject is_bool_to_object(bool);
ISObject is_symbol_function(ISObject);
void is_symbol_set_function(ISObject, ISObject);
ISObject is_symbol_global(ISObject);
void is_symbol_set_global(ISObject, ISObject);
ISObject is_gensym(void);
void is_gensym_f(int);
void is_symbolp_f(int);
void is_eql_f(int);
void is_property_f(int);
void is_set_property_f(int);
void is_symbol_function_f(int);
void is_add_builtin_function(const char *, ISFuncPtr, int, int);
void is_symbol_init(void);

// stream.c
void is_read_char(ISObject, ISObject, ISObject);
void is_read_char_f(int);
void is_standard_output_f(int);
void is_standard_input_f(int);
void is_error_output_f(int);
void is_stream_init(void);

// character.c
void is_characterp_f(int);
void is_char_eq_f(int);
void is_char_ne_f(int);
void is_char_lt_f(int);
void is_char_gt_f(int);
void is_char_le_f(int);
void is_char_ge_f(int);
void is_character_init(void);

// number.c
void is_add_2f(void);
void is_mul_2f(void);
void is_add_f(int);
void is_mul_f(int);
void is_sub_2f(void);
void is_sub_f(int);
void is_eq_number_f(int);
void is_ne_number_f(int);
void is_lt_number_f(int);
void is_le_number_f(int);
void is_gt_number_f(int);
void is_ge_number_f(int);
void is_number_init(void);

// cons.c
void is_consp_f(int);
void is_cons_f(int);
void is_car_f(int);
void is_cdr_f(int);
void is_set_car(ISObject, ISObject);
void is_set_cdr(ISObject, ISObject);
void is_set_car_f(int);
void is_set_cdr_f(int);
void is_nreverse_f(int);
void is_cons_init(void);

// vector.c
void is_create_vector_2f(void);
void is_create_vector_f(int);
void is_vector_init(void);

// string.c
void is_create_string_f(int);
void is_string_ref_f(int);
void is_string_set_f(int);
void is_string_length_f(int);
void is_string_init(void);

// convert.c
void is_convert_char_to_integer_f(int);
void is_convert_integer_to_char_f(int);
void is_convert_string_to_symbol_f(int);
void is_convert_init(void);

// shelter.c
int is_shelter_add(ISObject *);
void is_shelter_set_index(int);
void is_shelter_print(void);
void is_shelter_init(void);

// heap.c
void *is_xrealloc(void *, size_t);
void *is_xmalloc(size_t);
void is_gc_enable(void);
void is_gc_prohibition(void);
void is_gc_disable(void);
void is_check_valid_pointer(ISObject);
void is_heap_init(void);
ISObject is_make_float(double);
ISObject is_make_cons(ISObject *, ISObject *);
ISObject is_make_symbol(ISObject *);
ISObject is_make_string(const char *);
ISObject is_make_string_fill(int, char);
ISObject is_make_user_function(ISObject *, ISFuncPtr);
ISObject is_make_builtin_function(ISFuncPtr, const char *, int, int);
ISObject is_make_closure(ISFuncPtr);
ISObject is_make_input_stream(FILE *);
ISObject is_make_output_stream(FILE *);
ISObject is_make_vector(int, ISObject *);

// env.c
void is_env_init(void);
void is_env_extend(int, ...);
ISObject *is_env_get(int i);

// dynamic.c
void is_dynamic_init(void);
ISObject is_dynamic_get(ISObject);
void is_dynamic_set(ISObject, ISObject);
void is_dynamic_push(ISObject, ISObject);
void is_dynamic_pop(int);

// call.c
void is_call_stack_print(void);
void is_call(ISObject*, int);
void is_funcall_f(int);
void is_apply_f(int);
void is_call_init(void);

// print.c
void is_print(ISObject, FILE *);
void is_println(ISObject obj);
void is_print_f(int);
void is_print_init(void);

// lisp.c
void is_init(void);

#define IS_UNUSED(v) v __attribute__((unused))

#include "stack.h"

#endif
