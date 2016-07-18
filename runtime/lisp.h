#ifndef LISP_H
#define LISP_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>

#ifdef _IS_MAIN_
#define EXTERN
#else
#define EXTERN extern
#endif

typedef uintptr_t ISObject;

typedef void (*ISFuncPtr) (int);

enum ISType {
	IS_NUMBER_TYPE,
	IS_FLOAT_TYPE,
	IS_CONS_TYPE,
	IS_SYMBOL_TYPE,
	IS_STRING_TYPE,
	IS_USER_FUNCTION_TYPE,
	IS_BUILTIN_FUNCTION_TYPE,
	IS_CLOSURE_TYPE,
	IS_FUNCTION_TYPE,
};

#define IS_OBJECT_MASK 0x7

#define IS_INTEGER_TAG 0x1
#define IS_CHARACTER_TAG 0x2

#define IS_POINTER_P(obj) ((((intptr_t)obj) & IS_OBJECT_MASK) == 0)

#define IS_INTEGER(obj) (((intptr_t)obj) >> 3)
#define IS_INTEGER_P(obj) ((((intptr_t)obj) & IS_INTEGER_TAG) == IS_INTEGER_TAG)

#define IS_NULL(obj) (obj == (ISObject)NULL)
#define IS_OBJECT_PTR(obj) ((void*)obj)

#define IS_FLOAT_P(obj) (IS_POINTER_P(obj) && IS_HEAP_OBJECT_TYPE(obj) == IS_FLOAT_TYPE)
#define IS_CONS_P(obj) (IS_POINTER_P(obj) && IS_HEAP_OBJECT_TYPE(obj) == IS_CONS_TYPE)

#define is_make_integer(v) ((ISObject)(((v) << 3) | IS_INTEGER_TAG))

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
	ISObject value;
	ISObject function;
} ISSymbol;

#define IS_SYMBOL_NAME(obj) (((ISSymbol*)obj)->name)
#define IS_SYMBOL_VALUE(obj) (((ISSymbol*)obj)->value)
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
	ISFuncPtr ptr;
} ISUserFunction;

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

extern ISObject **is_shelter;
extern int is_shelter_used;
extern int is_shelter_size;

extern ISObject *is_symbol_table;
extern int is_symbol_table_size;

extern ISObject is_current_env;

extern ISObject is_symbol_nil;
#define is_nil is_symbol_nil

// error.c
void is_error(const char *);
void is_argc_error(void);
void is_stackoverflow(void);
void is_type_error(ISObject, enum ISType);
void is_undefined_function(ISObject);

// symbol.c
ISObject is_intern(ISObject *);
ISObject is_bool_to_object(bool);
void is_symbol_set_function(ISObject, ISObject);
void is_add_builtin_function(const char *, ISFuncPtr, int, int);
void is_symbol_init(void);

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
void is_cons_f(int);
void is_car_f(int);
void is_cdr_f(int);
void is_cons_init(void);

// shelter.c
int is_shelter_add(ISObject *);
void is_shelter_set_index(int);
void is_shelter_print(void);
void is_shelter_init(void);

// heap.c
void *is_xrealloc(void *, size_t);
void *is_xmalloc(size_t);
void is_gc_enable(void);
void is_gc_disable(void);
void is_heap_init(void);
ISObject is_make_float(double);
ISObject is_make_cons(ISObject *, ISObject *);
ISObject is_make_symbol(ISObject *);
ISObject is_make_string(const char *);
ISObject is_make_user_function(ISFuncPtr);
ISObject is_make_builtin_function(ISFuncPtr, const char *, int, int);
ISObject is_make_closure(ISFuncPtr);

// env.c
void is_env_init(void);
void is_env_extend(int, ...);
ISObject *is_env_get(int i);

// call.c
void is_call(ISObject, int);
void is_funcall_f(int);
void is_call_init(void);

// print.c
void is_print(ISObject, FILE *);
void is_println(ISObject obj);
void is_print_f(int);
void is_print_init(void);

#define IS_UNUSED(v) v __attribute__((unused))

#include "stack.h"

#endif
