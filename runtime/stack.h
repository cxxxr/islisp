#ifndef STACK_H
#define STACK_H

#include <stdbool.h>

#define IS_STACK_SIZE 10000

EXTERN ISObject is_stack[IS_STACK_SIZE];
EXTERN ISObject *is_stack_top, *is_stack_low, *is_stack_high;

void is_stack_init(void);
void is_stack_push(ISObject);
ISObject *is_stack_pop_ptr(void);
ISObject is_stack_pop(void);
ISObject *is_stack_peek_ptr(int);
ISObject is_stack_peek(int offset);
void is_stack_nip(int);
bool is_stack_top_null(void);
void is_stack_drop(int);
void is_stack_drop_push(int, ISObject);
void is_stack_change_tos(ISObject);
void is_stack_build_list(int);
void is_stack_print(void);
void is_stack_print_f(int);
int is_stack_get_sp(void);

#endif
