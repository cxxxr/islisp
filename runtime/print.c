#include "lisp.h"

static void print_cons(ISObject obj, FILE *fp, bool recursive)
{
	if (!recursive)
		fputs("(", fp);
	is_print(IS_CONS_CAR(obj), fp);
	if (IS_CONS_CDR(obj) == is_nil) {
		fputs(")", fp);
	} else if (!IS_CONS_P(IS_CONS_CDR(obj))) {
		fputs(" . ", fp);
		is_print(IS_CONS_CDR(obj), fp);
		fputs(")", fp);
	} else {
		fputs(" ", fp);
		print_cons(IS_CONS_CDR(obj), fp, true);
	}
}

void is_print(ISObject obj, FILE *fp)
{
	if (IS_INTEGER_P(obj)) {
		fprintf(fp, "%ld", IS_INTEGER(obj));
	} else if (IS_CHARACTER_P(obj)) {
		fprintf(fp, "#\\%c", (char)(IS_CHARACTER(obj)));
	} else {
		switch (IS_HEAP_OBJECT_TYPE(obj)) {
		case IS_FLOAT_TYPE:
			fprintf(fp, "%lf", IS_FLOAT_VALUE(obj));
			break;
		case IS_CONS_TYPE:
			print_cons(obj, fp, false);
			break;
		case IS_SYMBOL_TYPE:
			if (IS_INTEGER_P(IS_SYMBOL_NAME(obj))) {
				fprintf(fp, "#:%d", (int)(IS_INTEGER(IS_SYMBOL_NAME(obj))));
			} else {
				fprintf(fp, "%s", IS_STRING_DATA(IS_SYMBOL_NAME(obj)));
			}
			break;
		case IS_STRING_TYPE:
			fprintf(fp, "\"%s\"", IS_STRING_DATA(obj));
			break;
		case IS_USER_FUNCTION_TYPE:
			fprintf(fp, "#<USER-FUNCTION %s>",
				IS_STRING_DATA(IS_SYMBOL_NAME(IS_USER_FUNCTION_NAME(obj))));
			break;
		case IS_BUILTIN_FUNCTION_TYPE:
			fprintf(fp, "#<BUILTIN-FUNCTION %s>",
				IS_BUILTIN_FUNCTION_NAME(obj));
			break;
		case IS_CLOSURE_TYPE:
			fprintf(fp, "#<CLOSURE %p>", IS_CLOSURE_PTR(obj));
			break;
		case IS_INPUT_STREAM_TYPE:
			fprintf(fp, "#<INPUT-STREAM %p>", IS_STREAM_PTR(obj));
			break;
		case IS_OUTPUT_STREAM_TYPE:
			fprintf(fp, "#<OUTPUT-STREAM %p>", IS_STREAM_PTR(obj));
			break;
		case IS_VECTOR_TYPE:
			fputs("#(", fp);
			for (int i = 0; i < IS_VECTOR_LENGTH(obj); i++) {
				is_print(IS_VECTOR_DATA(obj)[i], fp);
				fputs(" ", fp);
			}
			fputs(")", fp);
			break;
		default:
			printf("unknown type: %d\n", IS_HEAP_OBJECT_TYPE(obj));
			abort();
		}
	}
}

void is_println(ISObject obj)
{
	is_print(obj, stdout);
	puts("");
}

void is_print_f(int IS_UNUSED(argc))
{
	is_print(is_stack_peek(1), stdout);
	puts("");
}

void is_print_init(void)
{
	is_add_builtin_function("PRINT", is_print_f, 1, 1);
}
