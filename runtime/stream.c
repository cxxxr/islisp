#include "lisp.h"

static ISObject is_standard_input;
static ISObject is_standard_output;
static ISObject is_error_output;

void is_read_char(ISObject input_stream, ISObject eos_error_p, ISObject eos_value)
{
	if (!IS_INPUT_STREAM_P(input_stream))
		is_type_error(input_stream, IS_INPUT_STREAM_TYPE);
	switch (IS_HEAP_OBJECT_TYPE(input_stream)) {
		case IS_INPUT_STREAM_TYPE:
		{
			int c = fgetc(IS_STREAM_PTR(input_stream));
			if (c != EOF) {
				is_stack_push(is_make_character(c));
			} else if (eos_error_p == is_nil) {
				is_stack_push(eos_value);
			} else {
				is_not_an_input_stream();
			}
			break;
		}
		case IS_INPUT_STRING_STREAM_TYPE:
			is_error("unsupported string stream");
			break;
		default:
			break;
	}
}

void is_read_char_f(int argc)
{
	ISObject input_stream = is_standard_input;
	ISObject eos_error_p = is_symbol_t;
	ISObject eos_value = is_nil;

	switch (argc) {
	case 0:
		break;
	case 1:
		input_stream = is_stack_peek(1);
		break;
	case 2:
		input_stream = is_stack_peek(2);
		eos_error_p = is_stack_peek(1);
		break;
	case 3:
		input_stream = is_stack_peek(3);
		eos_error_p = is_stack_peek(2);
		eos_value = is_stack_peek(1);
		break;
	}

	is_read_char(input_stream, eos_error_p, eos_value);
	is_stack_nip(argc);
}

void is_standard_output_f(int IS_UNUSED(argc))
{
	is_stack_push(is_standard_output);
}

void is_standard_input_f(int IS_UNUSED(argc))
{
	is_stack_push(is_standard_input);
}

void is_error_output_f(int IS_UNUSED(argc))
{
	is_stack_push(is_error_output);
}

void is_stream_init(void)
{
	is_standard_input = is_make_input_stream(stdin);
	is_standard_output = is_make_output_stream(stdout);
	is_error_output = is_make_output_stream(stderr);
	is_add_builtin_function("READ-CHAR", is_read_char_f, 0, 3);
	is_add_builtin_function("STANDARD-OUTPUT", is_standard_output_f, 0, 0);
	is_add_builtin_function("STANDARD-INPUT", is_standard_input_f, 0, 0);
}
