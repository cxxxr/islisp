#include "lisp.h"

ISObject **is_shelter = NULL;
int is_shelter_used = 0;
int is_shelter_size;

static void shelter_resize(int size)
{
	is_shelter_size = size;
	is_shelter = is_xrealloc(is_shelter, sizeof(ISObject *) * is_shelter_size);
}

int is_shelter_add(ISObject *ptr)
{
	if (!IS_POINTER_P(*ptr)) return is_shelter_used;

	if (is_shelter_used >= is_shelter_size) {
		shelter_resize(is_shelter_size * 2);
	}
	is_shelter[is_shelter_used++] = ptr;
	return is_shelter_used - 1;
}

void is_shelter_set_index(int i)
{
	is_shelter_used = i;
}

void is_shelter_print(void)
{
	for (int i = 0; i < is_shelter_used; i++) {
		is_println(*is_shelter[i]);
	}
}

void is_shelter_init(void)
{
	shelter_resize(10);
}
