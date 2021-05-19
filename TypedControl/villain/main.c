#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include "types.h"
#include "runtime.h"

void print_result(int64_t);
void print_char(int64_t);
int64_t *heap;

int main(int argc, char** argv) {

  heap = malloc(8 * heap_size);
  int64_t result = entry(heap);
  print_result(result);
  free(heap);
  return 0;
}

void print_result(int64_t result) {
  if (int_type_tag == (int_type_mask & result)) {
    printf("%" PRId64 "\n", result >> int_shift);
  } else if (char_type_tag == (char_type_mask & result)) {
    print_char(result);
  } 
   else {
    switch (result) {
    case val_true:
      printf("#t\n"); break;
    case val_false:
      printf("#f\n"); break;
    case val_eof:
      printf("#<eof>\n"); break;
    case val_void:
      /* nothing */ break;
    }
  }  
}
