#include "object.h"

#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include <time.h>

static unsigned num_gc = 0;
static clock_t time_gc = 0;

static word_t* base_stack_pointer = NULL;

static word_t *heap_buffer = NULL;
static unsigned heap_size = 0;

static word_t* heap_headers = NULL;
static word_t* heap_marks = NULL;

#define NIL NULL

#define WORD_WIDTH (sizeof(word_t)*8)

static bool is_header(word_t* ptr) {
  word_t diff = ptr - heap_buffer;
  word_t index = diff % WORD_WIDTH;
  return heap_headers[diff / WORD_WIDTH] >> index & 1;
}

static void set_header(word_t* ptr) {
  word_t diff = ptr - heap_buffer;
  word_t index = diff % WORD_WIDTH;
  heap_headers[diff / WORD_WIDTH] |= (word_t)1 << index;
}

static void clear_header(word_t* ptr) {
  word_t diff = ptr - heap_buffer;
  word_t index = diff % WORD_WIDTH;
  heap_headers[diff / WORD_WIDTH] &= ~((word_t)1 << index);
}

static bool is_marked(word_t* ptr) {
  word_t diff = ptr - heap_buffer;
  word_t index = diff % WORD_WIDTH;
  return heap_marks[diff / WORD_WIDTH] >> index & 1;
}

static void set_mark(word_t* ptr) {
  word_t diff = ptr - heap_buffer;
  word_t index = diff % WORD_WIDTH;
  heap_marks[diff / WORD_WIDTH] |= (word_t)1 << index;
}

static void clear_mark(word_t* ptr) {
  word_t diff = ptr - heap_buffer;
  word_t index = diff % WORD_WIDTH;
  heap_marks[diff / WORD_WIDTH] &= ~((word_t)1 << index);
}

bool is_valid_pointer(word_t x) {
  return word_is_pointer(x) && heap_buffer <= (word_t*)x && (word_t*)x < heap_buffer + heap_size;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Contains informations about the current heap hole:
///   - heap_hole_len is the available length of the current hole from heap_hole_ptr
///   - heap_next_hole is a pointer to the next hole in the list, or NULL we used all the available
///     space
///
/// When the allocator used all the space available it must go to the next hole using heap_next_hole
/// and read the following values:
///   - heap_hole_len using `word_to_int(heap_next_hole[1])`
///   - heap_next_hole using `word_to_ptr(heap_next_hole[0])`
///////////////////////////////////////////////////////////////////////////////////////////////////
static word_t* heap_hole_ptr = NULL;
static unsigned heap_hole_len = 0;
static word_t* heap_next_hole = NULL;

word_t* alloc_get_base() { return heap_buffer; }

void alloc_init(word_t* base_sp, word_t* buf, unsigned len) {
  unsigned headers_len = (len + WORD_WIDTH-1) / WORD_WIDTH;

  heap_headers = buf;
  buf += headers_len;
  len -= headers_len;
  heap_marks = buf;
  buf += headers_len;
  len -= headers_len;
  //heap_headers = malloc(headers_len * sizeof(word_t));
  //heap_marks = malloc(headers_len * sizeof(word_t));

  memset(heap_headers, 0, sizeof(word_t)*headers_len);
  memset(heap_marks, 0, sizeof(word_t)*headers_len);

  base_stack_pointer = base_sp;

  heap_buffer = buf;
  heap_size = len;

  heap_hole_ptr = buf;
  heap_hole_len = len;
  heap_next_hole = NIL;

  for (int i=0; i < len; i++) {
    buf[i] = int_to_word(0);
  }
}

void mark(word_t word) {
  if (!word_is_pointer(word)) return;
  word_t* ptr = (word_t*)(word);

  if (ptr < heap_buffer || ptr >= heap_buffer + heap_size) return;
  if (!is_header(ptr)) return;
  if (is_marked(ptr)) return;

  // We have the confirmation that the pointer to a valid header in the heap, so
  // it is safe to write
  word_t header = ptr[0];
  set_mark(ptr);

  if (word_to_header(header) == CLOSURE_HEADER) {
    closure_t* closure = word_to_closure(word);

    unsigned len = word_to_int(closure->len);

    for (int i=0; i < len; i++) {
      mark(closure->buf[i]);
    }
  } else if (word_to_header(header) == CONSTRUCTOR_HEADER) {
    constructor_t* constructor = word_to_constructor(word);

    unsigned len = word_to_int(constructor->len);

    for (int i=0; i < len; i++) {
      mark(constructor->buf[i]);
    }
  } else {
    assert(false);
  }
}

static void sweep() {
  // Clears all the headers, reset the non-free locations later
  for (int i=0; i < (heap_size+WORD_WIDTH-1) / WORD_WIDTH; i++) heap_headers[i] = 0;

  word_t* ptr = heap_buffer;

  word_t* region_ptr = NIL;
  word_t region_len = 0;

  while (ptr < heap_buffer + heap_size) {
    unsigned len = 0;
    while (ptr+len < heap_buffer + heap_size) {
      if (is_marked(ptr+len)) break;
      ptr[len] = int_to_word(0);
      len++;
    }

    // `ptr` correspond to an allocated region
    if (len == 0) {
      set_header(ptr);

      if (word_to_header(ptr[0]) == CLOSURE_HEADER) {
        word_t closure_size = word_to_int(((closure_t*)(ptr))->len);
        ptr += (sizeof(closure_t) / sizeof(word_t)) + closure_size;
        continue;
      } else if (word_to_header(ptr[0]) == CONSTRUCTOR_HEADER) {
        word_t constructor_size = word_to_int(((constructor_t*)(ptr))->len);
        ptr += (sizeof(constructor_t) / sizeof(word_t)) + constructor_size;
        continue;
      } else {
        assert(false);
      }
    }

    // Skip: not enough space for the region header
    if (len == 1) {
      ptr++;
      continue;
    }

    // Configure the next hole
    heap_hole_len = len;
    heap_hole_ptr = ptr;
    heap_next_hole = region_ptr;

    ptr[0] = (word_t)region_ptr;
    ptr[1] = (word_t)len;
    region_ptr = ptr;
    region_len = len;
    ptr += len;
  }

  // Clear all the visited headers, prepare for the next GC call
  for (int i=0; i < (heap_size+WORD_WIDTH-1) / WORD_WIDTH; i++) heap_marks[i] = 0;
}

#include <setjmp.h>

static void gc() {
  time_gc -= clock();
  num_gc++;

  jmp_buf regs;
  word_t* regs_ptr = (word_t*) &regs;
  for (int i=0; i < sizeof(regs)/sizeof(word_t); i++) regs_ptr[i] = 0;
  setjmp(regs);

  register word_t* sp asm("sp");
  word_t* stack = sp;

  while (stack <= base_stack_pointer) {
    mark(*stack);
    stack++;
  }

  sweep();

  time_gc += clock();
}

#include <stdio.h>
word_t* alloc_words(unsigned size) {
  while (1) {
    if (size <= heap_hole_len) {
      word_t* ptr = heap_hole_ptr;
      heap_hole_ptr += size;
      heap_hole_len -= size;
      set_header(ptr);
      return ptr;
    }

    if (heap_next_hole != NIL) {
      heap_hole_ptr = heap_next_hole;
      heap_next_hole = (word_t*)(heap_hole_ptr[0]);
      heap_hole_len = heap_hole_ptr[1];
      continue;
    }

    gc();
  }
}

void alloc_stats(unsigned* gc_calls, double* gc_time) {
  *gc_time = (double)time_gc / CLOCKS_PER_SEC;
  *gc_calls = num_gc;
}
