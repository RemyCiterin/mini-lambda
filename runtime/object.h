#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <alloca.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

typedef size_t word_t;

#define PTR_MASK 0b00 // Pointer
#define HDR_MASK 0b10 // Header of an allocated object

bool is_valid_pointer(word_t x);

inline bool word_is_int(word_t x) { return (x&1) == 1; }
inline word_t int_to_word(word_t x) { return (x << 1) | 1; }
inline word_t word_to_int(word_t x) { return x >> 1; }

inline bool word_is_pointer(word_t x) { return x % sizeof(word_t) == 0; }
inline word_t ptr_to_word(word_t* x) { return (word_t)x; }
inline word_t* word_to_ptr(word_t x) {
  assert(is_valid_pointer(x));
  return (word_t*)x; }

typedef word_t hdr_t;
inline bool word_is_header(word_t x) { return (x&3) == HDR_MASK; }
inline word_t header_to_word(hdr_t x) { return (x << 2) | HDR_MASK; }
inline hdr_t word_to_header(word_t x) { return x >> 2; }

// Allocate a buffer of memory
word_t* alloc_words(unsigned num_words);

// Initialise the memory allocator
void alloc_init(word_t* stack_pointer, word_t* buffer, unsigned num_words);

// Get GC statistics
void alloc_stats(unsigned* gc_calls, double* gc_time);

/// Headers of allocated objects
#define CLOSURE_HEADER 128

#define CONSTRUCTOR_HEADER 127

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Builtin integer functions
///////////////////////////////////////////////////////////////////////////////////////////////////

inline word_t int_add(word_t* args) {
  return int_to_word(word_to_int(args[0]) + word_to_int(args[1])); }
inline word_t int_mul(word_t* args) {
  return int_to_word(word_to_int(args[0]) * word_to_int(args[1])); }
inline word_t int_sub(word_t* args) {
  return int_to_word(word_to_int(args[0]) - word_to_int(args[1])); }
inline word_t int_div(word_t* args) {
  return int_to_word(word_to_int(args[0]) / word_to_int(args[1])); }
inline word_t int_rem(word_t* args) {
  return int_to_word(word_to_int(args[0]) % word_to_int(args[1])); }
inline word_t int_neg(word_t* args) {
  return int_to_word(-word_to_int(args[0])); }
inline word_t int_not(word_t* args) {
  return int_to_word(!word_to_int(args[0])); }
inline word_t int_bnot(word_t* args) {
  return int_to_word(~word_to_int(args[0])); }
inline word_t int_and(word_t* args) {
  return int_to_word(word_to_int(args[0]) && word_to_int(args[1])); }
inline word_t int_or(word_t* args) {
  return int_to_word(word_to_int(args[0]) || word_to_int(args[1])); }
inline word_t int_eq(word_t* args) {
  return int_to_word(word_to_int(args[0]) == word_to_int(args[1])); }
inline word_t int_lt(word_t* args) {
  return int_to_word(word_to_int(args[0]) < word_to_int(args[1])); }
inline word_t int_gt(word_t* args) {
  return int_to_word(word_to_int(args[0]) > word_to_int(args[1])); }
inline word_t int_leq(word_t* args) {
  return int_to_word(word_to_int(args[0]) <= word_to_int(args[1])); }
inline word_t int_geq(word_t* args) {
  return int_to_word(word_to_int(args[0]) >= word_to_int(args[1])); }
inline word_t int_neq(word_t* args) {
  return int_to_word(word_to_int(args[0]) != word_to_int(args[1])); }
inline word_t int_band(word_t* args) {
  return int_to_word(word_to_int(args[0]) & word_to_int(args[1])); }
inline word_t int_bor(word_t* args) {
  return int_to_word(word_to_int(args[0]) | word_to_int(args[1])); }
inline word_t int_xor(word_t* args) {
  return int_to_word(word_to_int(args[0]) ^ word_to_int(args[1])); }

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Allocated objects: constructors
///////////////////////////////////////////////////////////////////////////////////////////////////
typedef struct {
  word_t header;
  word_t tag;
  word_t len;
  word_t buf[0];
} constructor_t;

word_t* alloc_get_base();

inline bool word_is_constructor(word_t x) {
  return word_is_pointer(x) && *word_to_ptr(x) == header_to_word(CONSTRUCTOR_HEADER); }
inline word_t constructor_to_word(constructor_t* x) { return ptr_to_word((word_t*)x); }
inline constructor_t* word_to_constructor(word_t x) { return (constructor_t*)word_to_ptr(x); }

inline constructor_t* make_constructor(unsigned tag, word_t* buf, word_t len) {
  constructor_t* obj = (constructor_t*)alloc_words(sizeof(constructor_t)/sizeof(word_t) + len);
  obj->header = header_to_word(CONSTRUCTOR_HEADER);
  memcpy(obj->buf, buf, len*sizeof(word_t));
  obj->tag = int_to_word(tag);
  obj->len = int_to_word(len);
  return obj;
}


inline word_t extract_constructor(word_t *args) {
  constructor_t* obj = word_to_constructor(args[0]);
  return obj->buf[word_to_int(args[1])];
}

inline bool test_constructor(word_t arg, unsigned tag) {
  if (!word_is_constructor(arg)) return 0;
  constructor_t* obj = word_to_constructor(arg);
  return word_to_int(obj->tag) == tag;
}

inline unsigned constructor_tag(word_t constructor) {
  constructor_t* obj = word_to_constructor(constructor);
  return obj->tag;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Allocated objects: closures
///////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct {
  word_t header;
  word_t len;
  word_t fun;
  word_t arity;
  word_t buf[0];
} closure_t;

//inline bool word_is_closure(word_t x) {
//  return word_is_pointer(x) && *word_to_ptr(x) == header_to_word(CLOSURE_HEADER); }
inline word_t closure_to_word(closure_t* x) { return ptr_to_word((word_t*)x); }
inline closure_t* word_to_closure(word_t x) {
  assert(is_valid_pointer(x));
  return (closure_t*)word_to_ptr(x); }

inline closure_t* make_closure(word_t fun, word_t arity, word_t len) {
  closure_t* obj = (closure_t*)alloc_words(sizeof(closure_t)/sizeof(word_t) + len);
  obj->header = header_to_word(CLOSURE_HEADER);
  obj->arity = int_to_word(arity);
  obj->len = int_to_word(len);
  obj->fun = fun;
  return obj;
}

word_t apply_closure(closure_t* f, word_t* args, word_t len);
