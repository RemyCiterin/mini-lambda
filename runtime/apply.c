#include "object.h"

word_t apply_closure(closure_t* f, word_t* args, word_t len) {
  word_t f_arity = word_to_int(f->arity);
  word_t f_len = word_to_int(f->len);

  // We have the exact number of arguments: perform a call to f
  if (f_len == 0 && len == f_arity) {
    word_t (*fun)(word_t*) = (word_t (*)(word_t*))f->fun;
    return fun(args);
  }

  // The arguments are not enough to generate a function call,
  // create a new closure with the concatenation of the
  // arguments
  if (f_len + len < f_arity) {
    closure_t* ret = make_closure(f->fun, f_arity, f_len + len);
    for (int i=0; i < len; i++) ret->buf[i+f_len] = args[i];
    for (int i=0; i < f_len; i++) ret->buf[i] = f->buf[i];
    return (word_t)ret;
  }

  // We have the exact number of arguments: perform a call to f
  if (f_len + len == f_arity) {
    word_t* f_args = (word_t*)alloca(sizeof(word_t*) * f_arity);
    for (int i=0; i < len; i++) f_args[i+f_len] = args[i];
    for (int i=0; i < f_len; i++) f_args[i] = f->buf[i];
    word_t (*fun)(word_t*) = (word_t (*)(word_t*))f->fun;
    return fun(f_args);
  }

  closure_t* new_f = word_to_closure(apply_closure(f, args, f_arity - f_len));
  return apply_closure(new_f, &args[f_arity - f_len], len + f_len - f_arity);
}
