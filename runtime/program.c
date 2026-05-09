#include "object.h"

word_t fn_fibo(word_t*);

word_t fn_fibo_caller(word_t*);

word_t fn_fibo_wrapper(word_t*);

word_t fun0(word_t*);

word_t fun1(word_t*);

word_t fn_fibo(word_t* args)
{
  word_t anon0;
  anon0 = args[0];
  word_t anon1;
  anon1 = int_to_word(0);
  word_t anon2;
  {
    word_t* buf = alloca(sizeof(word_t)*2);
    buf[0] = anon0;
    buf[1] = anon1;
    word_t (*fun)(word_t*) = (word_t (*)(word_t*))int_neq;
    anon2 = fun(buf);
  }
  word_t anon3;
  if (word_to_int(anon2))
  {
    word_t anon4;
    anon4 = int_to_word(1);
    word_t anon5;
    {
      word_t* buf = alloca(sizeof(word_t)*2);
      buf[0] = anon0;
      buf[1] = anon4;
      word_t (*fun)(word_t*) = (word_t (*)(word_t*))int_sub;
      anon5 = fun(buf);
    }
    word_t anon6;
    anon6 = int_to_word(0);
    word_t anon7;
    {
      word_t* buf = alloca(sizeof(word_t)*2);
      buf[0] = anon5;
      buf[1] = anon6;
      word_t (*fun)(word_t*) = (word_t (*)(word_t*))int_neq;
      anon7 = fun(buf);
    }
    word_t anon8;
    if (word_to_int(anon7))
    {
      word_t anon9;
      anon9 = int_to_word(1);
      word_t anon10;
      {
        word_t* buf = alloca(sizeof(word_t)*2);
        buf[0] = anon0;
        buf[1] = anon9;
        word_t (*fun)(word_t*) = (word_t (*)(word_t*))int_sub;
        anon10 = fun(buf);
      }
      word_t anon11;
      {
        word_t* buf = alloca(sizeof(word_t)*1);
        buf[0] = anon10;
        word_t (*fun)(word_t*) = (word_t (*)(word_t*))fn_fibo_caller;
        anon11 = fun(buf);
      }
      word_t anon12;
      anon12 = int_to_word(2);
      word_t anon13;
      {
        word_t* buf = alloca(sizeof(word_t)*2);
        buf[0] = anon0;
        buf[1] = anon12;
        word_t (*fun)(word_t*) = (word_t (*)(word_t*))int_sub;
        anon13 = fun(buf);
      }
      word_t anon14;
      {
        word_t* buf = alloca(sizeof(word_t)*1);
        buf[0] = anon13;
        word_t (*fun)(word_t*) = (word_t (*)(word_t*))fn_fibo_caller;
        anon14 = fun(buf);
      }
      word_t anon15;
      {
        word_t* buf = alloca(sizeof(word_t)*2);
        buf[0] = anon11;
        buf[1] = anon14;
        word_t (*fun)(word_t*) = (word_t (*)(word_t*))int_add;
        anon15 = fun(buf);
      }
      anon8 = anon15;
    }
    else
    {
      word_t anon16;
      anon16 = int_to_word(1);
      anon8 = anon16;
    }
    anon3 = anon8;
  }
  else
  {
    word_t anon17;
    anon17 = int_to_word(0);
    anon3 = anon17;
  }
  return anon3;
}


word_t fn_fibo_caller(word_t* args)
{
  word_t anon18;
  anon18 = args[0];
  word_t anon19;
  anon19 = closure_to_word(make_closure((word_t)fun1,1,0));
  word_t anon20;
  {
    word_t* buf = alloca(sizeof(word_t)*1);
    buf[0] = anon18;
    anon20 = apply_closure(word_to_closure(anon19),buf,1);
  }
  return anon20;
}


word_t fn_fibo_wrapper(word_t* args)
{
  word_t anon21;
  anon21 = closure_to_word(make_closure((word_t)fun0,2,0));
  return anon21;
}


word_t fun0(word_t* args)
{
  word_t anon22;
  word_t anon23;
  anon22 = args[0];
  anon23 = args[1];
  word_t anon24;
  {
    word_t* buf = alloca(sizeof(word_t)*1);
    buf[0] = anon22;
    word_t (*fun)(word_t*) = (word_t (*)(word_t*))fn_fibo;
    anon24 = fun(buf);
  }
  return anon24;
}


word_t fun1(word_t* args)
{
  word_t anon25;
  anon25 = args[0];
  word_t anon26;
  anon26 = closure_to_word(make_closure((word_t)fn_fibo_wrapper,0,0));
  word_t anon27;
  {
    word_t* buf = alloca(sizeof(word_t)*2);
    buf[0] = anon25;
    buf[1] = anon25;
    anon27 = apply_closure(word_to_closure(anon26),buf,2);
  }
  return anon27;
}

#include <stdio.h>
#include <time.h>
word_t allocation_buffer[100000000];word_t test_fibo(word_t x) { if (x < 2) return x; return test_fibo(x-1)+test_fibo(x-2); }
int main() {
  register word_t* base_sp asm("sp");  alloc_init(base_sp, allocation_buffer, 100000);
  word_t buf;
  buf = int_to_word(32);
  clock_t t0 = clock();
  printf("fibo(32): %ld\n", test_fibo(32));  clock_t t1 = clock();
  printf("fibo(32): %ld\n", word_to_int(fn_fibo_caller(&buf)));
  clock_t t2 = clock();
  double diff0 = (double)(t1-t0) / CLOCKS_PER_SEC;
  double diff1 = (double)(t2-t1) / CLOCKS_PER_SEC;
  printf("baseline: %f compiled: %f\n", diff0, diff1);
  unsigned gc_calls;
  double gc_time;
  alloc_stats(&gc_calls, &gc_time);
  printf("gc calls: %d gc time: %f\n", gc_calls, gc_time);
}