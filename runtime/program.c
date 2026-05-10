#include "object.h"

#define CONS_BAR 0
word_t cons_BAR(word_t* args)
{
  word_t anon1;
  anon1 = constructor_to_word(make_constructor(CONS_BAR,args,2));
  return anon1;
}
word_t test_cons_BAR(word_t* args)
{
  word_t anon2;
  anon2 = int_to_word(test_constructor(*args,CONS_BAR));
  return anon2;
}
#define CONS_FOO 3
word_t cons_FOO(word_t* args)
{
  word_t anon4;
  anon4 = constructor_to_word(make_constructor(CONS_FOO,args,1));
  return anon4;
}
word_t test_cons_FOO(word_t* args)
{
  word_t anon5;
  anon5 = int_to_word(test_constructor(*args,CONS_FOO));
  return anon5;
}
#define CONS_Node 6
word_t cons_Node(word_t* args)
{
  word_t anon7;
  anon7 = constructor_to_word(make_constructor(CONS_Node,args,2));
  return anon7;
}
word_t test_cons_Node(word_t* args)
{
  word_t anon8;
  anon8 = int_to_word(test_constructor(*args,CONS_Node));
  return anon8;
}
word_t fn_arange(word_t*);

word_t fn_bar(word_t*);

word_t fn_cons(word_t*);

word_t fn_fibo(word_t*);

word_t fn_fibo_caller(word_t*);

word_t fn_fibo_wrapper(word_t*);

word_t fn_foo(word_t*);

word_t fn_is_cons(word_t*);

word_t fn_is_nil(word_t*);

word_t fn_map(word_t*);

word_t fn_member(word_t*);

word_t fn_nil(word_t*);

word_t fn_one(word_t*);

word_t fn_sum(word_t*);

word_t fn_total(word_t*);

word_t fn_zero(word_t*);

word_t fun0(word_t*);

word_t fun1(word_t*);

word_t fn_arange(word_t* args)
{
  word_t anon9;
  anon9 = args[0];
  word_t anon10 = int_to_word(0);
  word_t anon11[2] = { anon9,anon10 };
  word_t anon12 = int_eq(anon11);;
  word_t anon13;
  if (word_to_int(anon12))
  {
    word_t anon14 = fn_nil(NULL);
    anon13 = anon14;
  }
  else
  {
    word_t anon15 = int_to_word(0);
    word_t anon16;
    anon16 = closure_to_word(make_closure((word_t)fun1,1,0));
    word_t anon17 = int_to_word(1);
    word_t anon18[2] = { anon9,anon17 };
    word_t anon19 = int_sub(anon18);;
    word_t anon20[1] = { anon19 };
    word_t anon21 = fn_arange(anon20);;
    word_t anon22[2] = { anon16,anon21 };
    word_t anon23 = fn_map(anon22);;
    word_t anon24[2] = { anon15,anon23 };
    word_t anon25 = fn_cons(anon24);;
    anon13 = anon25;
  }
  return anon13;
}


word_t fn_bar(word_t* args)
{
  word_t anon26;
  anon26 = args[0];
  word_t anon27 = int_to_word(8);
  word_t anon28 = int_to_word(42);
  word_t anon29[2] = { anon27,anon28 };
  word_t anon30 = cons_BAR(anon29);;
  return anon30;
}


word_t fn_cons(word_t* args)
{
  word_t anon31;
  word_t anon32;
  anon31 = args[0];
  anon32 = args[1];
  word_t anon33[2] = { anon31,anon32 };
  word_t anon34 = cons_Node(anon33);;
  return anon34;
}


word_t fn_fibo(word_t* args)
{
  word_t anon35;
  anon35 = args[0];
  word_t anon36 = int_to_word(0);
  word_t anon37[2] = { anon35,anon36 };
  word_t anon38 = int_eq(anon37);;
  word_t anon39;
  if (word_to_int(anon38))
  {
    word_t anon40 = fn_zero(NULL);
    anon39 = anon40;
  }
  else
  {
    word_t anon41 = int_to_word(1);
    word_t anon42[2] = { anon35,anon41 };
    word_t anon43 = int_eq(anon42);;
    word_t anon44;
    if (word_to_int(anon43))
    {
      word_t anon45 = fn_one(NULL);
      anon44 = anon45;
    }
    else
    {
      word_t anon46 = fn_fibo_wrapper(NULL);
      word_t anon47 = int_to_word(1);
      word_t anon48[2] = { anon35,anon47 };
      word_t anon49 = int_sub(anon48);;
      word_t anon50[1] = { anon49 };
      word_t anon51 = apply_closure(word_to_closure(anon46),anon50,1);;
      word_t anon52 = fn_fibo_wrapper(NULL);
      word_t anon53 = int_to_word(2);
      word_t anon54[2] = { anon35,anon53 };
      word_t anon55 = int_sub(anon54);;
      word_t anon56[1] = { anon55 };
      word_t anon57 = apply_closure(word_to_closure(anon52),anon56,1);;
      word_t anon58 = int_to_word(0);
      word_t anon59[2] = { anon51,anon58 };
      word_t anon60 = extract_constructor(anon59);;
      word_t anon61 = int_to_word(0);
      word_t anon62[2] = { anon57,anon61 };
      word_t anon63 = extract_constructor(anon62);;
      word_t anon64[2] = { anon60,anon63 };
      word_t anon65 = int_add(anon64);;
      anon44 = anon65;
    }
    anon39 = anon44;
  }
  return anon39;
}


word_t fn_fibo_caller(word_t* args)
{
  word_t anon66;
  anon66 = args[0];
  word_t anon67[1] = { anon66 };
  word_t anon68 = fn_fibo(anon67);;
  word_t anon69[1] = { anon68 };
  word_t anon70 = cons_FOO(anon69);;
  word_t anon71 = int_to_word(0);
  word_t anon72[2] = { anon70,anon71 };
  word_t anon73 = extract_constructor(anon72);;
  return anon73;
}


word_t fn_fibo_wrapper(word_t* args)
{
  word_t anon74;
  anon74 = closure_to_word(make_closure((word_t)fun0,1,0));
  return anon74;
}


word_t fn_foo(word_t* args)
{
  word_t anon75;
  anon75 = args[0];
  word_t anon76[1] = { anon75 };
  word_t anon77 = cons_FOO(anon76);;
  return anon77;
}


word_t fn_is_cons(word_t* args)
{
  word_t anon78;
  anon78 = args[0];
  word_t anon79[1] = { anon78 };
  word_t anon80 = test_cons_Node(anon79);;
  return anon80;
}


word_t fn_is_nil(word_t* args)
{
  word_t anon81;
  anon81 = args[0];
  word_t anon82[1] = { anon81 };
  word_t anon83 = test_cons_Node(anon82);;
  word_t anon84[1] = { anon83 };
  word_t anon85 = int_not(anon84);;
  return anon85;
}


word_t fn_map(word_t* args)
{
  word_t anon86;
  word_t anon87;
  anon86 = args[0];
  anon87 = args[1];
  word_t anon88[1] = { anon87 };
  word_t anon89 = fn_is_nil(anon88);;
  word_t anon90;
  if (word_to_int(anon89))
  {
    anon90 = anon87;
  }
  else
  {
    word_t anon91 = int_to_word(0);
    word_t anon92[2] = { anon87,anon91 };
    word_t anon93 = extract_constructor(anon92);;
    word_t anon94[1] = { anon93 };
    word_t anon95 = apply_closure(word_to_closure(anon86),anon94,1);;
    word_t anon96 = int_to_word(1);
    word_t anon97[2] = { anon87,anon96 };
    word_t anon98 = extract_constructor(anon97);;
    word_t anon99[2] = { anon86,anon98 };
    word_t anon100 = fn_map(anon99);;
    word_t anon101[2] = { anon95,anon100 };
    word_t anon102 = fn_cons(anon101);;
    anon90 = anon102;
  }
  return anon90;
}


word_t fn_member(word_t* args)
{
  word_t anon103;
  word_t anon104;
  anon103 = args[0];
  anon104 = args[1];
  word_t anon105[1] = { anon104 };
  word_t anon106 = fn_is_cons(anon105);;
  word_t anon107;
  if (word_to_int(anon106))
  {
    word_t anon108 = int_to_word(0);
    word_t anon109[2] = { anon104,anon108 };
    word_t anon110 = extract_constructor(anon109);;
    word_t anon111[2] = { anon103,anon110 };
    word_t anon112 = int_eq(anon111);;
    word_t anon113 = int_to_word(1);
    word_t anon114[2] = { anon104,anon113 };
    word_t anon115 = extract_constructor(anon114);;
    word_t anon116[2] = { anon103,anon115 };
    word_t anon117 = fn_member(anon116);;
    word_t anon118[2] = { anon112,anon117 };
    word_t anon119 = int_or(anon118);;
    anon107 = anon119;
  }
  else
  {
    word_t anon120 = int_to_word(0);
    anon107 = anon120;
  }
  return anon107;
}


word_t fn_nil(word_t* args)
{
  word_t anon121 = int_to_word(0);
  return anon121;
}


word_t fn_one(word_t* args)
{
  word_t anon122 = int_to_word(1);
  return anon122;
}


word_t fn_sum(word_t* args)
{
  word_t anon123;
  anon123 = args[0];
  word_t anon124[1] = { anon123 };
  word_t anon125 = fn_is_nil(anon124);;
  word_t anon126;
  if (word_to_int(anon125))
  {
    word_t anon127 = int_to_word(0);
    anon126 = anon127;
  }
  else
  {
    word_t anon128 = int_to_word(0);
    word_t anon129[2] = { anon123,anon128 };
    word_t anon130 = extract_constructor(anon129);;
    word_t anon131 = int_to_word(1);
    word_t anon132[2] = { anon123,anon131 };
    word_t anon133 = extract_constructor(anon132);;
    word_t anon134[1] = { anon133 };
    word_t anon135 = fn_sum(anon134);;
    word_t anon136[2] = { anon130,anon135 };
    word_t anon137 = int_add(anon136);;
    anon126 = anon137;
  }
  return anon126;
}


word_t fn_total(word_t* args)
{
  word_t anon138 = int_to_word(4002);
  word_t anon139[1] = { anon138 };
  word_t anon140 = fn_arange(anon139);;
  word_t anon141[1] = { anon140 };
  word_t anon142 = fn_sum(anon141);;
  return anon142;
}


word_t fn_zero(word_t* args)
{
  word_t anon143 = int_to_word(0);
  return anon143;
}


word_t fun0(word_t* args)
{
  word_t anon144;
  anon144 = args[0];
  word_t anon145[1] = { anon144 };
  word_t anon146 = fn_fibo(anon145);;
  word_t anon147[1] = { anon146 };
  word_t anon148 = cons_FOO(anon147);;
  return anon148;
}


word_t fun1(word_t* args)
{
  word_t anon149;
  anon149 = args[0];
  word_t anon150 = int_to_word(1);
  word_t anon151[2] = { anon149,anon150 };
  word_t anon152 = int_add(anon151);;
  return anon152;
}

#include <stdio.h>
#include <time.h>
word_t allocation_buffer[100000000];
word_t test_fibo(word_t x) { if (x < 2) return x; return test_fibo(x-1)+test_fibo(x-2); }
int main() {
  register word_t* base_sp asm("sp");
  alloc_init(base_sp, allocation_buffer, 100000);
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
