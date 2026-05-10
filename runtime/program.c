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
  word_t anon10;
  anon10 = int_to_word(0);
  word_t anon11;
  {
    word_t buf[2] = { anon9,anon10 };
    anon11 = int_eq(buf);
  }
  word_t anon12;
  if (word_to_int(anon11))
  {
    word_t anon13;
    anon13 = fn_nil(NULL);
    anon12 = anon13;
  }
  else
  {
    word_t anon14;
    anon14 = int_to_word(0);
    word_t anon15;
    anon15 = closure_to_word(make_closure((word_t)fun1,1,0));
    word_t anon16;
    anon16 = int_to_word(1);
    word_t anon17;
    {
      word_t buf[2] = { anon9,anon16 };
      anon17 = int_sub(buf);
    }
    word_t anon18;
    {
      word_t buf[1] = { anon17 };
      anon18 = fn_arange(buf);
    }
    word_t anon19;
    {
      word_t buf[2] = { anon15,anon18 };
      anon19 = fn_map(buf);
    }
    word_t anon20;
    {
      word_t buf[2] = { anon14,anon19 };
      anon20 = fn_cons(buf);
    }
    anon12 = anon20;
  }
  return anon12;
}


word_t fn_bar(word_t* args)
{
  word_t anon21;
  anon21 = args[0];
  word_t anon22;
  anon22 = int_to_word(8);
  word_t anon23;
  anon23 = int_to_word(42);
  word_t anon24;
  {
    word_t buf[2] = { anon22,anon23 };
    anon24 = cons_BAR(buf);
  }
  return anon24;
}


word_t fn_cons(word_t* args)
{
  word_t anon25;
  word_t anon26;
  anon25 = args[0];
  anon26 = args[1];
  word_t anon27;
  {
    word_t buf[2] = { anon25,anon26 };
    anon27 = cons_Node(buf);
  }
  return anon27;
}


word_t fn_fibo(word_t* args)
{
  word_t anon28;
  anon28 = args[0];
  word_t anon29;
  anon29 = int_to_word(0);
  word_t anon30;
  {
    word_t buf[2] = { anon28,anon29 };
    anon30 = int_eq(buf);
  }
  word_t anon31;
  if (word_to_int(anon30))
  {
    word_t anon32;
    anon32 = fn_zero(NULL);
    anon31 = anon32;
  }
  else
  {
    word_t anon33;
    anon33 = int_to_word(1);
    word_t anon34;
    {
      word_t buf[2] = { anon28,anon33 };
      anon34 = int_eq(buf);
    }
    word_t anon35;
    if (word_to_int(anon34))
    {
      word_t anon36;
      anon36 = fn_one(NULL);
      anon35 = anon36;
    }
    else
    {
      word_t anon37;
      anon37 = fn_fibo_wrapper(NULL);
      word_t anon38;
      anon38 = int_to_word(1);
      word_t anon39;
      {
        word_t buf[2] = { anon28,anon38 };
        anon39 = int_sub(buf);
      }
      word_t anon40;
      {
        word_t buf[1] = { anon39 };
        anon40 = apply_closure(word_to_closure(anon37),buf,1);
      }
      word_t anon41;
      anon41 = fn_fibo_wrapper(NULL);
      word_t anon42;
      anon42 = int_to_word(2);
      word_t anon43;
      {
        word_t buf[2] = { anon28,anon42 };
        anon43 = int_sub(buf);
      }
      word_t anon44;
      {
        word_t buf[1] = { anon43 };
        anon44 = apply_closure(word_to_closure(anon41),buf,1);
      }
      word_t anon45;
      anon45 = int_to_word(0);
      word_t anon46;
      {
        word_t buf[2] = { anon40,anon45 };
        anon46 = extract_constructor(buf);
      }
      word_t anon47;
      anon47 = int_to_word(0);
      word_t anon48;
      {
        word_t buf[2] = { anon44,anon47 };
        anon48 = extract_constructor(buf);
      }
      word_t anon49;
      {
        word_t buf[2] = { anon46,anon48 };
        anon49 = int_add(buf);
      }
      anon35 = anon49;
    }
    anon31 = anon35;
  }
  return anon31;
}


word_t fn_fibo_caller(word_t* args)
{
  word_t anon50;
  anon50 = args[0];
  word_t anon51;
  {
    word_t buf[1] = { anon50 };
    anon51 = fn_fibo(buf);
  }
  word_t anon52;
  {
    word_t buf[1] = { anon51 };
    anon52 = cons_FOO(buf);
  }
  word_t anon53;
  anon53 = int_to_word(0);
  word_t anon54;
  {
    word_t buf[2] = { anon52,anon53 };
    anon54 = extract_constructor(buf);
  }
  return anon54;
}


word_t fn_fibo_wrapper(word_t* args)
{
  word_t anon55;
  anon55 = closure_to_word(make_closure((word_t)fun0,1,0));
  return anon55;
}


word_t fn_foo(word_t* args)
{
  word_t anon56;
  anon56 = args[0];
  word_t anon57;
  {
    word_t buf[1] = { anon56 };
    anon57 = cons_FOO(buf);
  }
  return anon57;
}


word_t fn_is_cons(word_t* args)
{
  word_t anon58;
  anon58 = args[0];
  word_t anon59;
  {
    word_t buf[1] = { anon58 };
    anon59 = test_cons_Node(buf);
  }
  return anon59;
}


word_t fn_is_nil(word_t* args)
{
  word_t anon60;
  anon60 = args[0];
  word_t anon61;
  {
    word_t buf[1] = { anon60 };
    anon61 = test_cons_Node(buf);
  }
  word_t anon62;
  {
    word_t buf[1] = { anon61 };
    anon62 = int_not(buf);
  }
  return anon62;
}


word_t fn_map(word_t* args)
{
  word_t anon63;
  word_t anon64;
  anon63 = args[0];
  anon64 = args[1];
  word_t anon65;
  {
    word_t buf[1] = { anon64 };
    anon65 = fn_is_nil(buf);
  }
  word_t anon66;
  if (word_to_int(anon65))
  {
    anon66 = anon64;
  }
  else
  {
    word_t anon67;
    anon67 = int_to_word(0);
    word_t anon68;
    {
      word_t buf[2] = { anon64,anon67 };
      anon68 = extract_constructor(buf);
    }
    word_t anon69;
    {
      word_t buf[1] = { anon68 };
      anon69 = apply_closure(word_to_closure(anon63),buf,1);
    }
    word_t anon70;
    anon70 = int_to_word(1);
    word_t anon71;
    {
      word_t buf[2] = { anon64,anon70 };
      anon71 = extract_constructor(buf);
    }
    word_t anon72;
    {
      word_t buf[2] = { anon63,anon71 };
      anon72 = fn_map(buf);
    }
    word_t anon73;
    {
      word_t buf[2] = { anon69,anon72 };
      anon73 = fn_cons(buf);
    }
    anon66 = anon73;
  }
  return anon66;
}


word_t fn_member(word_t* args)
{
  word_t anon74;
  word_t anon75;
  anon74 = args[0];
  anon75 = args[1];
  word_t anon76;
  {
    word_t buf[1] = { anon75 };
    anon76 = fn_is_cons(buf);
  }
  word_t anon77;
  if (word_to_int(anon76))
  {
    word_t anon78;
    anon78 = int_to_word(0);
    word_t anon79;
    {
      word_t buf[2] = { anon75,anon78 };
      anon79 = extract_constructor(buf);
    }
    word_t anon80;
    {
      word_t buf[2] = { anon74,anon79 };
      anon80 = int_eq(buf);
    }
    word_t anon81;
    anon81 = int_to_word(1);
    word_t anon82;
    {
      word_t buf[2] = { anon75,anon81 };
      anon82 = extract_constructor(buf);
    }
    word_t anon83;
    {
      word_t buf[2] = { anon74,anon82 };
      anon83 = fn_member(buf);
    }
    word_t anon84;
    {
      word_t buf[2] = { anon80,anon83 };
      anon84 = int_or(buf);
    }
    anon77 = anon84;
  }
  else
  {
    word_t anon85;
    anon85 = int_to_word(0);
    anon77 = anon85;
  }
  return anon77;
}


word_t fn_nil(word_t* args)
{
  word_t anon86;
  anon86 = int_to_word(0);
  return anon86;
}


word_t fn_one(word_t* args)
{
  word_t anon87;
  anon87 = int_to_word(1);
  return anon87;
}


word_t fn_sum(word_t* args)
{
  word_t anon88;
  anon88 = args[0];
  word_t anon89;
  {
    word_t buf[1] = { anon88 };
    anon89 = fn_is_nil(buf);
  }
  word_t anon90;
  if (word_to_int(anon89))
  {
    word_t anon91;
    anon91 = int_to_word(0);
    anon90 = anon91;
  }
  else
  {
    word_t anon92;
    anon92 = int_to_word(0);
    word_t anon93;
    {
      word_t buf[2] = { anon88,anon92 };
      anon93 = extract_constructor(buf);
    }
    word_t anon94;
    anon94 = int_to_word(1);
    word_t anon95;
    {
      word_t buf[2] = { anon88,anon94 };
      anon95 = extract_constructor(buf);
    }
    word_t anon96;
    {
      word_t buf[1] = { anon95 };
      anon96 = fn_sum(buf);
    }
    word_t anon97;
    {
      word_t buf[2] = { anon93,anon96 };
      anon97 = int_add(buf);
    }
    anon90 = anon97;
  }
  return anon90;
}


word_t fn_total(word_t* args)
{
  word_t anon98;
  anon98 = int_to_word(4002);
  word_t anon99;
  {
    word_t buf[1] = { anon98 };
    anon99 = fn_arange(buf);
  }
  word_t anon100;
  {
    word_t buf[1] = { anon99 };
    anon100 = fn_sum(buf);
  }
  return anon100;
}


word_t fn_zero(word_t* args)
{
  word_t anon101;
  anon101 = int_to_word(0);
  return anon101;
}


word_t fun0(word_t* args)
{
  word_t anon102;
  anon102 = args[0];
  word_t anon103;
  {
    word_t buf[1] = { anon102 };
    anon103 = fn_fibo(buf);
  }
  word_t anon104;
  {
    word_t buf[1] = { anon103 };
    anon104 = cons_FOO(buf);
  }
  return anon104;
}


word_t fun1(word_t* args)
{
  word_t anon105;
  anon105 = args[0];
  word_t anon106;
  anon106 = int_to_word(1);
  word_t anon107;
  {
    word_t buf[2] = { anon105,anon106 };
    anon107 = int_add(buf);
  }
  return anon107;
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