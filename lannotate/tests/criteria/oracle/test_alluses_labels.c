/* Generated by Frama-C LTest */


#ifndef pc_label
#define pc_label(...) do{}while(0)
#endif
#ifndef pc_label_bindings
#define pc_label_bindings(...) do{}while(0)
#endif
#ifndef pc_label_sequence
#define pc_label_sequence(...) do{}while(0)
#endif
#ifndef pc_label_sequence_condition
#define pc_label_sequence_condition(...) do{}while(0)
#endif

int f(void)
 {
   int __retres;
   __retres = 0;
   return __retres;
 }

int maintest(int c)
{
  int __retres;
  pc_label_sequence(1,4UL,1,2,"52",0);
  pc_label_sequence(1,3UL,1,2,"52",0);
  pc_label_sequence(1,2UL,1,2,"52",0);
  pc_label_sequence(1,1UL,1,2,"52",0);
  pc_label_sequence_condition(0,"53");
  int a = 1;
  pc_label_sequence(1,5UL,1,2,"53",0);
  pc_label_sequence(1,6UL,1,2,"53",0);
  pc_label_sequence(1,7UL,1,2,"53",0);
  pc_label_sequence_condition(0,"54");
  int b = 2;
  pc_label_sequence(1,8UL,1,2,"54",0);
  pc_label_sequence(1,9UL,1,2,"54",0);
  pc_label_sequence(1,10UL,1,2,"54",0);
  test: ;
  pc_label_sequence(1,5UL,2,2,"53",0);
  if (a) {
    pc_label_sequence(1,8UL,2,2,"54",0);
    if (b) {
      pc_label_sequence_condition(0,"54");
      b = 3;
      pc_label_sequence(1,11UL,1,2,"54",0);
      pc_label_sequence(1,12UL,1,2,"54",0);
    }
    else {
      test2: pc_label_sequence_condition(0,"53");
             a = f();
      pc_label_sequence(1,13UL,1,2,"53",0);
      pc_label_sequence(1,14UL,1,2,"53",0);
    }
  }
  else {
    pc_label_sequence(1,6UL,2,2,"53",0);
    pc_label_sequence(1,13UL,2,2,"53",0);
    pc_label_sequence_condition(0,"55");
    int i = a;
    pc_label_sequence(1,15UL,1,2,"55",0);
    pc_label_sequence(1,16UL,1,2,"55",0);
    while (1) {
      pc_label_sequence(1,15UL,2,2,"55",0);
      pc_label_sequence(1,17UL,2,2,"55",0);
      pc_label_sequence(1,9UL,2,2,"54",0);
      pc_label_sequence(1,11UL,2,2,"54",0);
      if (! (i < b)) break;
      f();
      pc_label_sequence(1,16UL,2,2,"55",0);
      pc_label_sequence(1,18UL,2,2,"55",0);
      pc_label_sequence_condition(0,"55");
      i ++;
      pc_label_sequence(1,17UL,1,2,"55",0);
      pc_label_sequence(1,18UL,1,2,"55",0);
    }
  }
  test4: ;
  pc_label_sequence(1,1UL,2,2,"52",0);
  switch (c) {
    int tmp;
    test5: case 1: case 14: ;
    pc_label_sequence(1,7UL,2,2,"53",0);
    pc_label_sequence(1,14UL,2,2,"53",0);
    pc_label_sequence(1,10UL,2,2,"54",0);
    pc_label_sequence(1,12UL,2,2,"54",0);
    __retres = a + b;
    goto return_label;
    case 12: case 13: ;
    pc_label_sequence(1,2UL,2,2,"52",0);
    __retres = c;
    goto return_label;
    default: ;
    pc_label_sequence(1,3UL,2,2,"52",0);
    if (c) tmp = 0;
    else {
      pc_label_sequence(1,4UL,2,2,"52",0);
      tmp = c;
    }
    __retres = tmp;
    goto return_label;
  }
  __retres = 0;
  return_label: return __retres;
}


