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

void donothing(void)
 {
   return;
 }

int maintest(int i)
{
  int __retres;
  pc_label_sequence(1,1UL,1,2,"4",0);
  while (1) pc_label_sequence_condition(0,"4");
  pc_label_sequence(1,1UL,2,2,"4",0);
  pc_label_sequence(1,2UL,1,2,"7",0);
  while (1) {
    pc_label_sequence_condition(0,"7");
    donothing();
  }
  pc_label_sequence(1,2UL,2,2,"7",0);
  pc_label_sequence(1,3UL,1,2,"9",0);
  while (1) {
    pc_label_sequence_condition(0,"9");
    if (! (i < 10)) break;
  }
  pc_label_sequence(1,3UL,2,2,"9",0);
  pc_label_sequence(1,4UL,1,2,"14",0);
  while (1) {
    if (i < 10) pc_label_sequence_condition(0,"14"); else break;
    donothing();
  }
  pc_label_sequence(1,4UL,2,2,"14",0);
  pc_label_sequence(1,5UL,1,2,"19",0);
  while (1) {
    int tmp;
    pc_label_sequence_condition(0,"19");
    tmp = i;
    i ++;
    ;
    if (! tmp) break;
  }
  pc_label_sequence(1,5UL,2,2,"19",0);
  pc_label_sequence(1,6UL,1,2,"27",0);
  while (1) {
    int tmp_0;
    tmp_0 = i;
    i ++;
    ;
    if (tmp_0) pc_label_sequence_condition(0,"27"); else break;
    donothing();
  }
  pc_label_sequence(1,6UL,2,2,"27",0);
  pc_label_sequence(1,7UL,1,2,"35",0);
  while (1) pc_label_sequence_condition(0,"35");
  pc_label_sequence(1,7UL,2,2,"35",0);
  pc_label_sequence(1,8UL,1,2,"37",0);
  while (1) {
    pc_label_sequence_condition(0,"37");
    donothing();
  }
  pc_label_sequence(1,8UL,2,2,"37",0);
  pc_label_sequence(1,9UL,1,2,"39",0);
  while (1) {
    pc_label_sequence_condition(0,"39");
    if (! (i < 10)) break;
  }
  pc_label_sequence(1,9UL,2,2,"39",0);
  pc_label_sequence(1,10UL,1,2,"45",0);
  while (1) {
    pc_label_sequence_condition(0,"45");
    donothing();
    if (! (i < 10)) break;
  }
  pc_label_sequence(1,10UL,2,2,"45",0);
  pc_label_sequence(1,11UL,1,2,"51",0);
  while (1) {
    int tmp_1;
    pc_label_sequence_condition(0,"51");
    tmp_1 = i;
    i ++;
    ;
    if (! tmp_1) break;
  }
  pc_label_sequence(1,11UL,2,2,"51",0);
  pc_label_sequence(1,12UL,1,2,"60",0);
  while (1) {
    int tmp_2;
    pc_label_sequence_condition(0,"60");
    donothing();
    tmp_2 = i;
    i ++;
    ;
    if (! tmp_2) break;
  }
  pc_label_sequence(1,12UL,2,2,"60",0);
  __retres = 0;
  return __retres;
}


