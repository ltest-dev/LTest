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

int main(int a)
 {
   a = 0;
   a = 1;
   a = 2;
   a = 3;
   a = 4;
   pc_label_sequence_condition(0,"24");
   a = 5;
   pc_label_sequence(1,1UL,1,2,"24",0);
   pc_label_sequence(1,1UL,2,2,"24",0);
   return a;
 }


