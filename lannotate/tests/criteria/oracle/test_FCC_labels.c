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

int f(int x)
 {
   int __retres;
   __retres = x + 1;
   return __retres;
 }

int g(int x)
{
  int tmp;
  pc_label(1,1,"FCC");
  tmp = f(x + 1);
  return tmp;
}

int main(void)
{
  int __retres;
  int tmp_1;
  pc_label(1,2,"FCC");
  int a = f(12);
  pc_label(1,3,"FCC");
  tmp_1 = g(40);
  if (tmp_1) {
    int tmp_0;
    pc_label(1,4,"FCC");
    tmp_0 = g(41);
    __retres = tmp_0;
    goto return_label;
  }
  __retres = a;
  return_label: return __retres;
}


