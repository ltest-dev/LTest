/* run.config
   EXECNOW: LOG @PTEST_NAME@.labels LOG @PTEST_NAME@.log @frama-c@ -luncov-value @PTEST_DIR@/@PTEST_NAME@.c -luncov-labels @PTEST_DIR@/result/@PTEST_NAME@.labels -wp-msg-key no-time-info > @PTEST_DIR@/result/@PTEST_NAME@.log 
 */

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
   int __retres;
   pc_label(a > 0,1,"DC");
   pc_label(! (a > 0),2,"DC");
   if (a > 0) {
     pc_label(a == 0,3,"DC");
     pc_label(! (a == 0),4,"DC");
     if (a == 0) {
       __retres = 1;
       goto return_label;
     }
   }
   __retres = 0;
   return_label: return __retres;
 }


