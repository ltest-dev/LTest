/* run.config
   LOG: @PTEST_NAME@_labels.c
   LOG: @PTEST_NAME@_labels.labels
   LOG: @PTEST_NAME@_labels.hyperlabels
   LOG: @PTEST_NAME@_output.log
   EXECNOW: @frama-c@ -lannot=context @PTEST_DIR@/@PTEST_NAME@.c -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */

int maintest(int c){
	int a = 1;
	int b = 2;
	
	while(c){
		b = a + c;
		a = 2;
	}
	
   return a + b + c;
}
