/* run.config
   LOG: @PTEST_NAME@_labels.c
   LOG: @PTEST_NAME@_labels.labels
   LOG: @PTEST_NAME@_labels.hyperlabels
   LOG: @PTEST_NAME@_output.log
   EXECNOW: @frama-c@ -lannot=RACC @PTEST_DIR@/@PTEST_NAME@.c -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */


int main(int a, int b, int c){
	if (a == b)
		a = a - b;

	if (a && b || c)
		return 0;
	return 1;
}
