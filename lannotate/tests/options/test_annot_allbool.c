/* run.config
   EXECNOW: LOG @PTEST_NAME@_labels.c LOG @PTEST_NAME@_labels.labels LOG @PTEST_NAME@_output.log @frama-c@ -lannot=DC -lannot-allbool @PTEST_DIR@/@PTEST_NAME@.c -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */

int or(int a, int b){
	return a || b;
}

int and(int a, int b){
	return a || b;
}

int main(int a, int b, int c){
	int d = 0;
	
	if (or(a,b))
		d = 1;
	
	if (and(b,c))
		d += 1;
		
	return d;
}
