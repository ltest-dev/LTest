/* run.config
   EXECNOW: LOG @PTEST_NAME@_labels.c LOG @PTEST_NAME@_labels.hyperlabels LOG @PTEST_NAME@_labels.labels LOG @PTEST_NAME@_output.log @frama-c@ -lannot=FCC @PTEST_DIR@/@PTEST_NAME@.c -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */


int f(int x){
	return x+1;
}

int g(int x){
	return f(x+1);
}

int main(){
	int a = f(12);
	if (g(40))
	  return g(41);
	return a;
}
