/* run.config
   LOG: @PTEST_NAME@_labels.c
   LOG: @PTEST_NAME@_labels.labels
   LOG: @PTEST_NAME@_labels.hyperlabels
   LOG: @PTEST_NAME@_output.log
   EXECNOW: @frama-c@ -lannot=SLO @PTEST_DIR@/@PTEST_NAME@.c -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */

void donothing(){;}

int main(int i){
	while(1){;} //OK
	while(1) donothing(); //OK
	while(i < 10); //OK
	while(i < 10) donothing(); //OK
	while(i++); //OK
	while(i++) donothing(); //OK
	
	do{;} while(1); //OK
	do{donothing();} while(1); //OK
	do{;} while(i < 10); //NOT OK
	do{donothing();} while(i < 10); //NOT OK
	do{;} while(i++); //NOT OK
	do{donothing();} while(i++); //NOT OK

	return 0;
}
