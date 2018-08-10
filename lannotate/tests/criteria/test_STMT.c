/* run.config
   LOG: @PTEST_NAME@_labels.c
   LOG: @PTEST_NAME@_labels.labels
   LOG: @PTEST_NAME@_output.log
   EXECNOW: @frama-c@ -lannot=STMT @PTEST_DIR@/@PTEST_NAME@.c -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */

int f(){
	return 0;
}

int maintest(int c){
	int a = 1;
	int b = 2;

	test : if (a){
		if(b){
			b = 3;
		}
		else{
			test2 : a = f();
		}
 	}
 	else{
		for(int i = a; i < b; i++){
			f();
		}
 	}

	test4 : switch(c){
		case 1 : case 14 :
	       test5 : return a + b;
     	case 12 : 
     	case 13 :
     		return c;
     
     	default :
     		return ( c ? 0 : c);
   }
   return 0;
}
