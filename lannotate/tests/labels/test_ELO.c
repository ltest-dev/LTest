/* run.config
   STDOPT: +"-lannot=ELO" 
 */

void donothing(){;}

int maintest(int i){
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
