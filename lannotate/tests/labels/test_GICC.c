/* run.config
   STDOPT: +"-lannot=GICC" 
 */


int maintest(int a, int b, int c){
	if (a && b || c)
		return 0;
	return 1;
}
