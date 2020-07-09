/* run.config
   STDOPT: +"-lannot=CACC"
 */


int maintest(int a, int b, int c){
	if (a == b)
		a = a - b;

	if (a && b || c)
		return 0;
	return 1;
}
