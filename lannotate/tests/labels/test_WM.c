/* run.config
   STDOPT: +"-lannot=WM" 
 */


int maintest(int a, int b){
	if (a < b && a/b != 42){
		return a%b;
	}
	else{
		return	(a + b) * 4;
	}
}
