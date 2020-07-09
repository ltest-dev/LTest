/* run.config
   STDOPT: +"-lannot=FCC"
 */


int f(int x){
	return x+1;
}

int g(int x){
	return f(x+1);
}

int maintest(){
	int a = f(12);
	if (g(40))
	  return g(41);
	return a;
}
