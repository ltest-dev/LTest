/* run.config
   STDOPT: +"-lannot=DC -lannot-allbool"
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
