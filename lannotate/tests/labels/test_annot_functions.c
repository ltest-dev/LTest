/* run.config
   STDOPT: +"-lannot=FC,CC -lannot-functions=add,sub,main -lannot-debug 1"
 */

int add (int a, int b){
	return a+b;
}

int sub (int a, int b){
	if (a >= b)
		return a-b;
	else
		return b-a;
}

int mul (int a, int b){
	return a*b;
}

int mod (int a, int b){
	return a%b;
}

int div (int a, int b){
	int c = mod(a,b);
	if (c)
		return a/b;
	else
		return c;
}

int main(){
	int a = 24;
	int b = 42;
	
	if (a < 0 || a > 100)
		a = 12;
	a = sub(div(add(mul(a,2),84),2),a);
	return !(a == b);
}
