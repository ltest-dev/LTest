/* run.config
   STDOPT: +"-lannot=AUC"
 */
 
void g(int x) {return;}
int j() {return 1;}

void f()
 {
    int x;
    for(x=0;x<10;x++){
        g(x);
        g(x);
    }    
     
    return;
}
