/* run.config
   STDOPT: +"-lannot=AUC"
 */
 
struct coord
{
    int x;
    int y;
};

int f(int x, int y)
 {
    struct coord test;
    test.x = x;
    test.y = y;
     
    return test.x + test.y;
}
