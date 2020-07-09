/* run.config
   STDOPT: +"-lannot=ADC"
 */

#include <stdio.h>
#include <math.h>

int armstrong(int low, int high)
{
    int i, temp1, temp2, remainder, n = 0, result = 0;
    
    printf("Armstrong numbers between %d an %d are: ", low, high);

    for(i = low + 1; i < high; ++i)
    {
        temp2 = i;
        temp1 = i;


        loop: if(temp1 == 0) goto skip;
        // number of digits calculation
        temp1 /= 10;
        ++n;
        goto loop;
        
        // result contains sum of nth power of its digits
        skip: while (temp2 != 0)
        {
            remainder = temp2 % 10;
            result += pow(remainder, n);
            temp2 /= 10;
        }

        // checks if number i is equal to the sum of nth power of its digits
        if (result == i) {
            printf("%d ", i);
        }

        // resetting the values to check Armstrong number for next iteration
        n = 0;
        result = 0;

    }
    return 0;
}
