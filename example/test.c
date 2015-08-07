#include "test.h"

int x;

void testfunc (int a, int b, int c, int d)
{
   if ((a || b) && (c || d))
   {
      x = 0;
   }
   else
   {
      x = 1;
   }
}
