#include <stdio.h>
#include "gprofExampleNew.hpp"

void new_func1(void)
{
    printf("\n Inside new_func1()\n");

    unsigned int i = 0;
    for(;i<0xffffffee;i++);

    return;
}
