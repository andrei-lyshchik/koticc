#include <stdio.h>

int println_int(int i) {
    return printf("%d\n", i);
}

int println_long(long l) {
    return printf("%ld\n", l);
}

int println_ulong(unsigned long i) {
    return printf("%lu\n", i);
}

int println_double(double d) {
    return printf("%f\n", d);
}