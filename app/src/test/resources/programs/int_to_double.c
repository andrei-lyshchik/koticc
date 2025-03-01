int println_double(double d);

int main(void) {
//
    double from_int = (double) -43434343;
    if (from_int != -43434343.0) {
        return 1;
    }

    double from_uint = (double) 5u;
    if (from_uint != 5.0) {
        return 2;
    }

    double from_ulong_big = (double) 18446744073709551586ul;
    if (from_ulong_big != 18446744073709551616.0) {
        return 3;
    }

    double from_long = (double) -9007199254751227l;
//    println_double(from_long);
    if (from_long != -9007199254751227.0) {
        return 4;
    }

    double from_long_not_exact = (double) 1152921504606846977l;
//    println_double(from_long_not_exact);
    // closest double to 1152921504606846977 is 1152921504606846976
    if (from_long_not_exact != 1152921504606846976.0) {
        return 5;
    }

    return 0;
}