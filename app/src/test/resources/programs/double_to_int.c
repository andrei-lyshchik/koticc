int double_to_int(double d) {
    return (int) d;
}

long double_to_long(double d) {
    return (long) d;
}

int main(void) {
    long l = double_to_long(2147483648.1);
    if (l != 2147483648l) {
        return 1;
    }

    int i = double_to_int(-4.9999);
    if (i != -4) {
        return 2;
    }

    return 0;
}