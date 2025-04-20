int a = 0;

int println_int(int i);

int *println_1(void) {
    println_int(1);
    return &a;
}

int main(void) {
    int res = (*println_1() += 2);
    if (a != 2) {
        return 1;
    }
    if (res != 2) {
        return 2;
    }

    double d = 5.0;
    double *d_ptr = &d;
    *d_ptr *= 1000u;
    if (d != 5000.0) {
        return 3;
    }

    return 0;
}