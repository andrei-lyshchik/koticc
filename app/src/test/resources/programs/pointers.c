int set(int *p, int update) {
    return (*p = update);
}

double *d = 0;
long *l;

int main(void) {
    int a = 0;
    int *p = &a;

    *p = 5;

    if (a != 5) {
        return 1;
    }

    if (*p != 5) {
        return 2;
    }

    int res = set(p, 10);
    if (res != 10) {
        return 3;
    }
    if (a != 10) {
        return 4;
    }

    if (!p) {
        return 5;
    }
    p = 0;
    if (p) {
        return 6;
    }
    int b = 3;
    p = &b;
    int **pp = &p;
    if (**pp != 3) {
        return 7;
    }

    *pp = &a;
    if (*p != 10) {
        return 8;
    }

    double d1 = 3.14;
    d = &d1;
    if (*d != 3.14) {
        return 9;
    }
    double *d2 = d;
    *d2 = 2.17;
    if (*d != 2.17) {
        return 10;
    }

    *p = -1;
    unsigned int *ul = (unsigned int *) p;
    if (*ul != 4294967295) {
        return 11;
    }

    long ll = 123;
    l = &ll;
    if (*l != 123) {
        return 12;
    }

    int *null_ptr = 0;
    if (&*null_ptr != 0) {
        return 13;
    }

    return 0;
}