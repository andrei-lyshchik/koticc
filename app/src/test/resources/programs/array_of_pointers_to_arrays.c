int main(void) {
    int a[3] = {1, 2, 3};
    int c[3] = {7, 8, 9};

    int (*p1)[3] = &a;
    int (*p2)[3] = &c;

    int (*ap[2])[3] = { p1, p2 };

    if (ap[0] != p1) {
        return 1;
    }
    if (ap[1][0] != c) {
        return 2;
    }
    if (ap[0][0][1] != 2) {
        return 3;
    }
    if (ap[1][0][2] != 9) {
        return 4;
    }
    ap[1][0][2] = 333;
    if (ap[1][0][2] != 333) {
        return 5;
    }

    return 0;
}