int main(void) {
    int a[10] = {0};
    for (int i = 0; i < 10; i++) {
        a[i] = i;
    }

    int res = 0;
    for (int i = 0; i < 10; i++) {
        res += a[i];
    }

    if (res != 45) {
        return 1;
    }

    return 0;
}