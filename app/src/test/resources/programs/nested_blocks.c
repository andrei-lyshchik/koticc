int main(void) {
    int a = 1;
    int b = 2;
    int c = 3;
    if (a < 100) {
start:
        a *= 2;
        int a = 1;
        c *= 2;
        c += a;
        if (c < 10) {
            goto start;
        }
        b += c;
    }
    return a + b + c;
}