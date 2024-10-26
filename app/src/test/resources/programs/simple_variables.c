int main(void) {
    int a;
    int b = 23 * 2;
    int c = 1 + 2 * (a = 3) - b;
    a = b * 5;
    return a - b + c;
}
