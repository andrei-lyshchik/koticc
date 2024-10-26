int main(void) {
    int a = 5;
    int b = 3 + --a;
    int c = b-- * a;
    int d = a++ * b;
    int e = d++ * a;
    int f = ++c * d;
    return a + b + c + d + e + f;
}