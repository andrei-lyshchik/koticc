int println_long(long l);

long factorial(int n) {
    long result = 1;
    for (int i = 1; i <= n; ++i) {
        result *= i;
    }
    return result;
}

int main(void) {
    println_long(factorial(13));
    return 0;
}