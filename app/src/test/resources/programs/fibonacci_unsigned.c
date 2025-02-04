unsigned long fibonacci(unsigned int n) {
    if (n <= 1) {
        return n;
    }
    unsigned long a = 0;
    unsigned long b = 1;
    for (unsigned int i = 2; i <= n; i++) {
        unsigned long c = a + b;
        a = b;
        b = c;
    }
    return b;
}

int println_ulong(unsigned long i);

int main(void) {
    println_ulong(fibonacci(93));
}

