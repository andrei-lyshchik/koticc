int main(void) {
    unsigned long a = 10324ul;
    unsigned int b = 34u;
    unsigned int c = a / b;
    if (c != 303u) {
        return 1;
    }

    unsigned long with_constants = 10324ul / 34u;
    if (with_constants != 303ul) {
        return 2;
    }

    return 0;
}