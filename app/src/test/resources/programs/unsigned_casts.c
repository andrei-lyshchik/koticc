unsigned int ui = 4294967200u;

int main(void) {
    int i = (int) ui;
    if (i != -96) {
        return 1;
    }

    unsigned long ul1 = (unsigned long) i;
    if (ul1 != 18446744073709551520ul) {
        return 2;
    }

    unsigned long ul2 = (unsigned long) (int) ui;
    if (ul2 != 18446744073709551520ul) {
        return 3;
    }

    return 0;
}