int main(void) {
    static double d = 1.5;
    if (d++ != 1.5) {
        return 1;
    }
    if (d != 2.5) {
        return 2;
    }

    if (d-- != 2.5) {
        return 3;
    }
    if (d != 1.5) {
        return 4;
    }

    return 0;
}