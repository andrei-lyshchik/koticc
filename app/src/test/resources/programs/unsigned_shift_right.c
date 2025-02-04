int main(void) {
    unsigned long l = 1lu << 33;
    unsigned int i = (unsigned int) (l - 1);

    if ((i >> 2) != 1073741823) {
        return 1;
    }

    return 0;
}