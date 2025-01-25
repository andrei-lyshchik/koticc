long sign_extend(int i, long expected) {
    long extended = (long) i;
    return extended == expected;
}

int main(void) {
//    int result1 = 0;
    long result1 = sign_extend(10, 10l);
    long result2 = sign_extend(-10, -10l);
//    int result2 = 0;
    long l = (long) 100;
    long result3 = l == 100l;
//    int result3 = 0;

    return result1 +
        result2 +
        result3;
}