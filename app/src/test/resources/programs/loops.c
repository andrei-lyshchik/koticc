int main(void) {
    int a = 0;
    do {
        a++;
        if (a == 5) {
            break;
        }
        if (a == 3) {
            continue;
        }
        a += 2;
    } while (a < 10);

    int b = 0;
    while (b < 10) {
        if (b == 5) {
            break;
        }
        b++;
        if (b == 3) {
            continue;
        }
        b += 2;
    }

    int c = 0;
    for (int i = 0; i < 10; i++) {
        c += i;
        if (c == 5) {
            break;
        }
        if (c == 3) {
            continue;
        }
        c += 2;
    }

    return a + b + c;
}