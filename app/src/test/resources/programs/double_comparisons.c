int main(void) {
    if (1.0 != 1.0) {
        return 1;
    }

    if (2.0 > 3.0) {
        return 2;
    }

    if (5.0 < 4.0) {
        return 3;
    }

    if (6.0 >= 7.0) {
        return 4;
    }

    if (8.0 <= 9.0) {
        return 5;
    }

    return 0;
}