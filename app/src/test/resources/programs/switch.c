int main(void) {
    int result = 0;

    int a = 1;
    switch (a + 1) {
    case 2:
        result = 1;
        break;
    case 3:
        result = 2;
        break;
    }

    switch (a + 2) {
    case 0:
        result += 1;
        break;
    case 3:
        result += 2;
    case 4:
        result += 3;
    }

    switch (a) {
    case 10:
        result += 1;
    default:
        result *= 3;
    }

    switch (a) {
        result /= 0;
    }

    return result;
}