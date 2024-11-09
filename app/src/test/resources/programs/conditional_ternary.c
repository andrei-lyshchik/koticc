int main(void) {
    int a = 1;
    int b = a == 2 ? 2 : 3;
    return a < b ? 1 : a == b ? 2 : 3;
}