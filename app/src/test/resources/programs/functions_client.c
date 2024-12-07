int func1(int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8, int a9);
int func2(int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8, int a9, int a10);

int main(void) {
    return func1(1, 2, 3, 4, 5, 6, 7, 8, 9) + func2(10, 9, 8, 7, 6, 5, 4, 3, 2, 1);
}
