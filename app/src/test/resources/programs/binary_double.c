int println_double(double d);

int main(void) {
    println_double(3.14);
    double a = 1.0;
    double b = 2.0;
    double c = a + b * 2.0 - 3.0 / 4.0;
    println_double(c);
    return 0;
}