double func(double d1, long l1, double d2, int i1, double d3, unsigned int u1, double d4, long l2, double d5, int i2, double d6, unsigned int u2, double d7, long l3, double d8, int i3, double d9, unsigned int u3, double d10, long l4, double d11, int i4, double d12, unsigned int u4, double d13, long l5, double d14, int i5);

double double_abs(double d) {
    if (d < 0) {
        return -d;
    }
    return d;
}

int main(void) {
    double result = func(1.0, 2l, 3.0, 4, 5.0, 6u, 7.0, 8l, 9.0, 10, 11.0, 12u, 13.0, 14l, 15.0, 16, 17.0, 18u, 19.0, 20l, 21.0, 22, 23.0, 24u, 25.0, 26l, 27, 28);
    double expected_value = 1369.793211;
    double epsilon = 1e-6;
    if (double_abs(expected_value - result) > epsilon) {
        return 1;
    }

    return 0;
}