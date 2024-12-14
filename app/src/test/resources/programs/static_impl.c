static int x = 0;
extern int y;

int read_x_plus_y(void) {
    return x + y;
}

int write_x(int new_val) {
    x = new_val;
    return x;
}