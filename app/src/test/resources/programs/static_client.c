int x = 1;
int y;

int write_x(int new_val);
int read_x_plus_y(void);

int main(void) {
    x = 2;
    y = 15;
    write_x(3);
    return read_x_plus_y() + x;
}