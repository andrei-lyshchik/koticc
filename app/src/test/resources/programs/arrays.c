int file_scope[3] = { 4, 5, 6 };
long file_scope_zero_padded[15] = { 7, 8, 0, 10, 11, 0, 13, 14, 0, 16, 17, 18 };
double double_array[3] = { 3.14, 2.71 };

int inc_static_array(void) {
    static int e[5] = {0, 1, 2, 3, 4};
    for (int i = 0; i < 5; i++) {
        e[i]++;
    }
    int sum = 0;
    for (int i = 0; i < 5; i++) {
        sum += e[i];
    }
    return sum;
}

int array_sum(int *arr, int size) {
    int sum = 0;
    for (int i = 0; i < size; i++) {
        sum += arr[i];
    }
    return sum;
}

int array_sum_with_array_param(int arr[1], int size) {
    int sum = 0;
    for (int i = 0; i < size; i++) {
        sum += arr[i];
    }
    return sum;
}

// shouldn't conflict with previous declaration
int array_sum_with_array_param(int *arr, int size);

int check_subscript(unsigned *arr, unsigned expected) {
    // make sure our index can be any integer type
    unsigned val1 = arr[5];
    unsigned val2 = arr[5u];
    unsigned val3 = arr[5l];
    unsigned val4 = arr[5ul];
    if (val1 != expected) {
        return 1;
    }

    if (val2 != expected) {
        return 2;
    }

    if (val3 != expected) {
        return 3;
    }

    if (val4 != expected) {
        return 4;
    }
    return 0;
}

int main(void) {
    int a[10] = {0};
    for (int i = 0; i < 10; i++) {
        a[i] = i;
    }

    int res = 0;
    for (int i = 0; i < 10; i++) {
        res += a[i];
    }

    if (res != 45) {
        return 1;
    }

    int file_scope_sum = 0;
    for (int i = 0; i < 3; i++) {
        file_scope_sum += file_scope[i];
    }
    if (file_scope_sum != 15) {
        return 2;
    }
    long file_scope_zero_padded_sum = 0;
    for (int i = 0; i < 15; i++) {
        file_scope_zero_padded_sum += file_scope_zero_padded[i];
    }
    if (file_scope_zero_padded_sum != 114) {
        return 3;
    }

    int uninitialized[5];
    for (int i = 0; i < 5; i++) {
        uninitialized[i] = i + 1;
    }
    int uninitialized_sum = 0;
    for (int i = 0; i < 5; i++) {
        uninitialized_sum += uninitialized[i];
    }
    if (uninitialized_sum != 15) {
        return 4;
    }

    int not_all_initialized[5] = { 4 + 8, 2 * 6 + a[1] };
    int not_all_initialized_sum = 0;
    for (int i = 0; i < 5; i++) {
        not_all_initialized_sum += not_all_initialized[i];
    }
    if (not_all_initialized_sum != 25) {
        return 5;
    }

    int static_array_sum = inc_static_array();
    if (static_array_sum != 15) {
        return 6;
    }
    int static_array_sum2 = inc_static_array();
    if (static_array_sum2 != 20) {
        return 7;
    }

    double double_array_sum = 0.0;
    for (int i = 0; i < 3; i++) {
        double_array_sum += double_array[i];
    }
    if (double_array_sum != 5.85) {
        return 8;
    }

    int array_sum_result = array_sum(a, 10);
    if (array_sum_result != 45) {
        return 9;
    }

    int array_sum_with_array_param_result = array_sum_with_array_param(file_scope, 3);
    if (array_sum_with_array_param_result != 15) {
        return 10;
    }

    unsigned arr_unsigned[6] = {0, 0, 0, 0, 0, 7u};
    int check = check_subscript(arr_unsigned, 7u);
    if (check) {
        return 10 + check;
    }

    return 0;
}