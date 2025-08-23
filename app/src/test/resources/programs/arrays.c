int file_scope[3] = { 4, 5, 6 };
int file_scope_zero_padded[15] = { 7, 8, 0, 10, 11, 0, 13, 14, 0, 16, 17, 18 };

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

int main(void) {
    int a[10] = {0};
    for (int i = 0; i < 10; i++) {
        a[i] = i;
    }
//
//    int res = 0;
//    for (int i = 0; i < 10; i++) {
//        res += a[i];
//    }
//
//    if (res != 45) {
//        return 1;
//    }
//
//    int file_scope_sum = 0;
//    for (int i = 0; i < 3; i++) {
//        file_scope_sum += file_scope[i];
//    }
//    if (file_scope_sum != 15) {
//        return 2;
//    }
    int file_scope_zero_padded_sum = 0;
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

    int file_scope_sum = 0;
    for (int i = 0; i < 3; i++) {
        file_scope_sum += file_scope[i];
    }
    if (file_scope_sum != 15) {
        return 2;
    }
    int file_scope_zero_padded_sum = 0;
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

    return 0;
}