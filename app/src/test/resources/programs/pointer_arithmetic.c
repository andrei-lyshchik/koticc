unsigned sum_via_ptr(unsigned *arr, int length) {
    unsigned total = 0;
    for (unsigned *ptr = arr; ptr < arr + length; ptr = ptr + 1) {
        total += *ptr;
    }
    return total;
}

unsigned sum_via_ptr_with_diff(unsigned *arr, int length) {
    unsigned total = 0;
    for (unsigned *ptr = arr; ptr - arr < length; ptr = ptr + 1) {
        total += *ptr;
    }
    return total;
}

int is_ptr_less(unsigned *ptr, unsigned *another) {
    return ptr < another;
}

int main(void) {
    unsigned arr[5] = {1, 2, 3};
    if (is_ptr_less(arr, arr)) {
        return 1;
    }
    if (is_ptr_less(arr + 1, arr)) {
        return 2;
    }
    if (!is_ptr_less(arr, arr + 1)) {
        return 3;
    }

    unsigned arr_1 = *(arr + 1);
    if (arr_1 != 2) {
        return 4;
    }
    unsigned arr_2 = *(arr + 2);
    if (arr_1 + arr_2 != 5) {
        return 5;
    }

    unsigned arr_sum = sum_via_ptr(arr, 5);
    if (arr_sum != 6) {
        return 6;
    }

    unsigned *arr_3 = arr + 3;
    unsigned *arr_3_prev = arr_3 - 1;
    if (*arr_3_prev != 3) {
        return 7;
    }

    unsigned *arr_2_index_first = 2 + arr;
    if (*arr_2_index_first != 3) {
        return 8;
    }

    unsigned sum_via_ptr_with_diff_result = sum_via_ptr_with_diff(arr, 5);
    if (sum_via_ptr_with_diff_result != 6) {
        return 9;
    }

    long negative_ptr_diff = arr - arr_3;
    if (negative_ptr_diff != -3) {
        return 10;
    }

    return 0;
}