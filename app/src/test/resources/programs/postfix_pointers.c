int main(void) {
    int arr[5] = {10, 11, 12, 13, 14};
    int *ptr = arr;
    int *ptr_before = ptr++;
    if (*ptr_before != 10) {
        return 1;
    }
    if (*ptr != 11) {
        return 2;
    }
    int **ptr_to_ptr = &ptr;
    (*ptr_to_ptr)++;
    if (*ptr != 12) {
        return 3;
    }

    ptr += 2;
    if (*ptr != 14) {
        return 4;
    }

    ptr -= 1;
    if (*ptr != 13) {
        return 5;
    }

    (*ptr_to_ptr) -= 2;
    if (*ptr != 11) {
        return 6;
    }

    return 0;
}