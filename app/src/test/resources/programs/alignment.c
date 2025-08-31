int check_alignment(int *ptr) {
    unsigned long addr = (unsigned long) ptr;
    return (addr % 16 == 0);
}

int check_alignment_2d(int (*ptr)[2]) {
    unsigned long addr = (unsigned long) ptr;
    return (addr % 16 == 0);
}

int main(void)
{
    int arr[5] = {0};
    int arr2[2][2] = {{0}};

    if (!check_alignment(arr)) {
        return 1;
    }

    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 2; j++) {
            arr2[i][j] = i + j;
        }
    }

    if (!check_alignment_2d(arr2)) {
        return 2;
    }

    return 0;
}