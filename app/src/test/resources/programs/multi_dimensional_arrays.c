long sum(long arr[3][4], int width, int height) {
    long total = 0;
    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            total += arr[i][j];
        }
    }
    return total;
}

int main(void) {
    long arr[3][4] = {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12}
    };
    if (arr[0][1] != 2) {
        return 1;
    }
    if (arr[2][0] != 9) {
        return 2;
    }
    if (arr[2][3] != 12) {
        return 3;
    }

    long s = sum(arr, 4, 3);
    if (s != 78) {
        return 4;
    }

    int arr2[2][2] = {{0}};
    for (int i = 0; i < 2; i++)
        for (int j = 0; j < 2; j++)
            if (arr2[i][j] != 0)
                return 5;

    return 0;
}