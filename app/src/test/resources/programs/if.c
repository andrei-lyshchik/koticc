int main(void) {
    int a = 1;
    int b = 2;
    if (a++ < --b)
        return 1;
    else
        if (a == b)
            return 2;
        else
            return 3;
}