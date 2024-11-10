int main(void) {
    int a = 1;
again:
    a *= 2;
    if (a < 60)
        goto again;
    a--;
    if (a > 200)
        goto end;
    goto again;
end:
    return a;
}