int main(void) {
    double nan = 0.0 / 0.0;
    if (nan < 0.0 || nan == 0.0 || nan > 0.0 || nan <= 0.0 || nan >= 0.0) {
        return 1;
    }
    if (nan == nan) {
        return 2;
    }
    if (!(nan != nan)) { // != should evaluate to true
        return 3;
    }
    if (!nan) {
        return 4;
    }
    if (nan) {
    } else {
        return 5;
    }

    return 0;
}