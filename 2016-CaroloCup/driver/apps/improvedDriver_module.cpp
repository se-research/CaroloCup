#include "../src/Driver_victor.cpp"

int32_t main(int32_t argc, char **argv) {
    msv::Driver d(argc, argv);
    return d.runModule();
}

