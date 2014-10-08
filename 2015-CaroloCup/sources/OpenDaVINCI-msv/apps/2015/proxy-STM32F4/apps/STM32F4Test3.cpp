/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "STM32F4Tester3.h"

int32_t main(int32_t argc, char **argv) {
    msv::STM32F4Tester3 tester3(argc, argv);
    return tester3.runModule();
}
