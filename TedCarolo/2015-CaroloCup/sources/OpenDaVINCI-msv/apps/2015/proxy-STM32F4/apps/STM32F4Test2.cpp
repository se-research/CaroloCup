/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "STM32F4Tester2.h"

int32_t main(int32_t argc, char **argv) {
    msv::STM32F4Tester2 tester2(argc, argv);
    return tester2.runModule();
}
