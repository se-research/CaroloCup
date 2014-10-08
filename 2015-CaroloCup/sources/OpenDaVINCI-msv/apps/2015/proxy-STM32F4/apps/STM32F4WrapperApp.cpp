/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "STM32F4Wrapper.h"

int32_t main(int32_t argc, char **argv) {
    msv::STM32F4Wrapper wrapper(argc, argv);
    return wrapper.runModule();
}
