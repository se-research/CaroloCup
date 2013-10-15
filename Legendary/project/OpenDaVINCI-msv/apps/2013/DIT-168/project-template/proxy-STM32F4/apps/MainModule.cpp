/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "ProxySTM32F4.h"

int32_t main(int32_t argc, char **argv) {
    msv::ProxySTM32F4 p(argc, argv);
    return p.runModule();
}
