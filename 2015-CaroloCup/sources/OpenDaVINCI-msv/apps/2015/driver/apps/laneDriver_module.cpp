/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */
#include "../include/laneDriver.h"

int32_t main(int32_t argc, char **argv) {
    msv::laneDriver d(argc, argv);
    return d.runModule();
}