/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */
#include "../include/obstacleDriver.h"

int32_t main(int32_t argc, char **argv) {
    msv::obstacleDriver d(argc, argv);
    return d.runModule();
}
