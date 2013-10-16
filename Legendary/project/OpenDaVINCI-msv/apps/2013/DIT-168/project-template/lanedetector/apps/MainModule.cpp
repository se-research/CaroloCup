/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "LaneDetector.h"

int32_t main(int32_t argc, char **argv) {
    msv::LaneDetector ld(argc, argv);
    return ld.runModule();
}
