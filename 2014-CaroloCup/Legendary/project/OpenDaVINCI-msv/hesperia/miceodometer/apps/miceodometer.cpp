/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "MiceOdometer.h"

int32_t main(int32_t argc, char **argv) {
    miceodometer::MiceOdometer mo(argc, argv);
    return mo.runModule();
}
