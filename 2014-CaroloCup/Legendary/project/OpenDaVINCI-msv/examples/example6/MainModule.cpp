/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "Example6.h"

int32_t main(int32_t argc, char **argv) {
    mousetracker::MouseTracker mt(argc, argv);
    return mt.runModule();
}
