/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "Cockpit.h"

int32_t main(int32_t argc, char **argv) {
    cockpit::Cockpit cp(argc, argv);
    return cp.runModule();
}
