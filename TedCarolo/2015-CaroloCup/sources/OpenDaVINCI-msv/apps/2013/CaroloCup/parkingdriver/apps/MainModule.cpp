/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "parkingDriver.h"

int32_t main(int32_t argc, char **argv) {
    carolocup::parkingDriver d(argc, argv);
    return d.runModule();
}
