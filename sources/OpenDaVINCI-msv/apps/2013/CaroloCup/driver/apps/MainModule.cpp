/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "Driver.h"

int32_t main(int32_t argc, char **argv) {
    carolocup::Driver d(argc, argv);
    return d.runModule();
}