/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "Sensors.h"

int32_t main(int32_t argc, char **argv) {
    carolocup::Sensors ss(argc, argv);
    return ss.runModule();
}
