/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "ExtraDriver.h"

int32_t main(int32_t argc, char **argv) {
    carolocup::ExtraDriver d(argc, argv);
    return d.runModule();
}
