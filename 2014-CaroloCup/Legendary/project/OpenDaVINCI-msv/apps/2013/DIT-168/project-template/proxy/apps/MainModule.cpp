/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "Proxy.h"

int32_t main(int32_t argc, char **argv) {
    msv::Proxy p(argc, argv);
    return p.runModule();
}
