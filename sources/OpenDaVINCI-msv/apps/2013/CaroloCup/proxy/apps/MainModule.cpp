/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "Proxy.h"

int32_t main(int32_t argc, char **argv) {
    carolocup::Proxy p(argc, argv);
    return p.runModule();
}
