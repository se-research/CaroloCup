/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "Lidar.h"

int32_t main(int32_t argc, char **argv) {
    carolocup::Lidar sb(argc, argv);
    return sb.runModule();
}
