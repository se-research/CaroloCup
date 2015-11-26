/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <DriverManager.h>

int32_t main(int32_t argc, char **argv) {
    msv::DriverManager d(argc, argv);
    return d.runModule();
}
