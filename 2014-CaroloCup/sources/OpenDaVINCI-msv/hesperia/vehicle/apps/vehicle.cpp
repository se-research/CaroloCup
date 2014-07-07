/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include "Vehicle.h"

int32_t main(int32_t argc, char **argv) {
    vehicle::Vehicle v(argc, argv);
    return v.runModule();
}
