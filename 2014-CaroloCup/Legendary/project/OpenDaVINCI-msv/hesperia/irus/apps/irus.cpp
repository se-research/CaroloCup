/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include "IRUS.h"

int32_t main(int32_t argc, char **argv) {
    irus::IRUS sensors(argc, argv);
    return sensors.runModule();
}
