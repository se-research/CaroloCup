/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "Monitor.h"

int32_t main(int32_t argc, char **argv) {
    monitor::Monitor m(argc, argv);
    return m.runModule();
}
